pub mod parse;

use std::{
    collections::HashMap,
    marker::PhantomData,
    path::{Path, PathBuf},
    rc::Rc,
};

use indexmap::IndexSet;
use relative_path::RelativePath;

use crate::{
    io::ErrorHandler as IOErrorHandler,
    plumbing::*,
    types::{LinkedList, StringWithVars},
};

use super::syntax::{
    span::Source, Directive, GroupKind, MacroBody, Node, Span, Spanned, Token,
    TokenGroup, Tree,
};

pub trait ErrorHandler: 'static {
    fn defined_with_args_but_no_body(span: Span) -> Self;

    fn cant_redefine_builtin(span: Span) -> Self;

    fn already_defined(new_defn_loc: Span, old_defn_loc: Span) -> Self;

    fn undef_not_defined(ident: &String, span: Span) -> Self;

    fn pool_unimplemented(span: Span) -> Self;

    fn ambiguous_include_file(candidates: Vec<PathBuf>, span: Span) -> Self;

    fn ambiguous_external_program(candidates: Vec<PathBuf>, span: Span)
        -> Self;

    fn expanded_empty_definition(defn_site: Span, use_site: Span) -> Self;

    fn builtin_in_template(symbol: &String, span: Span) -> Self;

    fn macro_in_template(symbol: &String, span: Span) -> Self;

    fn inctext_error(underlying: Self, span: Span) -> Self;

    fn io_error(underlying: std::io::Error, span: Span) -> Self;
}

enum Definition {
    // The separation between [Builtin] and [Reserved] here is for ease of
    // parsing. [Builtin]s take arguments, [Reserved]s don't.
    Builtin(Box<dyn Fn(TokenGroup) -> TokenGroup>),
    Reserved,
    Empty(Span),
    Rename(Vec<TokenGroup>, Span),
    Macro(IndexSet<String>, Vec<TokenGroup>, Span),
}

/// The main EA language processor to feed preprocessed output to.
///
/// tl;dr: the `#inctext` semantics are the root of all evil
///
/// Currently, we allow `#inctext` to run a program using `currentOffset` and
/// other labels, etc, that are only known at runtime. This means that we have
/// no choice but to fuse the preprocessing and actual assembly pass. However,
/// for organization's sake, we should keep the directive processor (e.g., this
/// file) separate from the raws engine etc.
pub trait Driver<E: ErrorHandler> {
    /// Push a `MESSAGE` node to the driver.
    fn push_message(
        &mut self,
        msg: &StringWithVars,
        lookup_symbol: impl Fn(&String) -> (Option<String>, Option<E>),
        span: Span,
    );

    /// Push a regular line of assembly to the driver.
    fn push_line(&mut self, line: Vec<VerboseSpanned<Token>>);

    /// Push a preprocessing error to the driver.
    fn push_error(&mut self, err: E);

    /// Registers a symbol with the driver. This is important for error
    /// reporting in cases where a label is defined with the same name as a
    /// `#define` macro. Note that the definition itself is irrelevant to the
    /// driver, just its location. We also don't care if the driver has this
    /// symbol registered already; we have to track it internally anyway so
    /// our `#ifdef`s continue to work intuitively.
    // XXX: This design redundantly stores symbol information twice (in the
    // driver and in the preprocessor). It might be a better idea to track
    // everything in the preprocessor and have [push_line] return a list of
    // new definitions.
    fn register_symbol(&mut self, symbol: String, span: Span);

    /// Push an `#incbin` directive. The success or failure of finding such a
    /// file is irrelevant to the preprocessor (the driver can register any
    /// encountered errors for itself), so this method returns a raw `unit`
    /// rather than a result.
    fn push_binary_file(
        &mut self,
        path: &RelativePath,
        current_dir: Option<impl AsRef<Path>>,
        span: &Span,
    );

    /// Ask the driver to locate a file for the purpose of `#include`, then
    /// return its contents if possible.
    ///
    /// Unlike `push_binary_file`, the preprocessor needs to be able to read
    /// the output of, so errors need to be caught and registered by the
    /// preprocessor itself.
    fn request_file<IOError>(
        &self,
        path: &RelativePath,
        current_dir: Option<impl AsRef<Path>>,
    ) -> Result<(PathBuf, String), IOError>
    where
        IOError: IOErrorHandler;

    /// Run a process in `current_dir` and capture its binary output. See
    /// `push_binary_file`.
    fn push_binary_process_run(
        &mut self,
        exe: &RelativePath,
        args: &Vec<StringWithVars>,
        lookup_symbol: impl Fn(&String) -> (Option<String>, Option<E>),
        current_dir: Option<impl AsRef<Path>>,
        span: &Span,
    );

    /// Run a process in `current_dir` and capture its output for event
    /// processing. See `request_file`.
    fn request_process_run<IOError>(
        &self,
        exe: &RelativePath,
        args: &Vec<StringWithVars>,
        lookup_symbol: impl Fn(&String) -> (Option<String>, Option<E>),
        current_dir: Option<impl AsRef<Path>>,
    ) -> Result<String, IOError>
    where
        IOError: IOErrorHandler;
}

struct Context<D, E> {
    driver: D,
    defines: HashMap<String, Definition>,
    phantom: PhantomData<E>,
}

impl<D, E> Context<D, E>
where
    E: ErrorHandler + parse::ErrorHandler,
    D: Driver<E>,
{
    fn new(driver: D) -> Self {
        Self {
            driver,
            defines: HashMap::new(),
            phantom: PhantomData,
        }
    }

    fn register_symbol(&mut self, ident: String, body: Definition, span: Span) {
        match self.defines.insert(ident.clone(), body) {
            None => (),
            Some(Definition::Empty(old_span))
            | Some(Definition::Rename(_, old_span))
            | Some(Definition::Macro(_, _, old_span)) => self
                .driver
                .push_error(E::already_defined(span.clone(), old_span)),
            Some(Definition::Builtin(_)) | Some(Definition::Reserved) => self
                .driver
                .push_error(E::cant_redefine_builtin(span.clone())),
        };

        self.driver.register_symbol(ident, span);
    }

    fn walk(&mut self, Tree(nodes): &Tree) {
        for (node, span) in nodes.iter() {
            self.handle_node(node, span)
        }
    }

    fn handle_node(&mut self, node: &Node, span: &Span) {
        match node {
            Node::Line(line) => panic!("todo"),
            Node::Directive(d) => self.dispatch_directive(d, span),
            Node::Message(swv) => self.driver.push_message(
                swv,
                |s| lookup_symbol_for_rendering(&self.defines, s, span.clone()),
                span.clone(),
            ),
        }
    }

    fn walk_if(
        &mut self,
        f: impl Fn(bool) -> bool,
        ident: &String,
        then: &Tree,
        else_: &Tree,
    ) {
        if f(self.defines.contains_key(ident)) {
            self.walk(then)
        } else {
            self.walk(else_)
        }
    }

    fn dispatch_directive(&mut self, d: &Directive, span: &Span) {
        use Directive::*;
        match d {
            IfDef(ident, then, else_) => self.walk_if(id, ident, then, else_),
            IfNDef(ident, then, else_) => {
                self.walk_if(|b| !b, ident, then, else_)
            }
            Define(ident, args, body) => {
                let body = match (args, body) {
                    (None, MacroBody::Macro(body)) => Definition::Rename(
                        body.iter().map(|(group, _)| group.clone()).collect(),
                        span.clone(),
                    ),
                    (Some(args), MacroBody::Macro(body)) => Definition::Macro(
                        args.clone(),
                        body.iter().map(|(group, _)| group.clone()).collect(),
                        span.clone(),
                    ),
                    (None, MacroBody::Empty) => Definition::Empty(span.clone()),
                    (Some(args), MacroBody::Empty) => {
                        self.driver.push_error(
                            E::defined_with_args_but_no_body(span.clone()),
                        );
                        Definition::Macro(args.clone(), vec![], span.clone())
                    }
                };

                self.register_symbol(ident.clone(), body, span.clone())
            }
            Include(path) => {
                let current_dir = extract_current_dir(span);

                let s: Result<(PathBuf, String), IncludeErrorConverter<E>> =
                    self.driver.request_file(path, current_dir);

                match s {
                    Err(IncludeErrorConverter(f)) => {
                        self.driver.push_error(f(span.clone()))
                    }
                    Ok((file, contents)) => {
                        match parse::parse(
                            &Source::File(Rc::new(file)),
                            contents,
                        ) {
                            Ok(tree) => self.walk(&tree),
                            Err(errs) => {
                                for err in errs.into_iter() {
                                    self.driver.push_error(err)
                                }
                            }
                        }
                    }
                }
            }
            Incbin(path) => self.driver.push_binary_file(
                path,
                extract_current_dir(span),
                span,
            ),
            Incext(exe, args) => self.driver.push_binary_process_run(
                exe,
                &args,
                |s| lookup_symbol_for_rendering(&self.defines, s, span.clone()),
                extract_current_dir(span),
                span,
            ),
            Inctevent(exe, args) => {
                let current_dir = extract_current_dir(span);

                let s: Result<String, InctextErrorConverter<E>> =
                    self.driver.request_process_run(
                        exe,
                        args,
                        |s| {
                            lookup_symbol_for_rendering(
                                &self.defines,
                                s,
                                span.clone(),
                            )
                        },
                        current_dir,
                    );

                match s {
                    Err(InctextErrorConverter(f)) => {
                        self.driver.push_error(f(span.clone()))
                    }
                    Ok(contents) => {
                        // TODO: This error reporting is not good. We have no
                        // way of retrieving any information that would be
                        // useful for actually debugging the problem (e.g.,
                        // the actual command line used to generate the code
                        // or a location in the cache containing the file).
                        //
                        // One way to fix this would be to have [driver]
                        // provide some associated [Details] type. Then, we could
                        // mark the span with driver-specific excess details,
                        // e.g. (the file location in the cache, the precise
                        // command line used to generate this file, etc).
                        match parse::parse(&Source::Unknown, contents) {
                            Ok(tree) => self.walk(&tree),
                            Err(errs) => {
                                for err in errs.into_iter() {
                                    self.driver.push_error(E::inctext_error(
                                        err,
                                        span.clone(),
                                    ))
                                }
                            }
                        }
                    }
                }
            }
            Pool => {
                self.driver.push_error(E::pool_unimplemented(span.clone()));
            }
            Undef(ident) => {
                match self.defines.remove(ident) {
                    Some(_) => (),
                    None => self
                        .driver
                        .push_error(E::undef_not_defined(ident, span.clone())),
                };
            }
        }
    }
}

fn lookup_symbol_for_rendering<E>(
    defines: &HashMap<String, Definition>,
    symbol: &String,
    span: Span,
) -> (Option<String>, Option<E>)
where
    E: ErrorHandler,
{
    match defines.get(symbol) {
        None => (None, None),
        Some(Definition::Empty(defn_site)) => (
            Some("".to_owned()),
            Some(E::expanded_empty_definition(defn_site.clone(), span)),
        ),
        Some(Definition::Rename(body, _)) => {
            let mut result = String::new();

            for (idx, group) in body.iter().enumerate() {
                if idx > 0 {
                    result.push_str(" ")
                }
                result.push_str(format!("{}", group).as_str())
            }

            (Some(result), None)
        }
        Some(Definition::Builtin(_)) => (
            Some(format!("{{{}}}", symbol)),
            Some(E::builtin_in_template(symbol, span)),
        ),
        Some(Definition::Reserved) => (None, None),
        Some(Definition::Macro(_, _, _)) => (
            Some(format!("{{{}}}", symbol)),
            Some(E::macro_in_template(symbol, span)),
        ),
    }
}

fn extract_current_dir(span: &Span) -> Option<&Path> {
    match &span.source {
        Source::Unknown => None,
        Source::File(path) => path.parent(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpanWithReason {
    why: Rc<String>,
    span: Span,
}

/// Tracks spans between macro invocations.
// XXX: Should we also track between [#include]s?
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerboseSpan {
    /// The span representing the original user text corresponding to this
    /// span.
    user_span: Span,
    /// A stack of spans corresponding to the macros expanded while processing
    /// this span.
    expansions: LinkedList<SpanWithReason>,
}

impl From<Span> for VerboseSpan {
    fn from(user_span: Span) -> Self {
        Self {
            user_span,
            expansions: LinkedList::empty(),
        }
    }
}

pub type VerboseSpanned<T> = (T, VerboseSpan);

pub fn mark_verbose<T>((t, span): Spanned<T>) -> VerboseSpanned<T> {
    (t, VerboseSpan::from(span))
}

struct IncludeErrorConverter<E>(Box<dyn FnOnce(Span) -> E>);

impl<E> IOErrorHandler for IncludeErrorConverter<E>
where
    E: ErrorHandler,
{
    fn os_error(underlying: std::io::Error) -> Self {
        IncludeErrorConverter(Box::new(move |span| {
            E::io_error(underlying, span)
        }))
    }

    fn multiple_candidates(found: Vec<PathBuf>) -> Self {
        IncludeErrorConverter(Box::new(move |span| {
            E::ambiguous_include_file(found, span)
        }))
    }
}

struct InctextErrorConverter<E>(Box<dyn FnOnce(Span) -> E>);

impl<E> IOErrorHandler for InctextErrorConverter<E>
where
    E: ErrorHandler,
{
    fn os_error(underlying: std::io::Error) -> Self {
        InctextErrorConverter(Box::new(move |span| {
            E::io_error(underlying, span)
        }))
    }

    fn multiple_candidates(found: Vec<PathBuf>) -> Self {
        InctextErrorConverter(Box::new(move |span| {
            E::ambiguous_external_program(found, span)
        }))
    }
}

struct IteratorStack<T>(Vec<Box<dyn Iterator<Item = T>>>);

impl<T> IteratorStack<T> {
    fn new<I>(iter: I) -> Self
    where
        I: Iterator<Item = T> + 'static,
    {
        Self(vec![Box::new(iter)])
    }

    fn push_iter<I>(&mut self, iter: I)
    where
        I: Iterator<Item = T> + 'static,
    {
        let Self(inner) = self;

        inner.push(Box::new(iter));
    }
}

impl<T> Iterator for IteratorStack<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let Self(inner) = self;

        let top = inner.last_mut()?;

        match top.next() {
            Some(t) => return Some(t),
            None => (),
        };

        inner.pop();

        self.next()
    }
}
