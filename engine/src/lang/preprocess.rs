// XXX: This architecture is _really_ complicated. Unfortunately, it's the only
// design I can think of that satisfies all of the following criteria:
//
// - The expression parser has enough info to produce contextual errors. e.g.,
//   [#define Eirika 1\nEirika:] should produce "[Eirika] is a macro and so
//   can't be used as a label name" instead of "identifier expected".
// - The preprocessor (macro expander), expression parser and raws engine live
//   in separate modules.
// - [#inctext] has the correct semantics.

pub mod parse;

#[cfg(test)]
mod tests;

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

// TODO: adjust these to include [VerboseContext] or something where necessary
pub trait ErrorHandler: 'static {
    fn defined_with_args_but_no_body(span: Span) -> Self;

    fn cant_redefine_builtin(span: Span) -> Self;

    fn already_defined(new_defn_loc: Span, old_defn_loc: Span) -> Self;

    fn undef_not_defined(ident: &String, span: Span) -> Self;

    fn pool_unimplemented(span: Span) -> Self;

    fn ambiguous_include_file(candidates: Vec<PathBuf>, span: Span) -> Self;

    fn ambiguous_external_program(candidates: Vec<PathBuf>, span: Span)
        -> Self;

    fn expanded_empty_definition(definition_site: Span, use_site: Span)
        -> Self;

    fn builtin_in_template(symbol: &String, span: Span) -> Self;

    fn macro_in_template(symbol: &String, span: Span) -> Self;

    fn inctext_error(underlying: Self, span: Span) -> Self;

    fn recursive_macro(
        macro_name: &String,
        definition_site: Span,
        use_site: Span,
    ) -> Self;

    fn wrong_number_of_arguments(
        macro_name: &String,
        expected: usize,
        actual: usize,
        definition_site: Span,
        span: Span,
    ) -> Self;

    fn macro_needs_arguments(
        macro_name: &String,
        definition_site: Option<Span>,
        span: Span,
    ) -> Self;

    fn io_error(underlying: std::io::Error, span: Span) -> Self;
}

// XXX: it'd be nice if we could somehow make this vector static
fn builtins() -> HashMap<String, Definition> {
    vec![
        ("__LINE__", Definition::Reserved),
        ("__FILE__", Definition::Reserved),
        ("currentOffset", Definition::Reserved),
    ]
    .into_iter()
    .map(|(s, d)| (s.to_string(), d))
    .collect()
}

pub fn drive<E, D>(driver: D, file: impl AsRef<Path>, contents: String) -> D
where
    E: ErrorHandler + parse::ErrorHandler,
    D: Driver<E>,
{
    let mut context: Context<_, E> = Context {
        driver,
        defines: builtins(),
        phantom: PhantomData,
    };

    context.process(
        &Source::File(Rc::new(file.as_ref().to_path_buf())),
        contents,
        id,
    );

    context.driver
}

pub struct Definitions<'a>(&'a HashMap<String, Definition>);

enum Definition {
    // The separation between [Builtin] and [Reserved] here is for ease of
    // parsing. [Builtin]s take arguments, [Reserved]s don't.
    Builtin(Box<dyn Fn(Vec<Vec<TokenGroup>>) -> Vec<TokenGroup>>),
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
/// for organization's sake, we keep the directive processor (e.g., this file)
/// separate from the raws engine and other transport concerns.
pub trait Driver<E: ErrorHandler> {
    /// Push a `MESSAGE` node to the driver. The type of `lookup_symbol` is
    /// necessary to ensure that warnings are raised when expanding empty
    /// symbols.
    fn push_message(
        &mut self,
        msg: &StringWithVars,
        lookup_symbol: impl Fn(&String) -> (Option<String>, Option<E>),
        span: Span,
    );

    /// Push a line to the driver, along with all currently known definitions.
    /// It is the driver's responsibility to call `expand_events_until_finished`
    /// from this module to flatten the line into its constituent tokens. This
    /// is to ensure that the driver doesn't lose context on the original user
    /// input while parsing, which will help error granularity.
    fn push_line<'a>(&'a mut self, definitions: Definitions<'a>, line: Events);

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
        // this is mutable so that any errors from rendering [args] can be
        // reported properly
        &mut self,
        exe: &RelativePath,
        args: &Vec<StringWithVars>,
        lookup_symbol: impl Fn(&String) -> (Option<String>, Option<E>) + Copy,
        current_dir: Option<impl AsRef<Path>>,
    ) -> Result<(String, Source), IOError>
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
    fn process(
        &mut self,
        source: &Source,
        contents: impl AsRef<str>,
        wrap_error: impl Fn(E) -> E,
    ) {
        match parse::parse(source, contents) {
            Ok(tree) => self.walk(&tree),
            Err(errs) => {
                for err in errs.into_iter() {
                    self.driver.push_error(wrap_error(err))
                }
            }
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
            Node::Directive(d) => self.dispatch_directive(d, span),
            Node::Message(swv) => self.driver.push_message(
                swv,
                |s| lookup_symbol_for_rendering(&self.defines, s, span.clone()),
                span.clone(),
            ),
            Node::Line(line) => match parse_events(&self.defines, &line) {
                Err(errs) => {
                    for err in errs {
                        self.driver.push_error(err);
                    }
                }
                Ok(events) => self
                    .driver
                    .push_line(Definitions(&self.defines), Events(events)),
            },
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
                        self.process(&Source::File(Rc::new(file)), contents, id)
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

                let s: Result<_, InctextErrorConverter<E>> =
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
                    Ok((contents, source)) => {
                        // TODO: This error reporting is not good. We have no
                        // way of retrieving any information that would be
                        // useful for actually debugging the problem (e.g.,
                        // the actual command line used to generate the code
                        // or a location in the cache containing the file).
                        self.process(&source, contents, move |e| {
                            E::inctext_error(e, span.clone())
                        })
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

enum Event {
    Token(Token),
    ExpandTo {
        from: Option<(Rc<String>, Span)>,
        body: Vec<TokenGroup>,
    },
}

pub struct Events(Vec<Spanned<Event>>);

pub fn expand_events_until_finished<E>(
    Definitions(definitions): Definitions<'_>,
    Events(events): Events,
) -> Result<Vec<Spanned<Token>>, Vec<E>>
where
    E: ErrorHandler,
{
    expand_events(definitions, events, LinkedList::empty())
}

fn expand_events<E>(
    definitions: &HashMap<String, Definition>,
    events: Vec<Spanned<Event>>,
    seen: LinkedList<String>,
) -> Result<Vec<Spanned<Token>>, Vec<E>>
where
    E: ErrorHandler,
{
    let mut result = Vec::new();
    let mut errs = Vec::new();

    for spanned_event in events {
        match expand_single_event(definitions, spanned_event, seen.clone()) {
            Ok(toks) => result.extend(toks),
            Err(es) => errs.extend(es),
        }
    }

    if errs.is_empty() {
        Ok(result)
    } else {
        Err(errs)
    }
}

fn expand_single_event<E>(
    definitions: &HashMap<String, Definition>,
    (event, span): Spanned<Event>,
    // Using a [LinkedList] here means that macro expansion is now quadratic
    // in the number of nested macro invocations, but it should be fine; it's
    // unlikely that we'll ever have to deal with a chain more than two digits
    // long.
    seen: LinkedList<String>,
) -> Result<Vec<Spanned<Token>>, Vec<E>>
where
    E: ErrorHandler,
{
    let span = &span;

    match event {
        Event::Token(t) => Ok(vec![(t, span.clone())]),
        Event::ExpandTo { from, body } => parse_events(
            definitions,
            &body
                .into_iter()
                .map(move |group| (group, span.clone()))
                .collect(),
        )
        .and_then(|evts| match from {
            Some((name, defn_site)) => {
                if seen.iter().any(|s| *s == *name) {
                    Err(vec![E::recursive_macro(
                        name.as_ref(),
                        defn_site.clone(),
                        span.clone(),
                    )])
                } else {
                    expand_events(
                        definitions,
                        evts,
                        seen.cons(name.as_ref().to_owned()),
                    )
                }
            }
            None => expand_events(definitions, evts, seen),
        }),
    }
}

fn parse_events<E>(
    definitions: &HashMap<String, Definition>,
    tokens: &Vec<Spanned<TokenGroup>>,
) -> Result<Vec<Spanned<Event>>, Vec<E>>
where
    E: ErrorHandler,
{
    let mut result = Vec::new();
    let mut errs = Vec::new();

    parse_events_impl(definitions, tokens, &mut result, &mut errs);

    if errs.is_empty() {
        Ok(result)
    } else {
        Err(errs)
    }
}

fn parse_events_impl<E>(
    definitions: &HashMap<String, Definition>,
    tokens: &Vec<Spanned<TokenGroup>>,
    result: &mut Vec<Spanned<Event>>,
    errs: &mut Vec<E>,
) where
    E: ErrorHandler,
{
    let mut tokens = tokens.iter().peekable();

    loop {
        let (t, span) = match tokens.next() {
            Some(elem) => elem,
            None => break,
        };

        match t {
            TokenGroup::Group { kind, members } => {
                let (start, end) = kind.delimiters();
                result.push((Event::Token(start), span.start_span()));
                parse_events_impl(definitions, members, result, errs);
                result.push((Event::Token(end), span.end_span()));
            }
            TokenGroup::Single((t @ Token::Ident(s), span)) => {
                match definitions.get(&**s) {
                    None | Some(Definition::Reserved) => {
                        result.push((Event::Token(t.clone()), span.clone()))
                    }
                    Some(Definition::Empty(defn_span)) => {
                        errs.push(E::expanded_empty_definition(
                            defn_span.clone(),
                            span.clone(),
                        ))
                    }
                    Some(Definition::Rename(out, defn_site)) => result.push((
                        (Event::ExpandTo {
                            from: Some((s.clone(), defn_site.clone())),
                            body: out.iter().map(|x| x.clone()).collect(),
                        }),
                        span.clone(),
                    )),
                    Some(Definition::Builtin(f)) => {
                        match fetch_args(&mut tokens, &**s, None, span) {
                            Ok((args, arg_span)) => result.push((
                                Event::ExpandTo {
                                    from: None,
                                    body: f(args),
                                },
                                span.join(&arg_span),
                            )),
                            Err(e) => errs.push(e),
                        }
                    }
                    Some(Definition::Macro(arg_names, body, defn_site)) => {
                        match fetch_args(
                            &mut tokens,
                            &**s,
                            Some(defn_site.clone()),
                            span,
                        ) {
                            Err(e) => errs.push(e),
                            Ok((args, arg_span)) => {
                                let expected = arg_names.len();
                                let received = args.len();

                                if expected != received {
                                    errs.push(E::wrong_number_of_arguments(
                                        &**s,
                                        expected,
                                        received,
                                        defn_site.clone(),
                                        span.join(&arg_span),
                                    ));

                                    continue;
                                }

                                let args: HashMap<_, _> =
                                    arg_names.iter().zip(args.iter()).collect();

                                let expanded =
                                    expand_body_single(&args, body, &span);

                                result.push((
                                    Event::ExpandTo {
                                        from: Some((
                                            s.clone(),
                                            defn_site.clone(),
                                        )),
                                        body: expanded,
                                    },
                                    span.join(&arg_span),
                                ))
                            }
                        }
                    }
                }
            }
            TokenGroup::Single((t, span)) => {
                result.push((Event::Token(t.clone()), span.clone()))
            }
        }
    }
}

fn expand_body_single(
    args: &HashMap<&String, &Vec<TokenGroup>>,
    body: &Vec<TokenGroup>,
    span: &Span,
) -> Vec<TokenGroup> {
    body.iter()
        .flat_map(|group| match group {
            TokenGroup::Single((Token::Ident(s), span)) => match args.get(&**s)
            {
                None => {
                    vec![TokenGroup::Single((
                        Token::Ident(s.clone()),
                        span.clone(),
                    ))]
                }
                Some(ts) => ts.iter().map(|x| x.clone()).collect(),
            },
            t @ TokenGroup::Single(_) => {
                vec![t.clone()]
            }
            TokenGroup::Group { kind, members } => vec![TokenGroup::Group {
                kind: *kind,
                members: expand_body_single(
                    args,
                    &members.iter().map(|(x, _)| x.clone()).collect(),
                    span,
                )
                .iter()
                .map(|x| (x.clone(), span.clone()))
                .collect(),
            }],
        })
        .collect()
}

fn fetch_args<E>(
    tokens: &mut std::iter::Peekable<std::slice::Iter<Spanned<TokenGroup>>>,
    s: &String,
    defn_span: Option<Span>,
    span: &Span,
) -> Result<(Vec<Vec<TokenGroup>>, Span), E>
where
    E: ErrorHandler,
{
    let (arg_tokens, arg_span) = match tokens.peek() {
        Some((
            TokenGroup::Group {
                kind: GroupKind::Paren,
                members,
            },
            arg_span,
        )) => (members, arg_span),
        Some((_, next_span)) => {
            return Err(E::macro_needs_arguments(
                s,
                defn_span,
                span.join(next_span),
            ));
        }
        None => {
            return Err(E::macro_needs_arguments(s, defn_span, span.clone()));
        }
    };

    let _ = tokens.next();

    let mut args = Vec::new();
    let mut current_arg = Vec::new();

    for (member, _) in arg_tokens {
        match member {
            TokenGroup::Single((Token::Comma, _)) => {
                args.push(current_arg);
                current_arg = Vec::new();
            }
            member => current_arg.push(member.clone()),
        }
    }

    if !current_arg.is_empty() {
        args.push(current_arg);
    }

    Ok((args, arg_span.clone()))
}

fn lookup_symbol_for_rendering<E>(
    defines: &HashMap<String, Definition>,
    symbol: &String,
    span: Span,
) -> (Option<String>, Option<E>)
where
    E: ErrorHandler,
{
    // XXX: this is hacky
    match symbol.as_str() {
        "__LINE__" => return (Some(format!("{}", span.start().row)), None),
        "__FILE__" => return (Some(format!("{}", span.source())), None),
        _ => (),
    };

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
