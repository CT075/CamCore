use std::collections::{HashMap, HashSet};

use super::*;
use crate::{lang::parse::common::GenericParseErrorHandler, types};

#[derive(Debug, PartialEq, Eq, Clone)]
enum E {
    DefinedWithArgsButNoBody(Span),
    CantRedefineBuiltin(Span),
    AlreadyDefined(Span, Span),
    UndefNotDefined(String, Span),
    ExpandedEmptyDefinition(Span, Span),
    BuiltinInTemplate(String, Span),
    MacroInTemplate(String, Span),
    InctextError(Box<Self>, Span),
    RecursiveMacro(String, Span, Span),
    WrongNumberOfArguments(String, usize, usize, Span, Span),
    MacroNeedsArguments(String, Option<Span>, Span),
}

impl GenericParseErrorHandler<char> for E {
    fn expected(
        _span: Span,
        _expected: HashSet<Option<char>>,
        _found: Option<char>,
    ) -> Self {
        panic!("parse error")
    }

    fn unclosed_delimiter(
        _span: Span,
        _: char,
        _expected: char,
        _found: Option<char>,
    ) -> Self {
        panic!("parse error")
    }
}

impl types::string_with_vars::ParseErrorHandler for E {
    fn bad_post_percent(span: Span) -> Self {
        panic!("swv parse error")
    }

    fn bad_identifier(span: Span) -> Self {
        panic!("swv parse error")
    }

    fn unclosed_var(span: Span) -> Self {
        panic!("swv parse error")
    }
}

impl parse::ErrorHandler for E {
    fn unclosed_comment(span: Span) -> Self {
        panic!("parse error")
    }

    fn unclosed_quotes(span: Span) -> Self {
        panic!("parse error")
    }

    fn bad_directive(span: Span) -> Self {
        panic!("parse error")
    }

    fn expected_end_of_line(why: &'static str, span: Span) -> Self {
        panic!("parse error")
    }

    fn unclosed_if(which: &'static str, span: Span) -> Self {
        panic!("parse error")
    }

    fn define_duplicate_arg(span: Span) -> Self {
        panic!("parse error")
    }

    fn define_unmatched_start_quote(span: Span) -> Self {
        panic!("parse error")
    }

    fn define_unmatched_end_quote(span: Span) -> Self {
        panic!("parse error")
    }

    fn unmatched_else(span: Span) -> Self {
        panic!("parse error")
    }

    fn unmatched_endif(span: Span) -> Self {
        panic!("parse error")
    }

    fn unmatched_paren(span: Span) -> Self {
        panic!("parse error")
    }
}

impl ErrorHandler for E {
    fn defined_with_args_but_no_body(span: Span) -> Self {
        Self::DefinedWithArgsButNoBody(span)
    }

    fn cant_redefine_builtin(span: Span) -> Self {
        Self::CantRedefineBuiltin(span)
    }

    fn already_defined(new_defn_loc: Span, old_defn_loc: Span) -> Self {
        Self::AlreadyDefined(new_defn_loc, old_defn_loc)
    }

    fn undef_not_defined(ident: &String, span: Span) -> Self {
        Self::UndefNotDefined(ident.clone(), span)
    }

    fn pool_unimplemented(_span: Span) -> Self {
        panic!("pool unimplemented")
    }

    fn ambiguous_include_file(_candidates: Vec<PathBuf>, _span: Span) -> Self {
        panic!("ambiguous include file")
    }

    fn ambiguous_external_program(
        _candidates: Vec<PathBuf>,
        _span: Span,
    ) -> Self {
        panic!("ambiguous external program")
    }

    fn expanded_empty_definition(
        definition_site: Span,
        use_site: Span,
    ) -> Self {
        Self::ExpandedEmptyDefinition(definition_site, use_site)
    }

    fn builtin_in_template(symbol: &String, span: Span) -> Self {
        Self::BuiltinInTemplate(symbol.clone(), span)
    }

    fn macro_in_template(symbol: &String, span: Span) -> Self {
        Self::MacroInTemplate(symbol.clone(), span)
    }

    fn inctext_error(underlying: Self, span: Span) -> Self {
        Self::InctextError(Box::new(underlying), span)
    }

    fn recursive_macro(
        macro_name: &String,
        definition_site: Span,
        use_site: Span,
    ) -> Self {
        Self::RecursiveMacro(macro_name.clone(), definition_site, use_site)
    }

    fn wrong_number_of_arguments(
        macro_name: &String,
        expected: usize,
        actual: usize,
        definition_site: Span,
        span: Span,
    ) -> Self {
        Self::WrongNumberOfArguments(
            macro_name.clone(),
            expected,
            actual,
            definition_site,
            span,
        )
    }

    fn macro_needs_arguments(
        macro_name: &String,
        definition_site: Option<Span>,
        span: Span,
    ) -> Self {
        Self::MacroNeedsArguments(macro_name.clone(), definition_site, span)
    }

    fn io_error(_underlying: std::io::Error, _span: Span) -> Self {
        panic!("io error")
    }
}

impl types::string_with_vars::RenderErrorHandler for E {
    fn unknown_var(v: String, span: Span) -> Self {
        panic!("string_with_vars::render")
    }
}

struct TestDriver {
    files_in_tree: HashMap<PathBuf, String>,
    executables: HashMap<PathBuf, Box<dyn Fn(Vec<String>) -> String>>,
    messages: Vec<String>,
    lines: Vec<Vec<Token>>,
    errors: Vec<E>,
}

// XXX: A lot of this functionality is duplicated with the actual [Driver]
// type. We should provide hooks to augment that type instead.
impl Driver<E> for TestDriver {
    fn push_message(
        &mut self,
        msg: &StringWithVars,
        lookup_symbol: impl Fn(&String) -> (Option<String>, Option<E>),
        span: Span,
    ) {
        let rendered: Result<String, Vec<E>> = msg.render(lookup_symbol);

        match rendered {
            Ok(msg) => self.messages.push(msg),
            Err(es) => {
                for e in es.iter() {
                    self.errors.push(e.clone())
                }
            }
        }
    }

    fn push_line<'a>(&'a mut self, definitions: Definitions<'a>, line: Events) {
        let expanded: Result<_, Vec<E>> =
            expand_events_until_finished(definitions, line);

        match expanded {
            Ok(toks) => {
                self.lines.push(toks.into_iter().map(|(t, _)| t).collect())
            }
            Err(es) => {
                for e in es.iter() {
                    self.errors.push(e.clone())
                }
            }
        }
    }

    fn push_error(&mut self, err: E) {
        self.errors.push(err)
    }

    fn register_symbol(&mut self, symbol: String, span: Span) {}

    fn push_binary_file(
        &mut self,
        _path: &RelativePath,
        _current_dir: Option<impl AsRef<Path>>,
        _span: &Span,
    ) {
        // nothing to do
    }

    fn request_file<IOError>(
        &self,
        path: &RelativePath,
        _current_dir: Option<impl AsRef<Path>>,
    ) -> Result<(PathBuf, String), IOError>
    where
        IOError: IOErrorHandler,
    {
        let root = PathBuf::from("testdir");

        let path = path.to_path(&root);

        match self.files_in_tree.get(&path) {
            None => Err(IOError::os_error(std::io::Error::from(
                std::io::ErrorKind::NotFound,
            ))),
            Some(contents) => Ok((path, contents.clone())),
        }
    }

    fn push_binary_process_run(
        &mut self,
        _exe: &RelativePath,
        _args: &Vec<StringWithVars>,
        _lookup_symbol: impl Fn(&String) -> (Option<String>, Option<E>),
        _current_dir: Option<impl AsRef<Path>>,
        _span: &Span,
    ) {
        // nothing to do
    }

    fn request_process_run<IOError>(
        &mut self,
        exe: &RelativePath,
        args: &Vec<StringWithVars>,
        lookup_symbol: impl Fn(&String) -> (Option<String>, Option<E>) + Copy,
        _current_dir: Option<impl AsRef<Path>>,
    ) -> Result<(String, Source), IOError>
    where
        IOError: IOErrorHandler,
    {
        let mut args_rendered = Vec::new();

        for swv in args.iter() {
            match swv.render(lookup_symbol) {
                Ok(arg) => args_rendered.push(arg),
                Err(es) => {
                    for e in es.iter() {
                        self.errors.push(e.clone())
                    }
                }
            }
        }

        let root = PathBuf::from("testdir");

        let exe = exe.to_path(&root);

        match self.executables.get(&exe) {
            None => Err(IOError::os_error(std::io::Error::from(
                std::io::ErrorKind::NotFound,
            ))),
            Some(f) => Ok((f(args_rendered), Source::Unknown)),
        }
    }
}
