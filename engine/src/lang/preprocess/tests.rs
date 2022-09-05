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
    ParseError(Span),
}

impl GenericParseErrorHandler<char> for E {
    fn expected(
        span: Span,
        _expected: HashSet<Option<char>>,
        _found: Option<char>,
    ) -> Self {
        Self::ParseError(span)
    }

    fn unclosed_delimiter(
        span: Span,
        _: char,
        _expected: char,
        _found: Option<char>,
    ) -> Self {
        Self::ParseError(span)
    }
}

impl types::string_with_vars::ParseErrorHandler for E {
    fn bad_post_percent(span: Span) -> Self {
        Self::ParseError(span)
    }

    fn bad_identifier(span: Span) -> Self {
        Self::ParseError(span)
    }

    fn unclosed_var(span: Span) -> Self {
        Self::ParseError(span)
    }
}

impl parse::ErrorHandler for E {
    fn unclosed_comment(span: Span) -> Self {
        Self::ParseError(span)
    }

    fn unclosed_quotes(span: Span) -> Self {
        Self::ParseError(span)
    }

    fn bad_directive(span: Span) -> Self {
        Self::ParseError(span)
    }

    fn expected_end_of_line(_why: &'static str, span: Span) -> Self {
        Self::ParseError(span)
    }

    fn unclosed_if(_which: &'static str, span: Span) -> Self {
        Self::ParseError(span)
    }

    fn define_duplicate_arg(span: Span) -> Self {
        Self::ParseError(span)
    }

    fn define_unmatched_start_quote(span: Span) -> Self {
        Self::ParseError(span)
    }

    fn define_unmatched_end_quote(span: Span) -> Self {
        Self::ParseError(span)
    }

    fn unmatched_else(span: Span) -> Self {
        Self::ParseError(span)
    }

    fn unmatched_endif(span: Span) -> Self {
        Self::ParseError(span)
    }

    fn unmatched_paren(span: Span) -> Self {
        Self::ParseError(span)
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

    fn pool_unimplemented(span: Span) -> Self {
        Self::ParseError(span)
    }

    fn ambiguous_include_file(_candidates: Vec<PathBuf>, span: Span) -> Self {
        Self::ParseError(span)
    }

    fn ambiguous_external_program(
        _candidates: Vec<PathBuf>,
        span: Span,
    ) -> Self {
        Self::ParseError(span)
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

    fn io_error(_underlying: std::io::Error, span: Span) -> Self {
        Self::ParseError(span)
    }
}

impl types::string_with_vars::RenderErrorHandler for E {
    fn unknown_var(_v: String, span: Span) -> Self {
        Self::ParseError(span)
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
        _span: Span,
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

    fn push_line<'a>(
        &'a mut self,
        definitions: Definitions<'a>,
        line: Events,
        _original_line: &'a Vec<TokenGroup>,
    ) {
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

    fn register_symbol(&mut self, _symbol: String, _span: Span) {
        // nothing to do
    }

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

impl TestDriver {
    fn new(
        files_in_tree: HashMap<PathBuf, String>,
        executables: HashMap<PathBuf, Box<dyn Fn(Vec<String>) -> String>>,
    ) -> Self {
        Self {
            files_in_tree,
            executables,
            errors: Vec::new(),
            messages: Vec::new(),
            lines: Vec::new(),
        }
    }

    fn pp(&self) -> String {
        let mut result = String::new();

        result.push_str("ERRORS:\n");
        for e in self.errors.iter() {
            result.push_str(format!("  {:?}\n", e).as_str());
        }

        result.push_str("\n\nMESSAGES:\n");
        for m in self.messages.iter() {
            result.push_str(format!("  {}\n", m).as_str());
        }

        result.push_str("\n\nOUTPUT:\n");
        for line in self.lines.iter() {
            result.push_str(" ");
            for token in line {
                result.push_str(format!(" {}", token).as_str());
            }
            result.push_str("\n")
        }

        result
    }
}

fn run_test(
    input: &'static str,
    files_in_tree: HashMap<PathBuf, String>,
    executables: HashMap<PathBuf, Box<dyn Fn(Vec<String>) -> String>>,
) -> String {
    let driver = TestDriver::new(files_in_tree, executables);

    drive(driver, "TEST_INPUT", input.to_string()).pp()
}

#[test]
fn e2e_basic() {
    let input = r#"
    #define foo(b) "bar(b)"
    #define bar(a) "baz a A"

    foo(whee)

    #define A B

    foo(whoo)
    "#;

    let output = run_test(input, HashMap::new(), HashMap::new());

    insta::assert_display_snapshot!("e2e_basic", output)
}
