use thiserror::Error;

use super::{lex::LexError, token};

pub mod syntax;

mod parse;
mod stream;

pub use parse::ast;

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum PreprocError {
    #[error("file ended unexpectedly (hint: {hint:?})")]
    UnexpectedEof { hint: String },
    #[error("arguments to macros should be surrounded by parentheses")]
    ExpectLParen,
    #[error("#include should be followed by a filepath")]
    ExpectIncludePath,
    #[error("#incbin should be followed by a filepath")]
    ExpectIncbinPath,
    #[error("#ifdef and #ifndef should be followed by an identifier")]
    ExpectIfdefName,
    #[error("expected a line break (too many arguments to #{directive:?})")]
    ExpectBreak { directive: &'static str },
    #[error("unclosed #if block")]
    UnclosedIf,
    #[error("#define should be followed by an identifier")]
    ExpectDefineName,
    #[error("found #else with no matching #if")]
    StandaloneElse,
    #[error("found #endif with no matching #if")]
    StandaloneEndif,
    #[error("macro {name:?} has duplicate arguments")]
    DuplicateMacroArg { name: String },
    #[error("invalid argument name in macro definition")]
    InvalidMacroArg,
    #[error(r#"macro body is empty (hint: if this is really what you want, use "")"#)]
    EmptyMacroBody,
    #[error("#undef should be followed by an identifier")]
    ExpectUndefName,
    #[error("#{directive:?} should be followed by a program name")]
    ExtBadProgram { directive: &'static str },
    #[error("macro cycle encountered")]
    MacroCycle { cycle: Vec<String> },
    #[error("macro {macro_:?} has no body, but is expanded here")]
    EmptyExpand { macro_: String },
    #[error("macro {name:?} is defined twice")]
    DuplicateMacro { name: String },
    #[error("{err:?} (in body of macro {name:?}, defined in {fname:?})")]
    LexErrorInMacroBody {
        err: LexError,
        name: String,
        fname: String,
    },
    #[error(transparent)]
    LexError(LexError),
}
