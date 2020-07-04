use phf::phf_map;

use thiserror::Error;

pub(in crate::lang) static DIRECTIVES: phf::Map<&'static str, Dispatch> = phf_map! {
    "define" => Dispatch::Define,
    "include" => Dispatch::Include,
    "incbin" => Dispatch::Incbin,
    "incext" => Dispatch::Incext,
    "inctevent" => Dispatch::Inctevent,
    "ifdef" => Dispatch::IfDef,
    "ifndef" => Dispatch::IfNDef,
    "else" => Dispatch::Else,
    "endif" => Dispatch::Endif,
    "pool" => Dispatch::Pool,
    "undef" => Dispatch::Undef,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Dispatch {
    Define,
    Include,
    Incbin,
    Incext,
    Inctevent,
    IfDef,
    IfNDef,
    Else,
    Endif,
    Pool,
    Undef,
}

#[derive(Error, Debug)]
pub enum DirectiveError {
    #[error("#{s:?} is not a directive")]
    InvalidDirective { s: String },
    #[error("no identifier passed to #define")]
    DefineNoSrc,
    #[error("can't define bad identifier")]
    DefineBadIdentifier,
    #[error("duplicate definition")]
    DefineDuplicate,
    #[error("included subfile ends with open block comment")]
    IncludeOpenComment,
    #[error("included subfile ends with unclosed #if directive")]
    IncludeOpenIf,
    #[error("#ifdef is missing an argument")]
    IfdefNoSrc,
    #[error("argument to #ifdef must be an identifier")]
    IfdefBadIdentifier,
    #[error("found unmatched #else")]
    ElseEmptyStack,
    #[error("found unmatched #endif")]
    EndifEmptyStack,
}
