use phf::phf_map;

use thiserror::Error;

use super::{
    state::{Definition, State},
    {FilePosStream, StreamStack, TokenAnnot},
};

static DIRECTIVES: phf::Map<&'static str, Dispatch> = phf_map! {
    "define" => Dispatch::Define,
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

#[derive(Debug)]
enum Dispatch {
    Define,
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

pub(super) fn process(
    directive: String,
    stream: &mut StreamStack,
    state: &mut State,
) -> Result<Option<FilePosStream<Box<dyn Iterator<Item = char>>>>, DirectiveError> {
    use Dispatch::*;

    match DIRECTIVES
        .get(directive.as_str())
        .map(|d| (d, state.active()))
    {
        None => Err(DirectiveError::InvalidDirective { s: directive }),
        Some((Else, _)) => else_(state).map(|_| None),
        Some((Endif, _)) => endif(state).map(|_| None),
        Some((_, false)) => Ok(None),
        Some((Ifdef, _)) => ifdef(stream, state).map(|_| None),
        Some((Define, _)) => define(stream, state).map(|_| None),
        _ => panic!("todo"),
    }
}

use DirectiveError::*;

fn define(stream: &mut StreamStack, state: &mut State) -> Result<(), DirectiveError> {
    let src = match stream.next_non_ws() {
        None => return Err(DefineNoSrc),
        Some(c) if c.is_alphabetic() => {
            c.to_string() + &stream.take_while(char::is_alphanumeric)
        }
        _ => return Err(DefineBadIdentifier),
    };

    match stream.next_char_only() {
        None | Some('\n') => state.define(src, Definition::Empty),
        Some(c) if c.is_whitespace() => {
            state.define(src, Definition::Rename(stream.to_end_of_line()))
        }
        Some('(') => panic!("todo"),
        Some(_) => Err(DefineBadIdentifier),
    }
}

fn ifdef(stream: &mut StreamStack, state: &mut State) -> Result<(), DirectiveError> {
    let src = match stream.next_non_ws() {
        None => return Err(IfdefNoSrc),
        Some(c) if c.is_alphabetic() => {
            c.to_string() + &stream.take_while(char::is_alphanumeric)
        }
        _ => return Err(IfdefBadIdentifier),
    };

    Ok(())
}

fn else_(state: &mut State) -> Result<(), DirectiveError> {
    match state.pop() {
        Some(b) => {
            state.push(!b);
            Ok(())
        }
        None => Err(ElseEmptyStack),
    }
}

fn endif(state: &mut State) -> Result<(), DirectiveError> {
    match state.pop() {
        Some(_) => Ok(()),
        None => Err(EndifEmptyStack),
    }
}
