use std::{collections::VecDeque, convert::Infallible};

use indexmap::set::IndexSet;
use relative_path::RelativePathBuf;

use super::{
    super::lex::{Token as LexToken, TokenAnnot as LexTokenAnnot},
    token,
};

pub type Token = token::Token<Infallible>;
pub type TokenAnnot = token::FilePosAnnot<Token>;

pub fn convert_token(t: LexToken) -> Token {
    use token::Token::*;
    match t {
        Ident(s) => Ident(s),
        Number(i) => Number(i),
        QuotedString(s) => QuotedString(s),
        Colon => Colon,
        Dash => Dash,
        Emdash => Emdash,
        Slash => Slash,
        Star => Star,
        Plus => Plus,
        Percent => Percent,
        Ampersand => Ampersand,
        Dot => Dot,
        Bar => Bar,
        Caret => Caret,
        LShift => LShift,
        RShift => RShift,
        Comma => Comma,
        LCurly => LCurly,
        RCurly => RCurly,
        LParen => LParen,
        RParen => RParen,
        LBrack => LBrack,
        RBrack => RBrack,
        LAngle => LAngle,
        RAngle => RAngle,
        Break => Break,
        Semi => Semi,
        Directive(d) => Directive(d),
        Filepath(p) => Filepath(p),
        Error(_) => {
            panic!("BUG: [Error] variant passed to [preprocess::syntax::convertToken]")
        }
    }
}

pub fn convert(annot: LexTokenAnnot) -> TokenAnnot {
    annot.map(convert_token)
}

#[derive(Debug, PartialEq, Eq)]
pub enum Definition {
    Empty,
    Rename(VecDeque<Token>),
    Macro(IndexSet<String>, VecDeque<Token>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Tokens(Vec<TokenAnnot>),
    IfDef(String, Ast, Ast),
    Include(RelativePathBuf),
    Incbin(RelativePathBuf),
    Pool,
    Define(String, Definition),
    Undef(String),
    Malformed,
    Incext(RelativePathBuf, Vec<Token>),
    Inctevent(RelativePathBuf, Vec<Token>),
}

pub type Ast = Vec<Statement>;
