use std::{ops::Range, rc::Rc};

use crate::types::hkt::Witness;

pub type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub span: Option<Span>,
    pub owner: Option<Rc<str>>,
    pub needed_by: Option<Rc<Location>>,
}

pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpannedW {}

impl<T> Witness<T> for SpannedW {
    type This = Spanned<T>;

    fn absurd(self) -> std::convert::Infallible {
        match self {}
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WithLocation<T> {
    pub value: T,
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WithLocationW {}

impl<T> Witness<T> for WithLocationW {
    type This = WithLocation<T>;

    fn absurd(self) -> std::convert::Infallible {
        match self {}
    }
}

pub mod directive;
pub use directive::Directive;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(String),
    Number { payload: String, radix: usize },
    QuotedString(String),
    Colon,
    Dash,
    Emdash,
    Slash,
    Star,
    Plus,
    Percent,
    Ampersand,
    Bar,
    Caret,
    Dot,
    LShift,
    RShift,
    //Hash,
    Comma,
    LCurly,
    RCurly,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LAngle,
    RAngle,
    Break,
}
