use std::{ops::Range, rc::Rc};

use crate::types::hkt::{Apply, Functor, Witness};

pub type Span = Range<usize>;

pub fn restrict_span(outer: &Span, inner: &Span) -> Span {
    outer.start + inner.start..outer.start + inner.end
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub owner: Option<Rc<str>>,
    pub needed_by: Option<Rc<Location>>,
}

pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpannedW {}

impl<T> Witness<T> for SpannedW {
    type This = Spanned<T>;
}

impl Functor for SpannedW {
    fn fmap<A, B, F>(this: Apply<Self, A>, f: F) -> Apply<Self, B>
    where
        F: Fn(A) -> B,
    {
        let (x, span) = this.prj();
        Apply::inj((f(x), span))
    }
}

pub type WithLocation<T> = (T, Location);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WithLocationW {}

impl<T> Witness<T> for WithLocationW {
    type This = WithLocation<T>;
}

impl Functor for WithLocationW {
    fn fmap<A, B, F>(this: Apply<Self, A>, f: F) -> Apply<Self, B>
    where
        F: Fn(A) -> B,
    {
        let (x, loc) = this.prj();
        Apply::inj((f(x), loc))
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
