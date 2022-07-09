use std::{ops::Range, rc::Rc};

use indexmap::IndexSet;
use relative_path::RelativePathBuf;

use crate::types::{
    hkt::{Apply, Functor, Witness},
    StringWithVars,
};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(String),
    Number { payload: String, radix: usize },
    QuotedString(String),
    Colon,
    Dash,
    Emdash,
    Semi,
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
    LAngle,
    RAngle,
    Error,
}

#[derive(Debug, Copy, Clone)]
pub enum GroupKind {
    Paren,
    Square,
    Curly,
}

#[derive(Debug, Clone)]
pub enum TokenGroup {
    Single(Spanned<Token>),
    Group {
        kind: GroupKind,
        members: Vec<TokenGroup>,
    },
}

#[derive(Debug, Clone)]
pub struct Tree(pub Vec<Spanned<Node>>);

#[derive(Debug, Clone)]
pub enum Node {
    Message(StringWithVars),
    Line(Vec<TokenGroup>),
    Directive(Directive),
}

#[derive(Debug, Clone)]
pub enum Definition {
    Empty,
    Macro(Vec<TokenGroup>),
    Builtin,
}

#[derive(Debug, Clone)]
pub enum Directive {
    IfDef(String, Tree, Tree),
    IfNDef(String, Tree, Tree),
    Define(String, Option<IndexSet<String>>, Definition),
    Include(RelativePathBuf),
    Incbin(RelativePathBuf),
    Incext(RelativePathBuf, Vec<StringWithVars>),
    Inctevent(RelativePathBuf, Vec<StringWithVars>),
    Pool,
    Undef(String),
}
