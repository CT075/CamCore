use std::ops::Range;

pub type Span = Range<usize>;

#[derive(Debug, Clone)]
pub struct Location<'a> {
    pub span: Option<Span>,
    pub owner: Option<&'a str>,
    pub needed_by: Option<Box<Location<'a>>>,
}

#[derive(Debug, Clone)]
pub struct WithLocation<'a, T> {
    pub value: T,
    pub loc: Location<'a>,
}

#[derive(Debug, Clone)]
pub enum MessageContent {
    Verbatim(String),
    CurrentOffset,
}

#[derive(Debug, Clone)]
pub enum Directive {
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

#[derive(Debug, Clone)]
pub enum Token {
    Ident(String),
    Number(u32),
    QuotedString(String),
    Colon,
    Dash,
    Emdash,
    Slash,
    Star,
    Plus,
    Percent,
    Ampersand,
    Dot,
    Bar,
    Caret,
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
    Semi,
}
