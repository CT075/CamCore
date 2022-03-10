use std::ops::Range;

pub type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location<'a> {
    pub span: Option<Span>,
    pub owner: Option<&'a str>,
    pub needed_by: Option<Box<Location<'a>>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WithLocation<'a, T> {
    pub value: T,
    pub loc: Location<'a>,
}

// The point of this trait is to allow us to use the same [Directive] enum
// with both parsed and unparsed arguments. In practice, implementors of this
// trait will have all types as [String] (unparsed), structured data with
// string/vars interpolated (parsed and unexpanded), or structured data with
// just strings (parsed and expanded).
pub trait DirectiveArgs {
    type Define: std::fmt::Debug + PartialEq + Eq + Clone;
    type Include: std::fmt::Debug + PartialEq + Eq + Clone;
    type Incbin: std::fmt::Debug + PartialEq + Eq + Clone;
    type Incext: std::fmt::Debug + PartialEq + Eq + Clone;
    type Inctevent: std::fmt::Debug + PartialEq + Eq + Clone;
    type IfDef: std::fmt::Debug + PartialEq + Eq + Clone;
    type IfNDef: std::fmt::Debug + PartialEq + Eq + Clone;
    type Else: std::fmt::Debug + PartialEq + Eq + Clone;
    type Endif: std::fmt::Debug + PartialEq + Eq + Clone;
    type Pool: std::fmt::Debug + PartialEq + Eq + Clone;
    type Undef: std::fmt::Debug + PartialEq + Eq + Clone;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive<Args: DirectiveArgs> {
    Define(Args::Define),
    Include(Args::Include),
    Incbin(Args::Incbin),
    Incext(Args::Incext),
    Inctevent(Args::Inctevent),
    IfDef(Args::IfDef),
    IfNDef(Args::IfNDef),
    Else(Args::Else),
    Endif(Args::Endif),
    Pool(Args::Pool),
    Undef(Args::Undef),
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
