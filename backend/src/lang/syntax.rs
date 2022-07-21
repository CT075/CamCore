use std::rc::Rc;

use indexmap::IndexSet;
use relative_path::RelativePathBuf;

use crate::types::StringWithVars;

pub mod span;

pub use span::{Span, Spanned};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(Rc<String>),
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
        members: Vec<Spanned<TokenGroup>>,
    },
}

#[derive(Debug, Clone)]
pub enum Node {
    Message(StringWithVars),
    Line(Vec<Spanned<TokenGroup>>),
    Directive(Directive),
}

#[derive(Debug, Clone)]
pub enum MacroBody {
    Empty,
    Macro(Vec<Spanned<TokenGroup>>),
}

#[derive(Debug, Clone)]
pub enum Directive {
    IfDef(String, Tree, Tree),
    IfNDef(String, Tree, Tree),
    Define(String, Option<IndexSet<String>>, MacroBody),
    Include(RelativePathBuf),
    Incbin(RelativePathBuf),
    Incext(RelativePathBuf, Vec<StringWithVars>),
    Inctevent(RelativePathBuf, Vec<StringWithVars>),
    Pool,
    Undef(String),
}

#[derive(Debug, Clone)]
pub struct Tree(pub Vec<Spanned<Node>>);

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Minus,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(i32),
    Var(Rc<String>),
    Binop(Operator, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Line {
        head: Rc<String>,
        args: Vec<Spanned<Expr>>,
    },
    Label(Rc<String>),
}
