use std::convert::Infallible;

use super::token;

pub type Token = token::Token<Infallible>;
pub type TokenAnnot = token::FilePosAnnot<Token>;

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    LAnd,
    LOr,
    LXor,
    RShift,
    LShift,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(String),
    Number(u32),
    Binop(Operator, Box<Expr>, Box<Expr>),
    Coords(Vec<Expr>),
    Malformed,
}

#[derive(Debug, Clone)]
pub struct Opcode {
    head: String,
    operands: Vec<Expr>,
}

// builtin macros
pub enum Builtin {
    String,
    External,
    ExternalFixed,
}

pub(super) enum Arity {
    Fixed(usize),
    Variable,
}

pub enum Node {
    Op(Opcode),
    Label(String),
    Builtin(Builtin, Vec<Expr>),
}

pub type NodeAnnot = token::FilePosAnnot<Node>;
