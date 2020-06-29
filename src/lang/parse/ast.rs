use crate::types::{Identifier, Void};

pub enum Statement<MacroType> {
    Directive(GenericDirective),
    Cmds(Vec<GenericInstr<MacroType>>),
    Label(Identifier),
}

#[derive(Debug, PartialEq, Eq)]
pub struct GenericDirective {
    pub cmd: String,
    pub args: Vec<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParamNode {
    Expr(ParsedExpr),
    Str(String),
    List(Vec<ParsedExpr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum InstrHead<MacroType> {
    Raw(Identifier),
    Macro(MacroType),
}

#[derive(Debug, PartialEq, Eq)]
pub struct GenericInstr<MacroType> {
    pub front: InstrHead<MacroType>,
    pub args: Vec<ParamNode>,
}

pub type ParsedInstr = GenericInstr<GenericMacro>;
pub type RawInstr = GenericInstr<Void>;

#[derive(Debug, PartialEq, Eq)]
pub struct GenericMacro {
    pub name: Identifier,
    pub args: Vec<ParamNode>,
}

// The MacroType argument represents the type that is used to hold macro
// invocations. It will either be MacroType or Void.
#[derive(Debug, PartialEq, Eq)]
pub enum ExprNode<MacroType> {
    Add(Box<ExprNode<MacroType>>, Box<ExprNode<MacroType>>),
    Sub(Box<ExprNode<MacroType>>, Box<ExprNode<MacroType>>),
    Mul(Box<ExprNode<MacroType>>, Box<ExprNode<MacroType>>),
    Div(Box<ExprNode<MacroType>>, Box<ExprNode<MacroType>>),
    Lshift(Box<ExprNode<MacroType>>, Box<ExprNode<MacroType>>),
    Rshift(Box<ExprNode<MacroType>>, Box<ExprNode<MacroType>>),
    LAnd(Box<ExprNode<MacroType>>, Box<ExprNode<MacroType>>),
    LOr(Box<ExprNode<MacroType>>, Box<ExprNode<MacroType>>),
    LXor(Box<ExprNode<MacroType>>, Box<ExprNode<MacroType>>),
    Symbol(Identifier),
    Number(u32),
    Macro(MacroType),
}

pub type ParsedExpr = ExprNode<GenericMacro>;
pub type FlatExpr = ExprNode<Void>;
