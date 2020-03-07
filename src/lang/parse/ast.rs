use crate::types::{Identifier, Void};

#[derive(Debug, PartialEq, Eq)]
pub struct GenericDirective {
    pub cmd: String,
    pub args: Vec<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct GenericInstr {
    pub instr: String,
    pub args: Vec<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct GenericMacro {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum EANumber {
    Byte(u8),
    Short(u16),
    Word(u32),
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
    Number(EANumber),
    Macro(MacroType),
}

pub type ParsedExpr = ExprNode<GenericMacro>;
pub type FlatExpr = ExprNode<Void>;
