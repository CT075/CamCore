use std::collections::VecDeque;

use indexmap::set::IndexSet;
use relative_path::RelativePathBuf;

#[derive(Debug, PartialEq, Eq)]
pub struct Token();

#[derive(Debug, PartialEq, Eq)]
pub struct TokenAnnot();

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
    Incext(RelativePathBuf, Vec<Token>),
    Inctevent(RelativePathBuf, Vec<Token>),
}

pub type Ast = Vec<Statement>;
