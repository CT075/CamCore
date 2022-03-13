use indexmap::IndexSet;
use relative_path::RelativePathBuf;

// The point of this trait is to allow us to use the same [Directive] enum
// with both parsed and unparsed arguments. In practice, implementors of this
// trait will have all types as [String] (unparsed), structured data with
// string/vars interpolated (parsed and unexpanded), or structured data with
// just strings (parsed and expanded).
pub trait Args {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Unparsed {}

impl Args for Unparsed {
    type Define = String;
    type Include = String;
    type Incbin = String;
    type Incext = String;
    type Inctevent = String;
    type IfDef = String;
    type IfNDef = String;
    type Else = String;
    type Endif = String;
    type Pool = String;
    type Undef = String;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Definition {
    Empty,
    Rename(Vec<super::Token>),
    Macro(IndexSet<String>, Vec<super::Token>),
    Builtin,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Parsed {}

impl Args for Parsed {
    type Define = (String, Definition);
    type Include = RelativePathBuf;
    type Incbin = (RelativePathBuf, String);
    type Incext = (RelativePathBuf, String);
    type Inctevent = (RelativePathBuf, String);
    type IfDef = String;
    type IfNDef = String;
    type Else = ();
    type Endif = ();
    type Pool = ();
    type Undef = ();
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive<A: Args> {
    Define(A::Define),
    Include(A::Include),
    Incbin(A::Incbin),
    Incext(A::Incext),
    Inctevent(A::Inctevent),
    IfDef(A::IfDef),
    IfNDef(A::IfNDef),
    Else(A::Else),
    Endif(A::Endif),
    Pool(A::Pool),
    Undef(A::Undef),
}
