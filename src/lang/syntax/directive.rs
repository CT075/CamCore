use indexmap::IndexSet;
use relative_path::RelativePathBuf;

use crate::types::StringWithVars;

// The point of this trait is to allow us to use the same [Directive] enum
// with both parsed and unparsed arguments. In practice, implementors of this
// trait will have all types as [String] (unparsed), structured data with
// string/vars interpolated (parsed and unexpanded), or structured data with
// just strings (parsed and expanded).
pub trait Args {
    type Define: std::fmt::Debug + PartialEq + Eq;
    type Include: std::fmt::Debug + PartialEq + Eq;
    type Incbin: std::fmt::Debug + PartialEq + Eq;
    type Incext: std::fmt::Debug + PartialEq + Eq;
    type Inctevent: std::fmt::Debug + PartialEq + Eq;
    type IfDef: std::fmt::Debug + PartialEq + Eq;
    type IfNDef: std::fmt::Debug + PartialEq + Eq;
    type Else: std::fmt::Debug + PartialEq + Eq;
    type Endif: std::fmt::Debug + PartialEq + Eq;
    type Pool: std::fmt::Debug + PartialEq + Eq;
    type Undef: std::fmt::Debug + PartialEq + Eq;
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

// For when you just need to know which directive without knowing the
// arguments.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NoPayload {}

impl Args for NoPayload {
    type Define = ();
    type Include = ();
    type Incbin = ();
    type Incext = ();
    type Inctevent = ();
    type IfDef = ();
    type IfNDef = ();
    type Else = ();
    type Endif = ();
    type Pool = ();
    type Undef = ();
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Unparsed {}

// We don't parse these in the lexer because that logic can be a bit more
// complex and should be handled separately, expecially for the sake of error
// handling.
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
    type Incbin = RelativePathBuf;
    type Incext = (RelativePathBuf, Vec<StringWithVars>);
    type Inctevent = (RelativePathBuf, Vec<StringWithVars>);
    type IfDef = String;
    type IfNDef = String;
    type Else = ();
    type Endif = ();
    type Pool = ();
    type Undef = String;
}

// This type is a generic representation of the "directive tree", something of
// the form:
//
//   #ifdef SOME_IDENTIFIER
//     some tokens...
//     #include some_file.event
//   #else
//     #ifdef SOME_OTHER_IDENTIFIER
//       some tokens
//     #endif
//   #endif
//
// where each level of indentation represents a node of the tree. The main
// mechanism of nesting is the [IfDef] node (and equivalently the [IfNDef],
// which contain their corresponding subblocks.
//
// The type/trait impl looks so weird because it is intended to work with
// [lexer::OutImpl] type to intersperse the directive tree with regular tokens
// and messages. As an example, from [preprocess.rs]:
//
//   struct TreeUnexpanded(
//       Box<
//           OutImpl<
//               directive::Tree<
//                   TreeUnexpanded,
//                   RelativePathBuf,
//                   (RelativePathBuf, StringWithVars),
//               >,
//               IdentityW,
//               IdentityW,
//           >,
//       >,
//   );
//
// Ignoring the non-recursive parameters for a moment, we can reduce the type
// to this:
//
//   struct TreeUnexpanded(Box<OutImpl<Tree<TreeUnexpanded, ...>, ...>>)
//
// Unfolding the definition of [OutImpl] once, we get [the equivalent of]:
//
//   enum TreeUnexpanded {
//     Token(...),
//     Directive(<L as Witness<Directive<Tree<TreeUnexpanded>>>>::This),
//     Message(...),
//   }
//
// If we assume [L] is [IdentityW], we can simplify things further to
//
//   enum TreeUnexpanded {
//     Token(...),
//     Directive(Directive<Tree<TreeUnexpanded>>),
//     Message(...),
//   }
//
// which, if we expand the enum [Directive<Tree<TreeUnexpanded>>], becomes a
// more obvious tree-like type.
//
// This trick is known as "tying the knot", and is useful in situations like
// this, where we need our nodes (in this case, the types attached to each
// [Directive] variant) to have different types in different phases.
//
// Finally, we can use the [Include] and [Inctevent] parameters to determine
// whether we've expanded those constructs. That is, [Include] could be
// [RelativePathBuf] (still need to read the file) or [Vec<Tree>] (we've parsed
// the file and put the new AST inline).
#[derive(Debug, PartialEq, Eq)]
pub struct Tree<T, Include, Inctevent> {
    phantom: std::marker::PhantomData<(T, Include, Inctevent)>,
    absurd: std::convert::Infallible,
}

impl<T, Include, Inctevent> Args for Tree<T, Include, Inctevent>
where
    T: std::fmt::Debug + PartialEq + Eq,
    Include: std::fmt::Debug + PartialEq + Eq,
    Inctevent: std::fmt::Debug + PartialEq + Eq,
{
    type Define = (String, Definition);
    type Include = Include;
    type Incbin = RelativePathBuf;
    type Incext = (RelativePathBuf, StringWithVars);
    type Inctevent = Inctevent;
    type IfDef = (String, Vec<T>, Vec<T>);
    type IfNDef = (String, Vec<T>, Vec<T>);
    type Else = std::convert::Infallible;
    type Endif = std::convert::Infallible;
    type Pool = std::convert::Infallible;
    type Undef = String;
}
