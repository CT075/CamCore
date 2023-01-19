use indexmap::IndexSet;
use relative_path::RelativePathBuf;

use crate::types::{hkt::Witness, StringWithVars};

mod impls;
pub mod span;

pub use impls::*;
pub use span::{Span, Spanned};

// TODO: Eventually, we should implement a string interner, probably at
// preprocessing time.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(String),
    Number { payload: String, radix: usize },
    QuotedString(String),
    Colon,
    Dash,
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
    LParen,
    RParen,
    LBrace,
    RBrace,
    LCurly,
    RCurly,
}

#[derive(Debug, Copy, Clone)]
pub enum GroupKind {
    Paren,
    Square,
    Curly,
}

impl GroupKind {
    pub fn delimiters(&self) -> (Token, Token) {
        match self {
            Self::Paren => (Token::LParen, Token::RParen),
            Self::Square => (Token::LBrace, Token::RBrace),
            Self::Curly => (Token::LCurly, Token::RCurly),
        }
    }
}

// TODO: In places where we don't care about the span (e.g. in macro
// definitions), we shouldn't bother storing it. We can use HKT to accomplish
// this.
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl Operator {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Literal(i32),
    Var(String),
    Binop(Operator, Box<Expr>, Box<Expr>),
}

/// When testing parsers, it's often useful to be able to strip the spans to
/// reduce the locational noise in the test output. Typically, this is
/// accomplished by having a secondary data structure mirroring the first,
/// except with all spans removed, e.g. `UnspannedArgument`. This is an unwieldy
/// maintenance burden, as every time these data structures change, we need to
/// update all definitions and any conversion functions.
///
/// Instead, we can take advantage of Rust's powerful type system to create a
/// data structure that is generic over containing a span. This trick is called
/// "higher-kinded data"[^1]. Unfortunately, Rust can't encode higher-kinded
/// types directly (yet!), so there's a bit more noise (see the source of
/// `crate::types::hkt` for a longer explanation).
///
/// For general use, the important thing to remember is that, for `SpannedW`,
/// `S::This` is `Spanned<T>`, where `T` is listed in the argument to the
/// `Witness` that `S` implements. In tests, you might also see `IdentityW`,
/// where `S::This` is just `T` alone.
///
/// A more lightweight solution that doesn't require HKT magic would be to
/// skip `Spanned` entirely, taking `Span` parameter as the second argument of
/// each `Spanned` tuple, e.g.:
///
/// ```no_run
/// # enum Expr {}
/// enum Argument<Span> {
///   // ...
///   List(Vec<(Expr, Span)>)
/// }
/// ```
///
/// where we set `Span` to `()` after being stripped. However, this leaves the
/// extra tuple noise in our debug output.
///
/// [^1]: https://reasonablypolymorphic.com/blog/higher-kinded-data/
///
///   Note that this article is pretty advanced, and we don't use most of it.
///   Only the first half, where it explains how to consolidate two nominally
///   related representations into the same generic representation, is really
///   necessary to understand this data type.
// XXX: Extend this framework to the syntax (e.g. [Tree], etc) above.
pub enum Argument<S: Witness<Expr>> {
    Single(Expr),
    List(Vec<S::This>),
    Tuple(Vec<S::This>),
}

pub enum Statement<S: Witness<Argument<S>> + Witness<Expr>> {
    Instruction {
        head: String,
        args: Vec<<S as Witness<Argument<S>>>::This>,
    },
    Label(String),
}

pub enum Event<S: Witness<Argument<S>> + Witness<Expr>> {
    Statement(Statement<S>),
    OpenScope,
    CloseScope,
}

mod private {
    use super::span::SpannedW;

    #[cfg(test)]
    use crate::types::hkt::IdentityW;

    pub trait Sealed {}

    impl Sealed for SpannedW {}

    // We hide this impl behind [cfg(test)] to enforce that we carry spans in
    // any actual code. That way, we can't accidentally drop spans between
    // phases.
    #[cfg(test)]
    impl Sealed for IdentityW {}
}
