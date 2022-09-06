use std::{fmt::Formatter, ops::Deref};

use super::*;

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Token::*;

        match self {
            Ident(s) => write!(f, "{}", s.deref()),
            Number { payload, radix } => match radix {
                10 => write!(f, "{}", payload),
                16 => write!(f, "0x{}", payload),
                2 => write!(f, "{}b", payload),
                _ => write!(f, "/*[BUG: unknown radix]*/{}", payload),
            },
            QuotedString(s) => write!(f, r#""{}""#, s),
            Colon => write!(f, ":"),
            Dash => write!(f, "-"),
            Semi => write!(f, ";"),
            Slash => write!(f, "/"),
            Star => write!(f, "*"),
            Plus => write!(f, "+"),
            Percent => write!(f, "%"),
            Ampersand => write!(f, "&"),
            Bar => write!(f, "|"),
            Caret => write!(f, "^"),
            Dot => write!(f, "."),
            LShift => write!(f, "<<"),
            RShift => write!(f, ">>"),
            Comma => write!(f, ","),
            LAngle => write!(f, "<"),
            RAngle => write!(f, ">"),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrace => write!(f, "["),
            RBrace => write!(f, "]"),
            LCurly => write!(f, "{{"),
            RCurly => write!(f, "}}"),
        }
    }
}

impl std::fmt::Display for TokenGroup {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            TokenGroup::Single((tok, _span)) => write!(f, "{}", tok),
            TokenGroup::Group { kind, members } => {
                let (open, close) = match kind {
                    GroupKind::Paren => ("(", ")"),
                    GroupKind::Square => ("[", "]"),
                    GroupKind::Curly => ("{", "}"),
                };

                write!(f, "{}", open)?;
                for (idx, (member, _span)) in members.iter().enumerate() {
                    if idx > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", member)?;
                }
                write!(f, "{}", close)
            }
        }
    }
}

// XXX: All of the following could probably be generated with macros. The
// existing [derive] macros aren't powerful enough (because they don't know
// to use [S::This] for the higher-kinded witnesses), but it should be fairly
// straightforward to do it ourselves.

impl<S> std::fmt::Debug for Argument<S>
where
    S: Witness<Expr>,
    S::This: std::fmt::Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Single(e) => write!(f, "Single({:?})", e),
            Self::List(es) => write!(f, "List({:?})", es),
            Self::Tuple(es) => write!(f, "Tuple({:?})", es),
        }
    }
}

impl<S> Clone for Argument<S>
where
    S: Witness<Expr>,
    S::This: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Single(e) => Self::Single(e.clone()),
            Self::List(es) => Self::List(es.clone()),
            Self::Tuple(es) => Self::Tuple(es.clone()),
        }
    }
}

impl<S> PartialEq for Argument<S>
where
    S: Witness<Expr>,
    S::This: PartialEq,
{
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Self::Single(e1), Self::Single(e2)) => e1 == e2,
            (Self::List(es1), Self::List(es2)) => es1 == es2,
            (Self::Tuple(es1), Self::Tuple(es2)) => es1 == es2,
            _ => false,
        }
    }
}

impl<S> Eq for Argument<S>
where
    S: Witness<Expr>,
    S::This: Eq,
{
}

impl<S> std::fmt::Debug for Statement<S>
where
    S: Witness<Argument<S>> + Witness<Expr>,
    <S as Witness<Expr>>::This: std::fmt::Debug,
    <S as Witness<Argument<S>>>::This: std::fmt::Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Instruction { head, args } => {
                write!(
                    f,
                    "Instruction {{ head: {:?}, args: {:?} }}",
                    head, args
                )
            }
            Self::Label(s) => write!(f, "Label({})", s),
        }
    }
}

impl<S> Clone for Statement<S>
where
    S: Witness<Argument<S>> + Witness<Expr>,
    <S as Witness<Expr>>::This: Clone,
    <S as Witness<Argument<S>>>::This: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Instruction { head, args } => Self::Instruction {
                head: head.clone(),
                args: args.clone(),
            },
            Self::Label(s) => Self::Label(s.clone()),
        }
    }
}

impl<S> PartialEq for Statement<S>
where
    S: Witness<Argument<S>> + Witness<Expr>,
    <S as Witness<Expr>>::This: PartialEq,
    <S as Witness<Argument<S>>>::This: PartialEq,
{
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (
                Self::Instruction { head: h1, args: a1 },
                Self::Instruction { head: h2, args: a2 },
            ) => h1 == h2 && a1 == a2,
            (Self::Label(s1), Self::Label(s2)) => s1 == s2,
            _ => false,
        }
    }
}

impl<S> Eq for Statement<S>
where
    S: Witness<Argument<S>> + Witness<Expr>,
    <S as Witness<Expr>>::This: Eq,
    <S as Witness<Argument<S>>>::This: Eq,
{
}
