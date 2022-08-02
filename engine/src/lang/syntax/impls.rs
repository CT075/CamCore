use std::{fmt::Formatter, ops::Deref};

use super::{GroupKind, Token, TokenGroup};

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
            Emdash => write!(f, "--"),
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

                write!(f, "{}", open);
                for (idx, (member, _span)) in members.iter().enumerate() {
                    if idx > 0 {
                        write!(f, " ");
                    }
                    write!(f, "{}", member);
                }
                write!(f, "{}", close)
            }
        }
    }
}
