use std::fmt;

use relative_path::RelativePathBuf;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Directive {
    Define,
    Include,
    Incbin,
    Incext,
    Inctevent,
    IfDef,
    IfNDef,
    Else,
    Endif,
    Pool,
    Undef,
}

impl Directive {
    fn to_string_human(&self) -> &'static str {
        use Directive::*;
        match self {
            Define => "#define",
            Include => "#include",
            Incbin => "#incbin",
            Incext => "#incext",
            Inctevent => "#inctevent",
            IfDef => "#ifdef",
            IfNDef => "#ifndef",
            Else => "#else",
            Endif => "#endif",
            Pool => "#pool",
            Undef => "#undef",
        }
    }
}

impl fmt::Display for Directive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string_human())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token<E> {
    Ident(String),
    Number(u32),
    QuotedString(String),
    Colon,
    Dash,
    Emdash,
    Slash,
    Star,
    Plus,
    Percent,
    Ampersand,
    Dot,
    Bar,
    Caret,
    LShift,
    RShift,
    //Hash,
    Comma,
    LCurly,
    RCurly,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LAngle,
    RAngle,
    Break,
    Semi,
    Directive(Directive),
    Filepath(RelativePathBuf),
    Error(E),
}

impl<E> Token<E> {
    pub fn map<F, E2>(self, f: F) -> Token<E2>
    where
        F: Fn(E) -> E2,
    {
        use Token::*;

        match self {
            Ident(s) => Ident(s),
            Number(n) => Number(n),
            QuotedString(s) => QuotedString(s),
            Colon => Colon,
            Dash => Dash,
            Emdash => Emdash,
            Slash => Slash,
            Star => Star,
            Plus => Plus,
            Percent => Percent,
            Ampersand => Ampersand,
            Bar => Bar,
            Dot => Dot,
            Caret => Caret,
            LShift => LShift,
            RShift => RShift,
            Comma => Comma,
            LCurly => LCurly,
            RCurly => RCurly,
            LParen => LParen,
            RParen => RParen,
            LBrack => LBrack,
            RBrack => RBrack,
            LAngle => LAngle,
            RAngle => RAngle,
            Break => Break,
            Semi => Semi,
            Directive(d) => Directive(d),
            Filepath(p) => Filepath(p),
            Error(e) => Error(f(e)),
        }
    }
}
