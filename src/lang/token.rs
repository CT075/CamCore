use std::convert::From;
use std::path::PathBuf;

use super::preprocess::directive::Dispatch;

#[derive(Debug, Clone)]
pub enum Token<E> {
    Ident(String),
    Number(u32),
    QuotedString(String),
    Colon,
    Dash,
    Slash,
    Star,
    Plus,
    Percent,
    Ampersand,
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
    Directive(Dispatch),
    Filepath(PathBuf),
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
            Slash => Slash,
            Star => Star,
            Plus => Plus,
            Percent => Percent,
            Ampersand => Ampersand,
            Bar => Bar,
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

#[derive(Debug, Clone)]
pub struct FilePosAnnot<T> {
    pub value: T,
    pub fname: String,
    pub row: usize,
    pub col: usize,
}

impl<T> FilePosAnnot<T> {
    pub fn annot(value: T, fname: String, row: usize, col: usize) -> Self {
        FilePosAnnot {
            value,
            fname,
            row,
            col,
        }
    }

    pub fn filename(&self) -> &str {
        &self.fname
    }

    pub fn borrow_value(&self) -> &T {
        let FilePosAnnot {
            value,
            row: _,
            col: _,
            fname: _,
        } = self;
        value
    }

    pub fn extract_value(self) -> T {
        let FilePosAnnot {
            value,
            row: _,
            col: _,
            fname: _,
        } = self;
        value
    }

    pub fn map<F, T2>(self, f: F) -> FilePosAnnot<T2>
    where
        F: Fn(T) -> T2,
    {
        let FilePosAnnot {
            value,
            row,
            col,
            fname,
        } = self;
        FilePosAnnot {
            value: f(value),
            row,
            col,
            fname,
        }
    }
}

impl<T> FilePosAnnot<T>
where
    T: Copy,
{
    pub fn copy_value(&self) -> T {
        self.value
    }
}
