use relative_path::RelativePathBuf;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FilePosAnnot<T> {
    pub value: T,
    pub row: usize,
    pub col: usize,
}

impl<T> FilePosAnnot<T> {
    pub fn annot(value: T, row: usize, col: usize) -> Self {
        FilePosAnnot { value, row, col }
    }

    pub fn borrow_value(&self) -> &T {
        let FilePosAnnot {
            value,
            row: _,
            col: _,
        } = self;
        value
    }

    pub fn extract_value(self) -> T {
        let FilePosAnnot {
            value,
            row: _,
            col: _,
        } = self;
        value
    }

    pub fn map<F, T2>(self, f: F) -> FilePosAnnot<T2>
    where
        F: Fn(T) -> T2,
    {
        let FilePosAnnot { value, row, col } = self;
        FilePosAnnot {
            value: f(value),
            row,
            col,
        }
    }

    pub fn substitute<T2>(self, value: T2) -> FilePosAnnot<T2> {
        FilePosAnnot {
            value,
            row: self.row,
            col: self.col,
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
