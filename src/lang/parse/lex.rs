use std::char;
use std::marker::PhantomData;
use std::io::{Bytes, Read};
use std::iter::Peekable;

pub mod errors {
    use error_chain::error_chain;

    error_chain! {
        errors { Eof }
    }
}

use errors::{Result};

pub trait Lexable {
    type LexBuf;

    fn newbuf() -> Self::LexBuf;
    fn push(buf: &mut Self::LexBuf, c: char) -> Result<()>;
    fn next(buf: &mut Self::LexBuf) -> Option<Box<Self>>;
}

pub struct Lexer<T:Lexable, R: Read>
{ buf: T::LexBuf
, stream: Peekable<Bytes<R>>
, phantom: PhantomData<T>
}

impl<T:Lexable, R:Read> Lexer<T,R> {
    pub fn new(r:R) -> Self {
        Lexer
        { buf:T::newbuf()
        , stream: r.bytes().peekable()
        , phantom: PhantomData
        }
    }

    fn current(&mut self) -> Result<char> {
        if let Some(&Ok(byte)) = self.stream.peek() { Ok(byte as char) }
        else { Err(errors::ErrorKind::Eof.into()) }
    }

    pub fn token(&mut self) -> Result<Option<T>> {
        let mut r = T::next(&mut self.buf);
        while r.is_none() {
            let ch = self.current()?;
            T::push(&mut self.buf, ch)?;
            r = T::next(&mut self.buf)
        }
        match r {
            Some(t) => Ok(Some(*t)),
            None => Ok(None)
        }
    }
}

