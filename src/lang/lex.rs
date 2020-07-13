use std::collections::VecDeque;
use std::path::PathBuf;

use genawaiter::rc::{Co, Gen};
use phf::phf_map;
use thiserror::Error;

use super::{
    token,
    token::{Directive, FilePosAnnot},
};

mod pathutil;

#[cfg(test)]
mod tests;

pub(in crate::lang) static DIRECTIVES: phf::Map<&'static str, Directive> = phf_map! {
    "define" => Directive::Define,
    "include" => Directive::Include,
    "incbin" => Directive::Incbin,
    "incext" => Directive::Incext,
    "inctevent" => Directive::Inctevent,
    "ifdef" => Directive::IfDef,
    "ifndef" => Directive::IfNDef,
    "else" => Directive::Else,
    "endif" => Directive::Endif,
    "pool" => Directive::Pool,
    "undef" => Directive::Undef,
};

#[derive(Error, Debug, Clone)]
pub enum LexError {
    #[error("found bad character {chr:?}")]
    BadChar { chr: char },
    #[error("unclosed quoted string")]
    UnclosedQuote,
    #[error("unclosed block comment")]
    UnclosedComment,
    // XXX: We have to lose some specificity here, because snailquote::UnescapeError
    // doesn't implement [Clone]. However, I don't think it matters;
    #[error("error in parsing quoted string: {0:?}")]
    Unescape(String),
    #[error("bad number for base {base:?}: {s:?}")]
    InvalidBaseNum { s: String, base: usize },
    #[error("number {n:?} out of range for 32 bits")]
    Overflow { n: u64 },
    #[error("undefined directive {s:?}")]
    BadDirective { s: String },
    #[error("bad character in filepath: {c:?}")]
    BadPathChar { c: char },
}

// XXX: need a better name for this
struct FilePosStream<I: Iterator> {
    wrapped: I,
    fname: String,
    row: usize,
    col: usize,
    // XXX: this is probably overkill
    buffer: VecDeque<FilePosAnnot<I::Item>>,
    comment_nesting: usize,
    in_line_comment: bool,
}

pub type Token = token::Token<LexError>;
pub type TokenAnnot = FilePosAnnot<Token>;

impl<I> FilePosStream<I>
where
    I: Iterator,
{
    fn annot(&self, value: I::Item) -> FilePosAnnot<I::Item> {
        FilePosAnnot::annot(value, self.fname.clone(), self.row, self.col)
    }

    fn buffer(&mut self, value: I::Item) {
        self.buffer.push_back(self.annot(value));
    }

    fn advance_row(&mut self) {
        self.row += 1;
        self.col = 1;
    }

    fn advance_col(&mut self) {
        self.col += 1;
    }
}

impl<I> FilePosStream<I>
where
    I: Iterator<Item = char>,
{
    fn new(fname: String, stream: I) -> FilePosStream<I> {
        FilePosStream {
            wrapped: stream,
            fname,
            row: 1,
            col: 1,
            buffer: VecDeque::new(),
            comment_nesting: 0,
            in_line_comment: false,
        }
    }

    // XXX: This is basically stacking two iterators on top of each other.
    fn advance(&mut self) -> Option<FilePosAnnot<char>> {
        if let Some(result) = self.buffer.pop_front() {
            return Some(result);
        }

        self.wrapped.next().and_then(|c| match c {
            '\\' => match self.wrapped.next() {
                Some('\n') => {
                    self.advance_row();
                    self.wrapped.next().map(|c| self.annot(c))
                }
                Some(c) => {
                    let result = self.annot('\\');
                    self.advance_col();
                    self.buffer(c);

                    Some(result)
                }
                None => Some(self.annot('\\')),
            },
            '\n' => {
                let result = self.annot('\n');
                self.advance_row();
                Some(result)
            }
            _ => {
                let result = self.annot(c);
                self.advance_col();
                Some(result)
            }
        })
    }

    fn next_char_only(&mut self) -> Option<char> {
        self.next().map(|v| v.extract_value())
    }

    fn next_non_ws(&mut self) -> Option<char> {
        self.skip_while(|c| c.is_whitespace() && c != '\n');
        self.next_char_only()
    }

    fn take_while<F: Fn(char) -> bool>(&mut self, pred: F) -> String {
        let mut result = String::new();

        while let Some(c) = self.next_char_only() {
            if !pred(c) {
                self.buffer(c);
                break;
            }

            result.push(c);
        }

        result
    }

    fn skip_while<F: Fn(char) -> bool>(&mut self, pred: F) -> () {
        self.take_while(pred);
    }

    async fn yield_single(
        &self,
        co: &Co<TokenAnnot, ()>,
        value: Token,
        row: usize,
        col: usize,
    ) -> () {
        co.yield_(TokenAnnot::annot(value, self.fname.clone(), row, col))
            .await;
    }

    async fn dispatch(
        &mut self,
        co: &Co<TokenAnnot, ()>,
        c: char,
        row: usize,
        col: usize,
    ) -> () {
        use token::Token::*;

        match c {
            ';' => self.yield_single(co, Semi, row, col).await,
            ':' => self.yield_single(co, Colon, row, col).await,
            '{' => self.yield_single(co, LCurly, row, col).await,
            '}' => self.yield_single(co, RCurly, row, col).await,
            '[' => self.yield_single(co, LBrack, row, col).await,
            ']' => self.yield_single(co, RBrack, row, col).await,
            '(' => self.yield_single(co, LParen, row, col).await,
            ')' => self.yield_single(co, RParen, row, col).await,
            '*' => self.yield_single(co, Star, row, col).await,
            '%' => self.yield_single(co, Percent, row, col).await,
            ',' => self.yield_single(co, Comma, row, col).await,
            // comments are handled below
            '/' => self.yield_single(co, Slash, row, col).await,
            '+' => self.yield_single(co, Plus, row, col).await,
            '-' => self.yield_single(co, Dash, row, col).await,
            '&' => self.yield_single(co, Ampersand, row, col).await,
            '^' => self.yield_single(co, Caret, row, col).await,
            '|' => self.yield_single(co, Bar, row, col).await,
            '"' => {
                let result = self.quoted_string();
                self.yield_single(co, result, row, col).await
            }
            '<' => {
                if let Some('<') = self.next_char_only() {
                    self.yield_single(co, LShift, row, col).await;
                } else {
                    self.buffer(c);
                    self.yield_single(co, LAngle, row, col).await;
                }
            }
            '>' => {
                if let Some('>') = self.next_char_only() {
                    self.yield_single(co, RShift, row, col).await;
                } else {
                    self.buffer(c);
                    self.yield_single(co, RAngle, row, col).await;
                }
            }
            '\n' => self.yield_single(co, Break, row, col).await,
            '#' => self.lex_directive(co, row, col).await,
            '$' => {
                let result = self.hexadecimal();
                self.yield_single(co, result, row, col).await
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let result = self.ident(c);
                self.yield_single(co, result, row, col).await
            }
            '0' => match self.next_char_only() {
                Some('x') => {
                    let result = self.hexadecimal();
                    self.yield_single(co, result, row, col).await
                }
                Some('b') => {
                    let result = self.binary();
                    self.yield_single(co, result, row, col).await
                }
                Some(c) => {
                    self.buffer('0');
                    self.buffer(c);
                    let result = self.decimal();
                    self.yield_single(co, result, row, col).await
                }
                None => self.yield_single(co, Number(0), row, col).await,
            },
            '1'..='9' => {
                self.buffer(c);
                let result = self.decimal();
                self.yield_single(co, result, row, col).await
            }
            c if c.is_whitespace() => (),
            _ => {
                self.yield_single(co, Error(LexError::BadChar { chr: c }), row, col)
                    .await
            }
        }
    }

    async fn lex_directive(&mut self, co: &Co<TokenAnnot, ()>, row: usize, col: usize) {
        let s = self.take_while(char::is_alphabetic);
        match DIRECTIVES.get(s.as_str()) {
            None => {
                self.yield_single(
                    co,
                    Token::Error(LexError::BadDirective { s }),
                    row,
                    col,
                )
                .await
            }
            Some(&d) => {
                self.yield_single(co, Token::Directive(d), row, col).await;
                if let Directive::Include | Directive::Incbin = d {
                    self.skip_while(|c| c.is_whitespace() && c != '\n');
                    self.filepath(co).await;
                }
            }
        }
    }

    async fn filepath(&mut self, co: &Co<TokenAnnot, ()>) -> () {
        let row = self.row;
        let col = self.col;

        let mut result = PathBuf::new();
        let mut buf = String::new();

        let quoted = match self.next_char_only() {
            None => return (),
            Some('"') => true,
            Some(c) => {
                self.buffer(c);
                false
            }
        };

        while let Some(c) = self.next_char_only() {
            if let '"' = c {
                if quoted {
                    result.push(buf.clone());
                    buf.clear();
                    break;
                }
            }

            if pathutil::separator(c) {
                result.push(buf.clone());
                buf.clear();
            } else if !pathutil::valid_segment_character(c) {
                result.push(buf.clone());
                buf.clear();
                self.buffer(c);
                break;
            } else {
                buf.push(c);
            }
        }

        self.yield_single(co, Token::Filepath(result), row, col)
            .await;
    }

    fn ident(&mut self, c: char) -> Token {
        self.buffer(c);

        Token::Ident(self.take_while(char::is_alphanumeric))
    }

    fn number_base(&mut self, base: u32) -> Token {
        let buffer = self.take_while(char::is_alphanumeric);

        let result: Result<u64, _> = u64::from_str_radix(&buffer, base);

        match result {
            Err(_) => Token::Error(LexError::InvalidBaseNum {
                s: buffer,
                base: 10,
            }),
            Ok(n) if n > std::u32::MAX as u64 => Token::Error(LexError::Overflow { n }),
            Ok(n) => Token::Number(n as u32),
        }
    }

    fn hexadecimal(&mut self) -> Token {
        self.number_base(16)
    }

    fn binary(&mut self) -> Token {
        self.number_base(2)
    }

    fn decimal(&mut self) -> Token {
        self.number_base(10)
    }

    fn quoted_string(&mut self) -> Token {
        let mut result = "\"".to_string();

        while let Some(c) = self.next_char_only() {
            match c {
                '"' => {
                    result.push('"');
                    match snailquote::unescape(result.as_str()) {
                        Ok(s) => return Token::QuotedString(s),
                        Err(e) => {
                            return Token::Error(LexError::Unescape(format!("{}", e)))
                        }
                    }
                }
                '\\' => match self.next_char_only() {
                    Some(c) => {
                        result.push('\\');
                        result.push(c);
                    }
                    None => {
                        return Token::Error(LexError::UnclosedQuote);
                    }
                },
                '\n' => {
                    return Token::Error(LexError::UnclosedQuote);
                }
                _ => result.push(c),
            }
        }

        Token::Error(LexError::UnclosedQuote)
    }
}

// This entire song and dance is effectively to strip escaped newlines from
// the input stream without compromising the file position.
impl<I> Iterator for FilePosStream<I>
where
    I: Iterator<Item = char>,
{
    type Item = FilePosAnnot<char>;

    fn next(&mut self) -> Option<Self::Item> {
        // XXX: This is just structured really weirdly
        loop {
            let mut exited_line_comment = false;
            while self.in_line_comment {
                if let '\n' = self.advance()?.copy_value() {
                    self.in_line_comment = false;
                    exited_line_comment = true;
                }
            }

            if exited_line_comment {
                return Some(self.annot('\n'));
            }

            while self.comment_nesting > 0 {
                match self.advance()?.copy_value() {
                    '*' => match self.advance()?.copy_value() {
                        '/' => self.comment_nesting -= 1,
                        _ => (),
                    },
                    '/' => match self.advance()?.copy_value() {
                        '/' => self.in_line_comment = true,
                        '*' => self.comment_nesting += 1,
                        _ => (),
                    },
                    _ => (),
                }
            }

            let c = self.advance()?;

            match c.copy_value() {
                '/' => match self.advance().map(|v| v.copy_value()) {
                    None => return Some(c),
                    Some('/') => self.in_line_comment = true,
                    Some('*') => self.comment_nesting += 1,
                    Some(c2) => {
                        self.buffer(c2);
                        return Some(c);
                    }
                },
                _ => return Some(c),
            }
        }
    }
}

pub fn lex(
    fname: String,
    stream: impl Iterator<Item = char>,
) -> Gen<TokenAnnot, (), impl std::future::Future<Output = ()>> {
    let mut stream = FilePosStream::new(fname, stream);

    Gen::new(|co| async move {
        while let Some(FilePosAnnot {
            value,
            fname: _,
            row,
            col,
        }) = stream.next()
        {
            if value.is_whitespace() && value != '\n' {
                continue;
            }

            stream.dispatch(&co, value, row, col).await;
        }
    })
}
