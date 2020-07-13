use std::collections::{HashMap, VecDeque};
use std::convert::From;

use indexmap::set::IndexSet;
use thiserror::Error;

use super::{
    lex::{LexError, Token as LexToken, TokenAnnot as LexTokenAnnot},
    token,
    token::{FilePosAnnot, Token::*},
};

#[cfg(test)]
mod tests;

#[derive(Error, Debug, Clone)]
pub enum PreprocError {
    #[error("file ended unexpectedly ({hint:?})")]
    UnexpectedEof { hint: String },
    #[error("arguments to macros should be surrounded by parentheses")]
    ExpectLParen,
    #[error(transparent)]
    LexError(LexError),
}

pub type Token = token::Token<PreprocError>;
pub type TokenAnnot = FilePosAnnot<Token>;

impl From<LexToken> for Token {
    fn from(t: LexToken) -> Self {
        t.map(PreprocError::LexError)
    }
}

impl From<LexTokenAnnot> for TokenAnnot {
    fn from(t: LexTokenAnnot) -> Self {
        t.map(Token::from)
    }
}

pub enum Definition {
    Empty,
    Rename(Vec<Token>),
    Macro(IndexSet<String>, VecDeque<Token>),
}

pub struct Preprocessor<I: Iterator> {
    stream: I,
    buffer: Vec<VecDeque<TokenAnnot>>,
    ifstack: Vec<(usize, bool)>,
    defines: HashMap<String, Definition>,
    expanded: IndexSet<String>,
}

macro_rules! expect {
    ( $stream:expr, $p:pat, $b:block, $err:expr, $s:expr ) => {{
        match $stream.next() {
            Some(FilePosAnnot {
                value: $p,
                row,
                col,
                fname,
            }) => {
                // These are required to make the linter shut up about
                // `unused variables`.
                let _ = row;
                let _ = col;
                let _ = fname;
                $b
            }
            Some(t) => return Some(VecDeque::from(vec![t.map(|_| Error($err))])),
            None => {
                return Some(VecDeque::from(vec![FilePosAnnot {
                    value: Error(PreprocError::UnexpectedEof {
                        hint: format!("I was looking for a {}", $s),
                    }),
                    row: 0,
                    col: 0,
                    fname: "end of file".to_string(),
                }]))
            }
        }
    }};
}

impl<I> Preprocessor<I>
where
    I: Iterator<Item = LexTokenAnnot>,
{
    pub fn lookup_definition<'a, 'b>(&'a self, s: &'b str) -> Option<&'a Definition> {
        self.defines.get(s)
    }

    pub fn pop_buffer(&mut self) -> Option<TokenAnnot> {
        let v = self.buffer.last_mut()?;
        let result = v.pop_front();
        if v.is_empty() {
            self.buffer.pop();
            self.expanded.pop();
        }
        result
    }

    pub fn attempt_expand(&mut self, t: &TokenAnnot) -> Option<VecDeque<TokenAnnot>> {
        use Definition::*;
        use PreprocError::ExpectLParen;

        let s = match t.borrow_value() {
            Ident(s) => s,
            _ => panic!("BUG: called [attempt_expand] on non-identifier"),
        };

        if self.expanded.get(s).is_some() {
            return None;
        }

        // We can't use [lookup_definition] here, because that borrows the entirety
        // of [self] immutably, preventing us from using [self.stream]...
        let defn = self.defines.get(s);

        match defn {
            None => None,
            // TODO: insert warning for expanded empty definition
            Some(Empty) => Some(VecDeque::new()),
            Some(Rename(vs)) => Some(
                vs.clone()
                    .into_iter()
                    .map(|value| {
                        TokenAnnot::annot(
                            value.clone(),
                            format!("expansion of definition {} at {}", s, t.fname),
                            t.row,
                            t.col,
                        )
                    })
                    .collect(),
            ),
            Some(Macro(vars, body)) => {
                expect!(self.stream, LParen, {}, ExpectLParen, "(");
                let mut args = Vec::new();
                let mut buffer = Vec::new();
                loop {
                    match self.stream.next() {
                        Some(FilePosAnnot {
                            value: RParen,
                            row: _,
                            col: _,
                            fname: _,
                        }) => {
                            args.push(buffer);
                            break;
                        }
                        Some(FilePosAnnot {
                            value: Comma,
                            row: _,
                            col: _,
                            fname: _,
                        }) => {
                            args.push(buffer);
                            buffer = Vec::new();
                        }
                        Some(FilePosAnnot {
                            value: Break,
                            row: _,
                            col: _,
                            fname: _,
                        }) => (),
                        Some(FilePosAnnot {
                            value,
                            row: _,
                            col: _,
                            fname: _,
                        }) => buffer.push(FilePosAnnot {
                            value: Token::from(value),
                            fname: format!("expansion of macro {} at {}", s, t.fname),
                            row: t.row,
                            col: t.col,
                        }),
                        None => {
                            return Some(VecDeque::from(vec![FilePosAnnot {
                                value: Error(PreprocError::UnexpectedEof {
                                    hint: "are there unclosed parentheses?".to_string(),
                                }),
                                row: 0,
                                col: 0,
                                fname: "end of file".to_string(),
                            }]))
                        }
                    }
                }

                let args: HashMap<String, Vec<TokenAnnot>> =
                    vars.clone().into_iter().zip(args.into_iter()).collect();

                Some(
                    body.clone()
                        .into_iter()
                        .flat_map(|value| match value {
                            Ident(s) => match args.get(&s) {
                                Some(vs) => vs.clone(),
                                None => vec![FilePosAnnot {
                                    value: Ident(s.clone()),
                                    row: t.row,
                                    col: t.col,
                                    fname: format!(
                                        "expansion of macro {} at {}",
                                        s, t.fname
                                    ),
                                }],
                            },
                            value => vec![FilePosAnnot {
                                value,
                                row: t.row,
                                col: t.col,
                                fname: format!(
                                    "expansion of macro {} at {}",
                                    s, t.fname
                                ),
                            }],
                        })
                        .collect(),
                )
            }
        }
    }
}

impl<I> Iterator for Preprocessor<I>
where
    I: Iterator<Item = LexTokenAnnot>,
{
    type Item = TokenAnnot;

    fn next(&mut self) -> Option<Self::Item> {
        let v = match self.pop_buffer() {
            Some(v) => v,
            None => TokenAnnot::from(self.stream.next()?),
        };

        match v.borrow_value() {
            Directive(d) => panic!("todo"),
            Ident(s) => match self.attempt_expand(&v) {
                None => Some(v),
                Some(results) => {
                    self.buffer.push(results);
                    self.expanded.insert(s.clone());
                    self.next()
                }
            },
            _ => Some(TokenAnnot::from(v)),
        }
    }
}
