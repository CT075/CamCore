use std::collections::{HashMap, VecDeque};
use std::convert::From;

use indexmap::set::IndexSet;
use thiserror::Error;

use super::{
    lex::{LexError, Token as LexToken, TokenAnnot as LexTokenAnnot},
    token,
    token::{Directive, FilePosAnnot, Token::*},
};

#[cfg(test)]
mod tests;

#[derive(Error, Debug, Clone)]
pub enum PreprocError {
    #[error("file ended unexpectedly (hint: {hint:?})")]
    UnexpectedEof { hint: String },
    #[error("arguments to macros should be surrounded by parentheses")]
    ExpectLParen,
    #[error("macro cycle encountered")]
    MacroCycle { cycle: Vec<String> },
    #[error("macro {macro_:?} has no body, but is expanded here")]
    EmptyExpand { macro_: String },
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
    ifstack: Vec<bool>,
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
            let cycle = self.expanded.drain(..).collect();
            return Some(VecDeque::from(vec![FilePosAnnot {
                value: Error(PreprocError::MacroCycle { cycle }),
                row: 0,
                col: 0,
                fname: format!("expansion of macro {} at {}", s, t.fname),
            }]));
        }

        // We can't use [lookup_definition] here, because that borrows the entirety
        // of [self] immutably, preventing us from using [self.stream]...
        let defn = self.defines.get(s);

        match defn {
            None => None,
            Some(Empty) => Some(VecDeque::from(vec![FilePosAnnot {
                value: Error(PreprocError::EmptyExpand { macro_: s.clone() }),
                row: t.row,
                col: t.col,
                fname: t.fname.clone(),
            }])),
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
                                fname: format!("end of file {}", t.fname),
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

    fn else_(&mut self) {}

    fn endif(&mut self) {}

    fn define(&mut self) {
        let s = match self.next().map(TokenAnnot::extract_value) {
            Some(Token::Ident(s)) => s,
            Some(_) => panic!("todo"),
            None => panic!("todo"),
        };
    }

    fn process_directive(&mut self, d: &Directive) {
        use Directive::*;
        match d {
            Define => self.define(),
            Include => panic!("todo"),
            Incbin => panic!("todo"),
            Incext => panic!("todo"),
            Inctevent => panic!("todo"),
            IfDef => panic!("todo"),
            IfNDef => panic!("todo"),
            Pool => panic!("todo"),
            Undef => panic!("todo"),
            Else => panic!("todo"),
            Endif => panic!("todo"),
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
            None => self.stream.next().map(TokenAnnot::from)?,
        };

        let if_state = self.ifstack.last().unwrap_or(&false);

        match (if_state, v.borrow_value()) {
            (false, Directive(Directive::Else)) => {
                self.else_();
                self.next()
            }
            (false, Directive(Directive::Endif)) => {
                self.endif();
                self.next()
            }
            (false, _) => self.next(),
            (true, Directive(d)) => {
                self.process_directive(d);
                self.next()
            }
            (true, Ident(s)) => match self.attempt_expand(&v) {
                None => Some(v),
                Some(results) => {
                    self.buffer.push(results);
                    self.expanded.insert(s.clone());
                    self.next()
                }
            },
            (true, _) => Some(TokenAnnot::from(v)),
        }
    }
}
