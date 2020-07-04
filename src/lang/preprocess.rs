use std::collections::{HashMap, HashSet, VecDeque};
use std::convert::From;

use thiserror::Error;

use super::lex::{LexError, Token as LexToken, TokenAnnot as LexTokenAnnot};
use super::{
    token,
    token::{FilePosAnnot, Token::*},
};

pub mod directive;

#[derive(Error, Debug, Clone)]
pub enum PreprocError {
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
    Rename(VecDeque<Token>),
    Macro(HashSet<String>, VecDeque<Token>),
}

pub struct State {
    ifstack: Vec<(usize, bool)>,
    defines: HashMap<String, Definition>,
    expanded: HashSet<String>,
}

impl State {
    fn lookup_definition(&self, s: &String) -> Option<&Definition> {
        self.defines.get(s)
    }
}

pub struct Preprocessor<I: Iterator> {
    stream: I,
    buffer: VecDeque<I::Item>,
    state: State,
}

impl<I> Preprocessor<I>
where
    I: Iterator<Item = LexTokenAnnot>,
{
    pub fn attempt_expand(
        &mut self,
        t: &LexTokenAnnot,
    ) -> Option<VecDeque<TokenAnnot>> {
        use Definition::*;

        let s = match t.borrow_value() {
            Ident(s) => s,
            _ => panic!("BUG: called attempt_expand on non-identifier"),
        };

        if self.state.expanded.get(s).is_some() {
            return None;
        }

        self.state.lookup_definition(s).and_then(|defn| match defn {
            // TODO: insert warning for expanded empty definition
            Empty => None,
            Rename(vs) => Some(
                vs.clone()
                    .into_iter()
                    .map(|value| {
                        TokenAnnot::annot(
                            value.clone(),
                            format!("expansion of definition {}", s),
                            t.row,
                            t.col,
                        )
                    })
                    .collect(),
            ),
            Macro(args, body) => panic!("todo"),
        })
    }
}

impl<I> Iterator for Preprocessor<I>
where
    I: Iterator<Item = LexTokenAnnot>,
{
    type Item = TokenAnnot;

    fn next(&mut self) -> Option<Self::Item> {
        let v = match self.buffer.pop_front() {
            Some(v) => v,
            None => self.stream.next()?,
        };

        match v.borrow_value() {
            Directive(d) => panic!("todo"),
            Ident(_) => match self.attempt_expand(&v) {
                None => Some(TokenAnnot::from(v)),
                Some(results) => panic!("todo"),
            },
            _ => Some(TokenAnnot::from(v)),
        }
    }
}
