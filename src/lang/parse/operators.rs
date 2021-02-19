use std::{cmp::Ordering, collections::HashMap};

use lazy_static::lazy_static;

use super::super::{
    syntax::{Operator, Token},
    token,
};

pub(super) struct Precedence {
    precedence: u32,
    op: Operator,
}

impl Ord for Precedence {
    fn cmp(&self, other: &Precedence) -> Ordering {
        self.precedence.cmp(&other.precedence)
    }
}
impl Eq for Precedence {}

impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Precedence) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Precedence {
    fn eq(&self, other: &Precedence) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

macro_rules! dictionary {
    { $($key:expr => $value:expr,)* } => {
        vec![$(($key, $value),)*].into_iter().collect()
    }
}

// One day, we'll have real static evaluation...
lazy_static! {
    pub(super) static ref OPERATOR_MAP: HashMap<Token, Precedence> = {
        use token::Token::*;
        dictionary! {
            Bar => Precedence { precedence: 3, op: Operator::LOr, },
            Caret => Precedence { precedence: 4, op: Operator::LXor },
            Ampersand => Precedence { precedence: 5, op: Operator::LAnd },
            RShift => Precedence { precedence: 6, op: Operator::RShift },
            LShift => Precedence { precedence: 6, op: Operator::LShift },
            Plus => Precedence { precedence: 7, op: Operator::Add },
            Dash => Precedence { precedence: 7, op: Operator::Sub },
            Star => Precedence { precedence: 8, op: Operator::Mul },
            Slash => Precedence { precedence: 8, op: Operator::Div },
        }
    };
}
