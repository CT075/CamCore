// TODO: Add more tests here

use std::collections::HashSet;

use super::*;
use crate::lang::{
    parse::common::GenericParseErrorHandler,
    syntax::span::{Position, Source},
};

// [rustc] invariably gets oomkilled when I include this file for reasons I
// don't at all understand. As much as I'd love to delve into that further,
// it's probably simpler to just avoid it.
//mod quickcheck_tests;

#[derive(PartialEq, Eq, Debug)]
enum Error {
    Unlabeled(Span, HashSet<Option<Token>>, Option<Token>),
    BadNumber(Span),
    EmptyParens(Span),
    EmptyList(Span),
}

impl GenericParseErrorHandler<Token> for Error {
    fn expected(
        span: Span,
        expected: HashSet<Option<Token>>,
        found: Option<Token>,
    ) -> Self {
        Self::Unlabeled(span, expected, found)
    }

    fn unclosed_delimiter(
        span: Span,
        _: Token,
        expected: Token,
        found: Option<Token>,
    ) -> Self {
        let mut set = HashSet::new();

        set.insert(Some(expected));

        Self::Unlabeled(span, set, found)
    }
}

impl super::ErrorHandler for Error {
    fn bad_number(span: Span) -> Self {
        Self::BadNumber(span)
    }
    fn empty_parens(span: Span) -> Self {
        Self::EmptyParens(span)
    }
    fn empty_list(span: Span) -> Self {
        Self::EmptyList(span)
    }
}

#[test]
fn basic_statement() {
    let span = Span {
        source: Source::Unknown,
        span: Position {
            offset: 0,
            row: 0,
            col: 0,
        }..Position {
            offset: 0,
            row: 0,
            col: 0,
        },
    };

    let expected = Statement::Instruction {
        head: Rc::new("t".to_owned()),
        args: vec![(
            Argument::Single(Expr::Binop(
                Operator::Or,
                Box::new(Expr::Binop(
                    Operator::Xor,
                    Box::new(Expr::Var(Rc::new("X".to_owned()))),
                    Box::new(Expr::Var(Rc::new("g".to_owned()))),
                )),
                Box::new(Expr::Var(Rc::new("v".to_owned()))),
            )),
            span.clone(),
        )],
    };

    // t (X ^ g) | v
    let actual: Result<_, Vec<Error>> = parse_line(vec![
        (Token::Ident(Rc::new("t".to_owned())), span.clone()),
        (Token::LParen, span.clone()),
        (Token::Ident(Rc::new("X".to_owned())), span.clone()),
        (Token::Caret, span.clone()),
        (Token::Ident(Rc::new("g".to_owned())), span.clone()),
        (Token::RParen, span.clone()),
        (Token::Bar, span.clone()),
        (Token::Ident(Rc::new("v".to_owned())), span.clone()),
    ]);

    assert_eq!(actual, Ok(vec![(expected, span.clone())]))
}
