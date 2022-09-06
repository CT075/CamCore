// TODO: Add more tests here

use std::collections::HashSet;

use super::*;
use crate::{
    lang::{
        parse::common::GenericParseErrorHandler,
        {
            syntax,
            syntax::span::{Position, Source},
        },
    },
    plumbing::*,
    types::hkt::IdentityW,
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

const BASIC_SPAN: Span = Span {
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

type UnspannedArgument = syntax::Argument<IdentityW>;
type UnspannedStatement = syntax::Statement<IdentityW>;

fn strip_span_arg(arg: Argument) -> UnspannedArgument {
    match arg {
        Argument::Single(e) => UnspannedArgument::Single(e),
        Argument::List(es) => {
            UnspannedArgument::List(es.into_iter().map(fst).collect())
        }
        Argument::Tuple(es) => {
            UnspannedArgument::Tuple(es.into_iter().map(fst).collect())
        }
    }
}

fn strip_span_stmt(stmt: Statement) -> UnspannedStatement {
    match stmt {
        Statement::Label(s) => UnspannedStatement::Label(s),
        Statement::Instruction { head, args } => {
            UnspannedStatement::Instruction {
                head,
                args: args
                    .into_iter()
                    .map(|(arg, _span)| strip_span_arg(arg))
                    .collect(),
            }
        }
    }
}

fn parse_line_no_span(
    toks: Vec<Token>,
) -> Result<Vec<UnspannedStatement>, Vec<Error>> {
    super::parse_line(
        toks.into_iter().map(|x| (x, BASIC_SPAN.clone())).collect(),
    )
    .map(|stmts| {
        stmts
            .into_iter()
            .map(|(stmt, _span)| strip_span_stmt(stmt))
            .collect()
    })
}

#[test]
fn basic_statement() {
    let expected = UnspannedStatement::Instruction {
        head: Rc::new("t".to_owned()),
        args: vec![
            (UnspannedArgument::Single(Expr::Binop(
                Operator::Or,
                Box::new(Expr::Binop(
                    Operator::Xor,
                    Box::new(Expr::Var(Rc::new("X".to_owned()))),
                    Box::new(Expr::Var(Rc::new("g".to_owned()))),
                )),
                Box::new(Expr::Var(Rc::new("v".to_owned()))),
            ))),
        ],
    };

    // t (X ^ g) | v
    let actual: Result<_, Vec<Error>> = parse_line_no_span(vec![
        Token::Ident(Rc::new("t".to_owned())),
        Token::LParen,
        Token::Ident(Rc::new("X".to_owned())),
        Token::Caret,
        Token::Ident(Rc::new("g".to_owned())),
        Token::RParen,
        Token::Bar,
        Token::Ident(Rc::new("v".to_owned())),
    ]);

    assert_eq!(actual, Ok(vec![expected]))
}

#[test]
fn tuple_arg() {
    let expected = UnspannedStatement::Instruction {
        head: Rc::new("t".to_owned()),
        args: vec![
            UnspannedArgument::Tuple(vec![
                Expr::Binop(
                    Operator::Or,
                    Box::new(Expr::Binop(
                        Operator::Xor,
                        Box::new(Expr::Var(Rc::new("X".to_owned()))),
                        Box::new(Expr::Var(Rc::new("g".to_owned()))),
                    )),
                    Box::new(Expr::Var(Rc::new("v".to_owned()))),
                ),
                Expr::Var(Rc::new("q".to_owned())),
            ]),
            UnspannedArgument::Single(Expr::Binop(
                Operator::Add,
                Box::new(Expr::Binop(
                    Operator::Add,
                    Box::new(Expr::Var(Rc::new("a".to_owned()))),
                    Box::new(Expr::Var(Rc::new("b".to_owned()))),
                )),
                Box::new(Expr::Var(Rc::new("c".to_owned()))),
            )),
            UnspannedArgument::Single(Expr::Binop(
                Operator::Add,
                Box::new(Expr::Var(Rc::new("d".to_owned()))),
                Box::new(Expr::Var(Rc::new("e".to_owned()))),
            )),
        ],
    };

    // t (X ^ g | v, q) (a + b) + c (d + e)
    let actual: Result<_, Vec<Error>> = parse_line_no_span(vec![
        Token::Ident(Rc::new("t".to_owned())),
        Token::LParen,
        Token::Ident(Rc::new("X".to_owned())),
        Token::Caret,
        Token::Ident(Rc::new("g".to_owned())),
        Token::Bar,
        Token::Ident(Rc::new("v".to_owned())),
        Token::Comma,
        Token::Ident(Rc::new("q".to_owned())),
        Token::RParen,
        Token::LParen,
        Token::Ident(Rc::new("a".to_owned())),
        Token::Plus,
        Token::Ident(Rc::new("b".to_owned())),
        Token::RParen,
        Token::Plus,
        Token::Ident(Rc::new("c".to_owned())),
        Token::LParen,
        Token::Ident(Rc::new("d".to_owned())),
        Token::Plus,
        Token::Ident(Rc::new("e".to_owned())),
        Token::RParen,
    ]);

    assert_eq!(actual, Ok(vec![expected]))
}
