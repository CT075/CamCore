use std::collections::HashMap;

use thiserror::Error;

use super::{
    preprocess::syntax as pps,
    syntax,
    syntax::{Node, NodeAnnot, Token, TokenAnnot},
    token::FilePosAnnot,
};
use crate::types::PriorityStack;

mod operators;
mod stream;

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum Error {
    #[error("macro {macro_:?} has no body, but is expanded here")]
    EmptyExpand { macro_: String },
    #[error("macro {name:?} is defined twice")]
    DuplicateMacro { name: String },
    #[error("macro {name:?} is `#undef`d, but is not currently defined")]
    UndefNoSuchDefn { name: String },
    #[error("the `#pool` macro is unsupported")]
    PoolUnsupported,
    #[error("unknown syntax error")]
    Malformed,
}

type ErrorAnnot = FilePosAnnot<Error>;

pub struct Context {
    definitions: HashMap<String, pps::Definition>,
    errors: Vec<ErrorAnnot>,
}

impl Context {
    fn push_error(&mut self, err: Error, row: usize, col: usize) -> () {
        self.errors.push(FilePosAnnot {
            value: err,
            row,
            col,
        })
    }

    fn lookup_definition(&self, s: &String) -> Option<&pps::Definition> {
        self.definitions.get(s)
    }

    // We take `dst` as a parameter to avoid an extra allocation in the `IfDef`
    // case.
    fn to_nodes(&mut self, tree: &pps::Ast, dst: &mut Vec<NodeAnnot>) -> () {
        use pps::Statement::*;

        for statement in tree {
            let FilePosAnnot {
                value: stmt,
                row,
                col,
            } = statement;
            match stmt {
                Pool => self.push_error(Error::PoolUnsupported, *row, *col),
                Malformed => self.push_error(Error::Malformed, *row, *col),
                Tokens(ts) => self.parse_line(ts, dst),
                Undef(s) => {
                    if let None = self.definitions.remove(s) {
                        self.push_error(
                            Error::UndefNoSuchDefn { name: s.clone() },
                            *row,
                            *col,
                        );
                    }
                }
                Define(s, d) => {
                    if let Some(_) = self.definitions.insert(s.clone(), d.clone()) {
                        self.push_error(
                            Error::DuplicateMacro { name: s.clone() },
                            *row,
                            *col,
                        );
                    }
                }
                IfDef(s, then, else_) => self.to_nodes(
                    if self.definitions.contains_key(s) {
                        then
                    } else {
                        else_
                    },
                    dst,
                ),
            }
        }
    }

    fn parse_line(&mut self, tokens: &Vec<TokenAnnot>, dst: &mut Vec<NodeAnnot>) -> () {
        let mut stream = tokens.iter();
        while let Some(FilePosAnnot { value, row, col }) = stream.next() {
            if matches!(value, Token::Semi | Token::Break) {
                continue;
            }

            let head = value;
        }
    }
}

/*
fn parse_operator(input: InStream) -> IResult<InStream, &OperatorPrec> {
    for symbol in OPERATORS.keys() {
        let (input, _) = consume_spaces(input)?;
        let r: IResult<_, _> = tag(*symbol)(input);
        if let Ok((input, _)) = r {
            return Ok((input, OPERATORS.get(*symbol).unwrap()));
        }
    }
    Err(nom::Err::Error((input, nom::error::ErrorKind::Alt)))
}
*/

// This is actually modified from the traditional shunting yard algorithm.
// Because we're outputting a syntax tree instead of a postfix stream, we use
// a stack to hold output tokens, where the top two elements of the stack are
// the right and left children of the next operator node.
/*
fn shunting_yard(input: InStream) -> IResult<InStream, ParsedExpr> {
    let mut operator_stack: PriorityStack<&OperatorPrec> = PriorityStack::new();
    let mut output_stack: Vec<ParsedExpr> = Vec::new();

    let mut input = input;

    loop {
        let (input_, _) = consume_spaces(input)?;
        let (input_, tok) = atom(input_)?;
        input = input_;

        output_stack.push(tok);
        if let Ok((input_, opprec)) = parse_operator(input) {
            input = input_;
            while let Some(OperatorPrec { precedence, op }) = operator_stack.peek() {
                if precedence < &opprec.precedence {
                    break;
                }
                operator_stack.pop();
                if let (Some(rhs), Some(lhs)) = (output_stack.pop(), output_stack.pop())
                {
                    output_stack.push(op(Box::new(lhs), Box::new(rhs)));
                } else {
                    return Err(nom::Err::Error((input, nom::error::ErrorKind::Alt)));
                }
            }

            operator_stack.push(opprec);
        } else {
            break;
        }
    }

    for OperatorPrec { precedence: _, op } in
        operator_stack.into_sorted_vec().into_iter().rev()
    {
        if let (Some(rhs), Some(lhs)) = (output_stack.pop(), output_stack.pop()) {
            output_stack.push(op(Box::new(lhs), Box::new(rhs)));
        } else {
            return Err(nom::Err::Error((input, nom::error::ErrorKind::Alt)));
        }
    }

    match output_stack.pop() {
        Some(result) if output_stack.len() == 0 => Ok((input, result)),
        _ => Err(nom::Err::Error((input, nom::error::ErrorKind::Alt))),
    }
}
*/
