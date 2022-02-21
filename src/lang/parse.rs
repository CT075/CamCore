use std::collections::{HashMap, HashSet};

use thiserror::Error;

use super::{
    preprocess::syntax as pps,
    syntax,
    syntax::{Node, NodeAnnot, Token, TokenAnnot},
};
use crate::{
    location::{FilePosAnnot, Source, SourceAnnot},
    types::PriorityStack,
};

mod operators;

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

type ErrorAnnot<'a> = SourceAnnot<'a, Error>;

pub struct Context<'a> {
    definitions: HashMap<String, pps::Definition>,
    errors: Vec<ErrorAnnot<'a>>,
    output: Vec<NodeAnnot<'a>>,
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
