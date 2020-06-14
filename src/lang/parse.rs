use crate::types::generic::PriorityStack;
use crate::types::*;
use ast::*;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::complete::{alpha1, char, digit1, hex_digit1, line_ending, one_of},
    combinator::{map, opt},
    multi::{many0, separated_list},
    sequence::{delimited, preceded, terminated},
    IResult,
};

use phf::phf_map;
use std::cmp::Ordering;

pub mod ast;

type InStream<'a> = &'a str;

struct OperatorPrec {
    precedence: u32,
    op: fn(Box<ParsedExpr>, Box<ParsedExpr>) -> ParsedExpr,
}

impl Ord for OperatorPrec {
    fn cmp(&self, other: &OperatorPrec) -> Ordering {
        self.precedence.cmp(&other.precedence)
    }
}
impl Eq for OperatorPrec {}

impl PartialOrd for OperatorPrec {
    fn partial_cmp(&self, other: &OperatorPrec) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for OperatorPrec {
    fn eq(&self, other: &OperatorPrec) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

static OPERATORS: phf::Map<&'static str, OperatorPrec> = phf_map! {
    "|" =>  OperatorPrec { precedence:3, op:ParsedExpr::LOr },
    "^" =>  OperatorPrec { precedence:4, op:ParsedExpr::LXor },
    "&" =>  OperatorPrec { precedence:5, op:ParsedExpr::LAnd },
    ">>" => OperatorPrec { precedence:6, op:ParsedExpr::Rshift },
    "<<" => OperatorPrec { precedence:6, op:ParsedExpr::Lshift },
    "+" =>  OperatorPrec { precedence:7, op:ParsedExpr::Add },
    "-" =>  OperatorPrec { precedence:7, op:ParsedExpr::Sub },
    "*" =>  OperatorPrec { precedence:8, op:ParsedExpr::Mul },
    "/" =>  OperatorPrec { precedence:8, op:ParsedExpr::Div },
};

// for future-proofing if we decide to change the stream type
pub fn input_stream<'a>(s: &'a str) -> InStream<'a> {
    s
}

pub fn parse_expr(s: &str) -> IResult<InStream, ParsedExpr> {
    let input = input_stream(s);
    let (input, result) = expr(input)?;
    Ok((input, result))
}

// It's surprisingly annoying getting nom to eat exactly one input item.
fn letter(input: InStream) -> IResult<InStream, char> {
    one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")(input)
}

fn bin_digit1(input: InStream) -> IResult<InStream, &str> {
    take_while(|c| match c {
        '0' | '1' => true,
        _ => false,
    })(input)
}

fn number(input: InStream) -> IResult<InStream, u32> {
    let original_input = input;
    let (input, sign): (InStream, Option<i32>) =
        opt(alt((map(tag("+"), |_| 1), map(tag("-"), |_| -1))))(input)?;
    let sign = sign.unwrap_or(1);

    let (input, (digits, radix)) = alt((
        map(preceded(alt((tag("0x"), tag("$"))), hex_digit1), |s| {
            (s, 16)
        }),
        map(terminated(bin_digit1, alt((tag("b"), tag("B")))), |s| {
            (s, 2)
        }),
        map(digit1, |s| (s, 10)),
    ))(input)?;

    match u32::from_str_radix(digits, radix) {
        Ok(i) => Ok((input, i)),
        Err(_) => Err(nom::Err::Error((
            original_input,
            nom::error::ErrorKind::Digit,
        ))),
    }
}

fn consume_spaces(input: InStream) -> IResult<InStream, ()> {
    let (input, _) = take_while(|c: char| c.is_whitespace())(input)?;
    Ok((input, ()))
}

fn string(input: InStream) -> IResult<InStream, &str> {
    let (input, _) = char('"')(input)?;
    let (input, result) = take_until("\"")(input)?;
    let (input, _) = char('"')(input)?;

    Ok((input, result))
}

// "word" as in the [\w]+ sense, not necessarily the chunk-size sense
fn word(input: InStream) -> IResult<InStream, &str> {
    take_while(|c| match c {
        '_' => true,
        _ => c.is_alphanumeric(),
    })(input)
}

fn param(input: InStream) -> IResult<InStream, ParamNode> {
    alt((
        map(expr, ParamNode::Expr),
        map(string, |s| ParamNode::Str(String::from(s))),
        map(
            delimited(
                char('('),
                separated_list(preceded(consume_spaces, char(',')), expr),
                preceded(consume_spaces, char(')')),
            ),
            ParamNode::List,
        ),
    ))(input)
}

fn ea_macro(input: InStream) -> IResult<InStream, GenericMacro> {
    let (input, _) = consume_spaces(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = char('(')(input)?;

    let (input, args) =
        separated_list(preceded(consume_spaces, tag(",")), param)(input)?;

    let (input, _) = consume_spaces(input)?;
    let (input, _) = char(')')(input)?;

    Ok((input, GenericMacro { name, args }))
}

fn preprocessor_param(input: InStream) -> IResult<InStream, String> {
    let (input, _) = consume_spaces(input)?;
    map(alt((string, word)), String::from)(input)
}

fn preprocessor_command(input: InStream) -> IResult<InStream, GenericDirective> {
    let (input, _) = char('#')(input)?;
    let (input, cmd) = map(alpha1, String::from)(input)?;
    let (input, args) = many0(preprocessor_param)(input)?;

    Ok((input, GenericDirective { cmd, args }))
}

fn identifier(input: InStream) -> IResult<InStream, Identifier> {
    let (input, _) = consume_spaces(input)?;
    let (input, fst) = letter(input)?;
    let (input, rest) = word(input)?;

    // I hate that this is what I have to do to append to the front
    let mut result = String::new();
    result.push(fst);
    result.push_str(rest);

    Ok((input, Identifier(result)))
}

fn label(input: InStream) -> IResult<InStream, Identifier> {
    let (input, _) = consume_spaces(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = consume_spaces(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = consume_spaces(input)?;
    Ok((input, name))
}

fn statement(input: InStream) -> IResult<InStream, ParsedInstr> {
    let (input, _) = consume_spaces(input)?;

    let (input, args) = many0(preceded(consume_spaces, param))(input)?;

    panic!("todo")
}

fn expr(input: InStream) -> IResult<InStream, ParsedExpr> {
    shunting_yard(input)
}

fn atom(input: InStream) -> IResult<InStream, ParsedExpr> {
    alt((
        map(ea_macro, ParsedExpr::Macro),
        map(identifier, ParsedExpr::Symbol),
        map(number, ParsedExpr::Number),
        delimited(tag("("), expr, tag(")")),
    ))(input)
}

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

// This is actually modified from the traditional shunting yard algorithm.
// Because we're outputting a syntax tree instead of a postfix stream, we use
// a stack to hold output tokens, where the top two elements of the stack are
// the right and left children of the next operator node.
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
