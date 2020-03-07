use crate::types::*;
use ast::*;
use nom::{
    alt,
    bytes::complete::{tag, take_until, take_while},
    char,
    character::complete::{alpha1, line_ending},
    complete, many0, map, one_of, preceded, tag, IResult,
};

pub mod ast;

#[cfg(test)]
pub mod tests;

type InStream<'a> = &'a str;

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
    one_of!(
        input,
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    )
}

fn consume_spaces(input: InStream) -> IResult<InStream, ()> {
    let (input, _) = take_while(|c: char| c.is_whitespace())(input)?;
    Ok((input, ()))
}

fn string(input: InStream) -> IResult<InStream, &str> {
    let (input, _) = char!(input, '"')?;
    let (input, result) = take_until("\"")(input)?;
    let (input, _) = char!(input, '"')?;

    Ok((input, result))
}

// "word" as in the [\w]+ sense, not necessarily the chunk-size sense
fn word(input: InStream) -> IResult<InStream, &str> {
    take_while(|c| match c {
        '_' => true,
        _ => c.is_alphanumeric(),
    })(input)
}

fn ea_macro(input: InStream) -> IResult<InStream, GenericMacro> {
    let (input, _) = consume_spaces(input)?;
    let (input, name) = identifier(input)?;
    let (input, _) = char!(input, '(')?;

    panic!("todo")
}

fn preprocessor_param(input: InStream) -> IResult<InStream, String> {
    let (input, _) = consume_spaces(input)?;
    map!(input, alt!(string | word), String::from)
}

fn preprocessor_command(input: InStream) -> IResult<InStream, GenericDirective> {
    let (input, _) = char!(input, '#')?;
    let (input, cmd) = map!(input, alpha1, String::from)?;
    let (input, args) = many0!(input, preprocessor_param)?;

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
    let (input, _) = char!(input, ':')?;
    let (input, _) = consume_spaces(input)?;
    let (input, _) = line_ending(input)?;
    Ok((input, name))
}

// XXX - I *really* need to clean these up. The expression parsing is a
// hacky disaster that is cobbled together by tears.
fn expr(input: InStream) -> IResult<InStream, ParsedExpr> {
    prec5(input)
}

fn parse_binop(
    input: InStream,
    op_chr: char,
    op_node: impl Fn(Box<ParsedExpr>, Box<ParsedExpr>) -> ParsedExpr,
    left: impl Fn(InStream) -> IResult<InStream, ParsedExpr>,
    right: impl Fn(InStream) -> IResult<InStream, ParsedExpr>,
) -> IResult<InStream, ParsedExpr> {
    let (input, l) = map!(input, left, Box::new)?;
    let (input, _) = consume_spaces(input)?;
    let (input, _) = char!(input, op_chr)?;
    let (input, _) = consume_spaces(input)?;
    let (input, r) = map!(input, right, Box::new)?;

    Ok((input, op_node(l, r)))
}

fn parse_left_recursive_binop(
    input: InStream,
    op_chr: impl Fn(
        InStream,
    )
        -> IResult<InStream, fn(Box<ParsedExpr>, Box<ParsedExpr>) -> ParsedExpr>,
    base: impl Fn(InStream) -> IResult<InStream, ParsedExpr>,
    right: impl Fn(InStream) -> IResult<InStream, ParsedExpr>,
) -> IResult<InStream, ParsedExpr> {
    let (input, b) = base(input)?;
    match preceded!(input, consume_spaces, op_chr) {
        Ok((input, op_node)) => {
            let (input, r) = preceded!(input, consume_spaces, right)?;
            Ok((input, op_node(Box::new(b), Box::new(r))))
        }
        _ => Ok((input, b)),
    }
}

fn atom(input: InStream) -> IResult<InStream, ParsedExpr> {
    alt!(
        input,
        map!(identifier, ParsedExpr::Symbol) | map!(ea_macro, ParsedExpr::Macro)
    )
}

pub fn prec0(input: InStream) -> IResult<InStream, ParsedExpr> {
    parse_left_recursive_binop(
        input,
        |input: InStream| {
            alt!(input,
                tag!("*") => { |_| ParsedExpr::Mul as
                    fn(Box<ParsedExpr>, Box<ParsedExpr>) -> ParsedExpr
                } |
                tag!("/") => { |_| ParsedExpr::Div as
                    fn(Box<ParsedExpr>, Box<ParsedExpr>) -> ParsedExpr
                }
            )
        },
        atom,
        prec0,
    )
}

fn prec1(input: InStream) -> IResult<InStream, ParsedExpr> {
    parse_left_recursive_binop(
        input,
        |input| {
            alt!(input,
                tag!("+") => { |_| ParsedExpr::Add as
                    fn(Box<ParsedExpr>, Box<ParsedExpr>) -> ParsedExpr
                } |
                tag!("-") => { |_| ParsedExpr::Sub as
                    fn(Box<ParsedExpr>, Box<ParsedExpr>) -> ParsedExpr
                }
            )
        },
        prec0,
        prec1,
    )
}

fn prec2(input: InStream) -> IResult<InStream, ParsedExpr> {
    parse_left_recursive_binop(
        input,
        |input| {
            alt!(input,
                tag!(">>") => { |_| ParsedExpr::Rshift as
                    fn(Box<ParsedExpr>, Box<ParsedExpr>) -> ParsedExpr
                } |
                tag!("<<") => { |_| ParsedExpr::Lshift as
                    fn(Box<ParsedExpr>, Box<ParsedExpr>) -> ParsedExpr
                }
            )
        },
        prec1,
        prec2,
    )
}

fn prec3(input: InStream) -> IResult<InStream, ParsedExpr> {
    fn internal_binop(input: InStream) -> IResult<InStream, ParsedExpr> {
        parse_binop(input, '&', ParsedExpr::LAnd, prec2, prec3)
    }

    alt!(input, complete!(internal_binop) | prec2)
}

fn prec4(input: InStream) -> IResult<InStream, ParsedExpr> {
    fn internal_binop(input: InStream) -> IResult<InStream, ParsedExpr> {
        parse_binop(input, '^', ParsedExpr::LXor, prec3, prec4)
    }

    alt!(input, complete!(internal_binop) | prec3)
}

fn prec5(input: InStream) -> IResult<InStream, ParsedExpr> {
    fn internal_binop(input: InStream) -> IResult<InStream, ParsedExpr> {
        parse_binop(input, '|', ParsedExpr::LOr, prec4, prec5)
    }

    alt!(input, complete!(internal_binop) | prec4)
}
