use chumsky::{error::Error as ChumskyError, prelude::*};

use crate::lang::{
    syntax,
    syntax::{
        span::{Position, SpannedW},
        Expr, Operator, Span, Spanned, Token,
    },
};

use super::common::GenericParseErrorHandler;

#[cfg(test)]
mod tests;

pub type Argument = syntax::Argument<SpannedW>;
pub type Statement = syntax::Statement<SpannedW>;
pub type Event = syntax::Event<SpannedW>;

enum W {}

type Carrier<E> = super::common::Carrier<Token, E, W>;

pub trait ErrorHandler: GenericParseErrorHandler<Token> + 'static {
    fn bad_number(span: Span) -> Self;

    fn empty_parens(span: Span) -> Self;

    fn empty_list(span: Span) -> Self;
}

impl<E> ChumskyError<Token> for Carrier<E>
where
    E: ErrorHandler,
{
    type Span = Span;
    type Label = std::convert::Infallible;

    fn expected_input_found<I>(
        span: Self::Span,
        expected: I,
        found: Option<Token>,
    ) -> Self
    where
        I: IntoIterator<Item = Option<Token>>,
    {
        Self::generic_parse_error(span, expected, found)
    }

    fn unclosed_delimiter(
        unclosed_span: Self::Span,
        unclosed: Token,
        span: Self::Span,
        expected: Token,
        found: Option<Token>,
    ) -> Self {
        Self::unclosed_delimiter_impl(
            unclosed_span,
            unclosed,
            span,
            expected,
            found,
        )
    }

    fn merge(self, other: Self) -> Self {
        Self::merge_impl(self, other)
    }

    fn with_label(self, label: Self::Label) -> Self {
        match label {}
    }
}

fn literal<E>() -> impl Parser<Token, i32, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    let number = select! {
        Token::Number { payload, radix } => (payload, radix)
    };

    just(Token::Dash).or_not().then(number).try_map(
        |(neg, (payload, radix)), span: Span| {
            i32::from_str_radix(payload.as_str(), radix as u32)
                .map_err(move |_| Carrier::Specific(E::bad_number(span)))
                .map(|i| match neg {
                    Some(_) => -i,
                    None => i,
                })
        },
    )
}

fn val<E>() -> impl Parser<Token, String, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    select! {
        Token::Ident(s) => s
    }
}

fn items<E>(
    expr: impl Parser<Token, Spanned<Expr>, Error = Carrier<E>> + Clone,
    left: Token,
    right: Token,
) -> impl Parser<Token, Vec<Spanned<Expr>>, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    expr.clone()
        .separated_by(just(Token::Comma))
        .delimited_by(just(left), just(right))
}

fn atom<E>(
    expr: impl Parser<Token, Spanned<Expr>, Error = Carrier<E>> + Clone,
) -> impl Parser<Token, Expr, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    // XXX: It might be nice to try to [recover_with(nested_delimiters)] here,
    // but we'd need to thread an error variant through [Expr], and I'm not
    // sure it's worth it. All we'd get out of it is the ability to report
    // multiple errors per line if parsing fails inside a delimiter pair.
    choice((
        literal().map(Expr::Literal),
        val().map(Expr::Var),
        expr.clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map(|(e, _span)| e),
    ))
}

fn make_binop_parser<E>(
    op: impl Parser<Token, Operator, Error = Carrier<E>> + Clone,
    unit: impl Parser<Token, Expr, Error = Carrier<E>> + Clone,
) -> impl Parser<Token, Expr, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    unit.clone()
        .then(op.then(unit).repeated())
        .foldl(|a, (op, b)| Expr::Binop(op, Box::new(a), Box::new(b)))
}

fn expr<E>() -> impl Parser<Token, Spanned<Expr>, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    recursive(|expr| {
        let mul_op = choice((
            just(Token::Star).to(Operator::Mul),
            just(Token::Slash).to(Operator::Div),
            just(Token::Percent).to(Operator::Mod),
        ));

        let product = make_binop_parser(mul_op, atom(expr));

        let sum_op = choice((
            just(Token::Plus).to(Operator::Add),
            just(Token::Dash).to(Operator::Minus),
        ));

        let sum = make_binop_parser(sum_op, product);

        let shift_op = choice((
            just(Token::LShift).to(Operator::ShiftLeft),
            just(Token::RShift).to(Operator::ShiftRight),
        ));

        let shift = make_binop_parser(shift_op, sum);

        let and =
            make_binop_parser(just(Token::Ampersand).to(Operator::And), shift);

        let xor = make_binop_parser(just(Token::Caret).to(Operator::Xor), and);

        let or = make_binop_parser(just(Token::Bar).to(Operator::Or), xor);

        or.map_with_span(|e, span| (e, span))
    })
}

fn argument<E>() -> impl Parser<Token, Argument, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    let expr = expr();

    let paren_wrapped = items(expr.clone(), Token::LParen, Token::RParen)
        .try_map(|mut es, span| match es.len() {
            0 => Err(Carrier::Specific(E::empty_parens(span))),
            1 => Ok(Argument::Single(es.pop().map(|(e, _span)| e).unwrap())),
            _ => Ok(Argument::Tuple(es)),
        });

    let list = items(expr.clone(), Token::LBrace, Token::RBrace).try_map(
        |es, span| match es.len() {
            0 => Err(Carrier::Specific(E::empty_list(span))),
            _ => Ok(Argument::List(es)),
        },
    );

    // It's important to check [expr] before [paren_wrapped]. If we do the
    // reverse, then we'll run into issues with lines like
    //
    //   h (a + b) + c
    //
    // where the text [(a+b)] is treated as a 1-tuple instead of an atom
    // expression, causing a parse error on the following `+`.
    choice((
        expr.map(|(e, _span)| Argument::Single(e)),
        paren_wrapped,
        list,
    ))
}

fn label<E>() -> impl Parser<Token, Statement, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    (select! { Token::Ident(s) => s })
        .then_ignore(just(Token::Colon))
        .map(|s| Statement::Label(s))
        // TODO: label this so we can properly report "junk at end of line"
        .then_ignore(end())
}

fn instruction<E>() -> impl Parser<Token, Statement, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    (select! { Token::Ident(s) => s })
        .then(argument().map_with_span(|a, span| (a, span)).repeated())
        .map(|(head, args)| Statement::Instruction { head, args })
}

fn line<E>(
) -> impl Parser<Token, Vec<Spanned<Event>>, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    label()
        .map_with_span(|l, span| vec![(Event::Statement(l), span)])
        .or(instruction()
            .map(Event::Statement)
            .or(just(Token::LCurly).to(Event::OpenScope))
            .or(just(Token::RCurly).to(Event::CloseScope))
            .map_with_span(|l, span| (l, span))
            .separated_by(just(Token::Semi))
            .then_ignore(end()))
}

pub fn parse_line<E>(
    tokens: Vec<Spanned<Token>>,
) -> Result<Vec<Spanned<Event>>, Vec<E>>
where
    E: ErrorHandler,
{
    let eoi = match tokens.last() {
        None => return Ok(vec![]),
        Some((_, Span { source, span })) => {
            let Position { offset, .. } = span.start;

            Span {
                source: source.clone(),
                span: Position {
                    offset: offset + 1,
                    row: 0,
                    col: 0,
                }..Position {
                    offset: offset + 1,
                    row: 0,
                    col: 0,
                },
            }
        }
    };

    line()
        .parse(chumsky::stream::Stream::from_iter(eoi, tokens.into_iter()))
        .map_err(|errs| errs.into_iter().map(Carrier::into).collect())
}
