// In order to make sure that quickcheck generates interesting-but-correct
// inputs to be parsed, we first generate a correct statement, then render it
// while randomly inserting parentheses while respecting precedence.

use quickcheck::{Arbitrary, Gen};
use quickcheck_macros::quickcheck;

use super::*;
use crate::{
    lang::syntax::span::{Position, Source, Span},
    plumbing::*,
};

fn arbitrary_identifier(g: &mut Gen) -> Rc<String> {
    let size = {
        let s = g.size();
        // [Gen::gen_range] is inexplicably private, so we have to do this
        // instead
        *g.choose(&(1..s).collect::<Vec<_>>()).unwrap_or(&1)
    };
    let mut s = String::with_capacity(size);

    let first = *g
        .choose(&(65u8..=90u8).chain(97u8..=122u8).collect::<Vec<_>>())
        .unwrap() as char;

    s.push(first);

    while s.len() < size {
        let c = *g
            .choose(
                &(48u8..=57u8)
                    .chain(65u8..=90u8)
                    .chain(97u8..=122u8)
                    .collect::<Vec<_>>(),
            )
            .unwrap() as char;

        s.push(c);
    }

    Rc::new(s)
}

fn shrink_identifier(s: &String) -> Box<dyn Iterator<Item = String>> {
    struct I {
        identifier: String,
    }

    impl Iterator for I {
        type Item = String;

        fn next(&mut self) -> Option<Self::Item> {
            // properly, we should also shrink the individual characters, but
            // whatever
            if self.identifier.len() == 1 {
                return None;
            }

            self.identifier.pop();

            Some(self.identifier.clone())
        }
    }

    Box::new(I {
        identifier: s.clone(),
    })
}

fn arbitrary_non_empty_vec<T>(g: &mut Gen) -> Vec<T>
where
    T: Arbitrary,
{
    let size = {
        let s = g.size();

        *g.choose(&(1..s).collect::<Vec<_>>()).unwrap_or(&1)
    };

    let mut result = Vec::new();

    while result.len() < size {
        result.push(T::arbitrary(g));
    }

    result
}

fn shrink_non_empty_vec<T>(
    v: &Vec<T>,
    min_size: usize,
) -> Box<dyn Iterator<Item = Vec<T>>>
where
    T: Arbitrary,
{
    match v.len() {
        0 => Box::new(std::iter::empty()),
        _ => Box::new(v.shrink().filter(move |v| v.len() >= min_size)),
    }
}

impl Arbitrary for Statement {
    fn arbitrary(g: &mut Gen) -> Self {
        match bool::arbitrary(g) {
            true => Self::Label(Rc::new(String::arbitrary(g))),
            false => Self::Instruction {
                head: arbitrary_identifier(g),
                args: {
                    let exprs: Vec<Argument> = arbitrary_non_empty_vec(g);
                    exprs
                        .into_iter()
                        .map(|arg| (arg, Span::default()))
                        .collect()
                },
            },
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        let t = self.clone();

        match t {
            Self::Label(s) => {
                Box::new(String::shrink(&*s).map(|s| Self::Label(Rc::new(s))))
            }
            Self::Instruction { head, args } => Box::new({
                shrink_identifier(&(*head).clone())
                    .map({
                        let args = args.clone();

                        move |head| Self::Instruction {
                            head: Rc::new(head),
                            args: args.clone(),
                        }
                    })
                    .chain(
                        Vec::shrink(
                            &args
                                .clone()
                                .into_iter()
                                .map(|(arg, _span)| arg)
                                .collect(),
                        )
                        .map({
                            let head = (*head).clone();

                            move |args| Self::Instruction {
                                head: Rc::new(head.clone()),
                                args: args
                                    .into_iter()
                                    .map(|arg| (arg, Span::default()))
                                    .collect(),
                            }
                        }),
                    )
            }),
        }
    }
}

// TODO: Instead of handwriting the [T] enum and/or the argument to [g.choose],
// we should be able to automatically generate it.

impl Arbitrary for Argument {
    fn arbitrary(g: &mut Gen) -> Self {
        enum T {
            Single,
            List,
            Tuple,
        }

        match g.choose(&[T::Single, T::List, T::Tuple]) {
            None => panic!("unreachable"),
            Some(T::Single) => Self::Single(Expr::arbitrary(g)),
            Some(T::List) => Self::List(
                arbitrary_non_empty_vec(g)
                    .into_iter()
                    .map(|e| (e, Span::default()))
                    .collect(),
            ),
            Some(T::Tuple) => Self::Tuple(
                arbitrary_non_empty_vec(g)
                    .into_iter()
                    .map(|e| (e, Span::default()))
                    .collect(),
            ),
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        let t = self.clone();

        match t {
            Self::Single(e) => Box::new(e.shrink().map(Self::Single)),
            Self::List(es) => Box::new(
                shrink_non_empty_vec(
                    &es.into_iter().map(|(e, _span)| e).collect::<Vec<_>>(),
                    1,
                )
                .map(|es| {
                    Self::List(
                        es.into_iter().map(|e| (e, Span::default())).collect(),
                    )
                }),
            ),
            Self::Tuple(es) => Box::new(
                shrink_non_empty_vec(
                    &es.into_iter().map(|(e, _span)| e).collect::<Vec<_>>(),
                    2,
                )
                .map(|es| {
                    Self::Tuple(
                        es.into_iter().map(|e| (e, Span::default())).collect(),
                    )
                }),
            ),
        }
    }
}

impl Arbitrary for Operator {
    fn arbitrary(g: &mut Gen) -> Self {
        use Operator::*;

        *g.choose(&[
            Add, Minus, Mul, Div, Mod, And, Or, Xor, ShiftLeft, ShiftRight,
        ])
        .unwrap()
    }
}

impl Expr {
    fn arbitrary_sized(g: &mut Gen, max_size: usize) -> Expr {
        enum T {
            Literal,
            Var,
            Binop,
        }

        let vec: Vec<_> = vec![
            Some(T::Literal),
            Some(T::Var),
            if max_size > 0 { Some(T::Binop) } else { None },
        ]
        .into_iter()
        .filter_map(id)
        .collect();

        match g.choose(&vec[..]) {
            None => panic!("unreachable"),
            Some(T::Literal) => Self::Literal(i32::arbitrary(g)),
            Some(T::Var) => Self::Var(arbitrary_identifier(g)),
            Some(T::Binop) => Self::Binop(
                Operator::arbitrary(g),
                Box::new(Expr::arbitrary_sized(g, max_size / 2)),
                Box::new(Expr::arbitrary_sized(g, max_size / 2)),
            ),
        }
    }
}

impl Arbitrary for Expr {
    fn arbitrary(g: &mut Gen) -> Self {
        let s = 32;

        //let s = *g.choose(&(1..s).collect::<Vec<_>>()).unwrap_or(&4);

        Expr::arbitrary_sized(g, s)
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        let t = self.clone();

        match t {
            Self::Literal(i) => Box::new(i.shrink().map(Self::Literal)),
            Self::Var(v) => Box::new(
                shrink_identifier(v.as_ref()).map(|v| Self::Var(Rc::new(v))),
            ),
            Self::Binop(op, e1, e2) => Box::new(
                vec![*e1.clone(), *e2.clone()]
                    .into_iter()
                    .chain(e2.shrink().map({
                        let e1 = e1.clone();

                        move |e2| Self::Binop(op, e1.clone(), e2)
                    }))
                    .chain(e1.shrink().map({
                        let e2 = e2.clone();

                        move |e1| Self::Binop(op, e1, e2.clone())
                    })),
            ),
        }
    }
}

#[derive(Clone)]
struct T {
    parsed: Statement,
    rendered: Vec<Token>,
}

impl std::fmt::Debug for T {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}\n", self.parsed)?;

        for t in self.rendered.iter() {
            write!(f, "{} ", t)?;
        }

        Ok(())
    }
}

impl Operator {
    fn priority(&self) -> usize {
        use Operator::*;
        match self {
            Mul | Div | Mod => 0,
            Add | Minus => 1,
            ShiftLeft | ShiftRight => 2,
            And => 3,
            Or => 4,
            Xor => 5,
        }
    }

    fn rendered(&self) -> Token {
        use Operator::*;
        match self {
            Mul => Token::Star,
            Div => Token::Slash,
            Mod => Token::Percent,
            Add => Token::Plus,
            Minus => Token::Dash,
            ShiftLeft => Token::LShift,
            ShiftRight => Token::RShift,
            And => Token::Ampersand,
            Or => Token::Bar,
            Xor => Token::Caret,
        }
    }
}

impl Expr {
    fn rendered(&self) -> Vec<Token> {
        let mut result = Vec::new();

        self.render_impl(None, &mut result);

        result
    }

    // TODO: To be fully robust, we should choose randomly whether to wrap the
    // current expression in parentheses. However, it's hard to make that
    // interact well with the shrinker, as we'd need a source of randomness in
    // [render], which we don't have when implementing [shrink].
    //
    // The way around this is to use a concrete expression syntax structure
    // with explicit parentheses that gets flattened later. Ideally, it would
    // be compiler-derived from the actual [Expr] datatype, but that might
    // require too much HKT-magic for a structure that only exists for testing.
    fn render_impl(&self, parent: Option<Operator>, out: &mut Vec<Token>) {
        match self {
            Expr::Literal(i) => {
                if *i < 0 {
                    out.push(Token::Dash);
                }

                out.push(Token::Number {
                    payload: format!("{}", i.unsigned_abs()),
                    radix: 10,
                });
            }
            Expr::Var(s) => out.push(Token::Ident(s.clone())),
            Expr::Binop(op, e1, e2) => {
                let parens_necessary = match parent {
                    None => false,
                    Some(p) => p.priority() <= op.priority(),
                };

                if parens_necessary {
                    out.push(Token::LParen);
                }

                e1.render_impl(Some(*op), out);
                out.push(op.rendered());
                e2.render_impl(Some(*op), out);

                if parens_necessary {
                    out.push(Token::RParen);
                }
            }
        }
    }
}

fn render_exprs_separated(
    es: &Vec<Spanned<Expr>>,
    start: Token,
    end: Token,
) -> Vec<Token> {
    let mut v = vec![start];

    for (i, (e, _)) in es.iter().enumerate() {
        if i > 0 {
            v.push(Token::Comma);
        }

        e.render_impl(None, &mut v);
    }

    v.push(end);

    v
}

impl Argument {
    fn rendered(&self) -> Vec<Token> {
        match self {
            Argument::Single(e) => e.rendered(),
            Argument::List(es) => {
                render_exprs_separated(es, Token::LBrace, Token::RBrace)
            }
            Argument::Tuple(es) => {
                render_exprs_separated(es, Token::LParen, Token::RParen)
            }
        }
    }
}

impl Statement {
    fn rendered(&self) -> Vec<Token> {
        match self {
            Statement::Label(s) => vec![Token::Ident(s.clone()), Token::Colon],
            Statement::Instruction { head, args } => {
                vec![Token::Ident(head.clone())]
                    .into_iter()
                    .chain(args.iter().flat_map(|(arg, _)| arg.rendered()))
                    .collect()
            }
        }
    }
}

impl Arbitrary for T {
    fn arbitrary(g: &mut Gen) -> Self {
        let parsed = Statement::arbitrary(g);

        let rendered = parsed.rendered();

        T { parsed, rendered }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        Box::new(self.parsed.shrink().map(|parsed| T {
            parsed: parsed.clone(),
            rendered: parsed.rendered(),
        }))
    }
}

impl Token {
    fn delta(&self) -> usize {
        use Token::*;

        match self {
            Ident(s) => s.as_ref().len(),
            Number { payload, radix } => match *radix {
                10 => payload.len(),
                16 => payload.len() + 2,
                2 => payload.len() + 1,
                _ => panic!("impossible"),
            },
            QuotedString(s) => s.len(),
            Colon | Dash | Semi | Slash | Star | Plus | Percent | Ampersand
            | Bar | Caret | Dot | LShift | RShift | Comma | LAngle | RAngle
            | LParen | RParen | LBrace | RBrace | LCurly | RCurly => 1,
        }
    }
}

impl GenericParseErrorHandler<Token> for () {
    fn expected(
        _span: Span,
        _expected: HashSet<Option<Token>>,
        _got: Option<Token>,
    ) -> Self {
    }
}

impl ErrorHandler for () {
    fn bad_number(_span: Span) -> Self {}

    fn empty_parens(_span: Span) -> Self {}

    fn empty_list(_span: Span) -> Self {}
}

// XXX: merge this with [spanless_equal_args] somehow
fn spanless_equal_exprs(
    es1: &Vec<Spanned<Expr>>,
    es2: &Vec<Spanned<Expr>>,
) -> bool {
    for ((e1, _span1), (e2, _span2)) in es1.iter().zip(es2.iter()) {
        if e1 != e2 {
            return false;
        }
    }
    true
}

fn spanless_equal_arg(a1: &Argument, a2: &Argument) -> bool {
    use Argument::*;
    match (a1, a2) {
        (Single(e1), Single(e2)) => e1 == e2,
        (List(l1), List(l2)) => spanless_equal_exprs(l1, l2),
        (Tuple(t1), Tuple(t2)) => spanless_equal_exprs(t1, t2),
        _ => false,
    }
}

fn spanless_equal_args(
    a1: &Vec<Spanned<Argument>>,
    a2: &Vec<Spanned<Argument>>,
) -> bool {
    for ((a1, _span1), (a2, _span2)) in a1.iter().zip(a2.iter()) {
        if !spanless_equal_arg(a1, a2) {
            return false;
        }
    }
    true
}

fn spanless_equal(s1: &Statement, s2: &Statement) -> bool {
    use Statement::*;
    match (s1, s2) {
        (Label(s1), Label(s2)) => *s1 == *s2,
        (
            Instruction { head: h1, args: a1 },
            Instruction { head: h2, args: a2 },
        ) => *h1 == *h2 && spanless_equal_args(a1, a2),
        _ => false,
    }
}

#[quickcheck]
fn render_parse_id(t: T) -> bool {
    struct I {
        iter: std::vec::IntoIter<Token>,
        offs: usize,
    }

    impl Iterator for I {
        type Item = Spanned<Token>;

        fn next(&mut self) -> Option<Self::Item> {
            let next = self.iter.next()?.clone();

            let delta = next.delta();

            let p0 = Position {
                offset: self.offs,
                row: 0,
                col: 0,
            };

            self.offs = self.offs + delta;

            let p1 = Position {
                offset: self.offs,
                row: 0,
                col: 0,
            };

            let span = Span {
                source: Source::Unknown,
                span: p0..p1,
            };

            Some((next, span))
        }
    }

    let T { rendered, parsed } = t;

    match parse_line::<()>(
        (I {
            iter: rendered.into_iter(),
            offs: 0,
        })
        .collect(),
    ) {
        Err(_) => false,
        Ok(mut s) => {
            if s.len() != 1 {
                return false;
            }
            spanless_equal(&s.pop().map(|(t, _span)| t).unwrap(), &parsed)
        }
    }
}
