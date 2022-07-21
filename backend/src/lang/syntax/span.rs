use std::{default::Default, ops::Range, rc::Rc};

use crate::types::hkt::{Apply, Functor, Witness};

// XXX CHORE: Instead of this, we could use a real global string interner with
// [lazy_static]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Source(Rc<String>);

const DUMMY_SOURCE: &'static str = "_unknown_";

impl Source {
    pub fn new(s: impl AsRef<str>) -> Self {
        Source(Rc::new(s.as_ref().to_owned()))
    }
}

impl Default for Source {
    fn default() -> Self {
        Self::new(DUMMY_SOURCE.to_owned())
    }
}

// XXX: This entire song and dance is necessary to support [__LINE__],
// [__COL__] and [__FILE__]. For error-reporting, all we actually need is
// [offset] -- the error rendering library will recompute [row] and [col]
// anyway. It would be great if we could just use [Span = (Source, Range<usize>)]
// and call it a day.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Position {
    pub offset: usize,
    pub row: usize,
    pub col: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub source: Source,
    pub span: Range<Position>,
}

impl Span {
    pub fn source(&self) -> &Source {
        &self.source
    }

    pub fn start(&self) -> &Position {
        &self.span.start
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            source: Source::default(),
            span: Position::default()..Position::default(),
        }
    }
}

impl chumsky::span::Span for Span {
    type Context = Source;
    type Offset = Position;

    fn new(ctx: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            source: ctx,
            span: range,
        }
    }

    fn context(&self) -> Self::Context {
        self.source.clone()
    }

    fn start(&self) -> Self::Offset {
        self.span.start.clone()
    }

    fn end(&self) -> Self::Offset {
        self.span.end.clone()
    }
}

pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpannedW {}

impl<T> Witness<T> for SpannedW {
    type This = Spanned<T>;
}

impl Functor for SpannedW {
    fn fmap<A, B, F>(this: Apply<Self, A>, f: F) -> Apply<Self, B>
    where
        F: Fn(A) -> B,
    {
        let (x, span) = this.prj();
        Apply::inj((f(x), span))
    }
}

pub struct I {
    underlying: std::vec::IntoIter<char>,
    source: Source,
    row: usize,
    col: usize,
    offset: usize,
}

impl Iterator for I {
    type Item = Spanned<char>;

    fn next(&mut self) -> Option<Self::Item> {
        self.underlying.next().map(|c| {
            let (row, col) = (self.row, self.col);
            let (row_, col_) = match c {
                '\n' => (row + 1, 0),
                _ => (row, col + 1),
            };
            let offset_ = self.offset + 1;

            let span = Span {
                source: self.source.clone(),
                span: Position {
                    offset: self.offset,
                    row,
                    col,
                }..Position {
                    offset: offset_,
                    row: row_,
                    col: col_,
                },
            };

            self.row = row_;
            self.col = col_;
            self.offset = offset_;

            (c, span)
        })
    }
}

pub fn stream_spanned<'a, 'b, 'c>(
    source: Option<&'a Source>,
    start: Option<&'a Position>,
    s: impl AsRef<str>,
) -> chumsky::stream::Stream<'b, char, Span, I> {
    let default_source = &Source::default();
    let source = source.unwrap_or(default_source);

    let default_start = &Position::default();
    let start = start.unwrap_or(default_start);

    // XXX: It'd be nice to avoid needing to do this
    let vec: Vec<_> = s.as_ref().chars().collect();

    let len = vec.len();

    let iter = I {
        source: source.clone(),
        underlying: vec.into_iter(),
        row: start.row,
        col: start.col,
        offset: start.offset,
    };

    let eoi = Span {
        source: source.clone(),
        span: Position {
            offset: len,
            // [row] and [col] are solely for macro expansion purposes and so
            // don't matter here.
            row: 0,
            col: 0,
        }..Position {
            offset: len,
            row: 0,
            col: 0,
        },
    };

    chumsky::stream::Stream::from_iter(eoi, iter)
}
