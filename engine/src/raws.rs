use std::collections::HashSet;

mod builder;
mod parse;

// TODO: Raws documentation comments.

mod param {
    pub enum Kind {
        Numeric { pointer: bool, length: usize },
        List { num_coords: usize, length: usize },
        Fixed { bytes: Vec<u8> },
    }

    pub struct T {
        name: String,
        kind: Kind,
    }
}

pub use param::T as Param;

pub enum Kind {
    Basic(Vec<(usize, Param)>),
    Repeatable(Param),
    Varargs { param: Param, terminator: u8 },
}

pub struct Raw {
    id: [u8; 2],
    game: HashSet<String>,
    alignment: usize,
    kind: Kind,
    params: Vec<Param>,
}

impl Raw {
    fn new(
        id: [u8; 2],
        game: HashSet<String>,
        alignment: usize,
        kind: Kind,
        params: Vec<Param>,
    ) -> Self {
        Self {
            id,
            game,
            alignment,
            kind,
            params,
        }
    }
}
