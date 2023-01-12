// XXX: The overall parsing infrastructure uses a bunch of different techniques
// to do the same thing. For example, there are a few places where we use
// [chumsky::error::Simple] instead of [Carrier] for inner parser errors. A
// good LHF fix would be to clean these up.

pub mod common;
mod core;

pub use self::core::{parse_line, Argument, ErrorHandler, Event, Statement};
