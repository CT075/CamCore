
//use chomp::parse;
use chomp::parsers::Error as ChompError;
use chomp::types::{Buffer, Input, ParseResult};

pub type Error = ChompError<u8>;

fn parse_line<I: Input<Token=u8>>(i: I) -> ParseResult<I, String, Error> {
    panic!("todo");
}

