use crate::lang::syntax::directive::Unparsed;

use super::{
    lexer, Carrier, Directive, GenericParseErrorHandler, Location, Span, Token,
    WithLocation,
};

use chumsky::{error::Error as ChumskyError, prelude::*};

pub trait PreprocessErrorHandler:
    GenericParseErrorHandler<lexer::Out> + 'static
{
}

enum Out {}
