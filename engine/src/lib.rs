pub mod driver;
pub mod io;
pub mod lang;
pub mod plumbing;
mod raws;
pub mod types;

pub mod errors {
    use super::*;

    pub use lang::{
        parse::{
            common::GenericParseErrorHandler, ErrorHandler as ParseErrorHandler,
        },
        preprocess::{
            parse::ErrorHandler as PreprocessParseErrorHandler,
            ErrorHandler as PreprocessErrorHandler,
        },
    };

    pub use types::string_with_vars::{
        ParseErrorHandler as SWVParseErrorHandler,
        RenderErrorHandler as SWVRenderErrorHandler,
    };
}
