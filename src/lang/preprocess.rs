use std::collections::HashSet;

use core::ops::Range;

pub mod syntax;

mod parse;

pub trait PreprocessParseErrorHandler {
    fn expected(
        what: &'static str,
        found: Option<char>,
        why: &'static str,
        span: Range<usize>,
    ) -> Self;
    fn unexpected_eof(hint: &'static str, span: Range<usize>) -> Self;
    fn too_many_args(
        directive: &'static str,
        expected_amount: usize,
        span: Range<usize>,
    ) -> Self;
    fn unclosed_if(span: Range<usize>) -> Self;
    fn standalone_else(span: Range<usize>) -> Self;
    fn standalone_endif(span: Range<usize>) -> Self;
    fn bad_macro_arg(span: Range<usize>) -> Self;
    fn empty_body(span: Range<usize>) -> Self;
}
