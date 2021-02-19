use genawaiter::rc::{Co, Gen};

use super::{
    super::syntax::{Token, TokenAnnot},
    Context, Error, ErrorAnnot,
};

pub(super) fn stream<'a, 'b>(
    v: &'a Vec<TokenAnnot>,
    ctx: &'b mut Context,
) -> impl Iterator<Item = TokenAnnot> {
    Gen::new(|co| async move { panic!("todo") }).into_iter()
}
