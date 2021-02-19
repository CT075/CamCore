use std::path::Path;

use relative_path::RelativePathBuf;

use super::{syntax::Ast, PreprocError};

pub fn include_file(
    search_paths: Vec<&Path>,
    path: RelativePathBuf,
) -> (Ast, Vec<PreprocError>) {
    panic!("todo")
}
