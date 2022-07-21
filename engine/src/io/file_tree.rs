use std::path::Path;

use relative_path::RelativePath;

pub trait FileTree {
    fn create_with_root(root: impl AsRef<Path>) -> Self;

    type Lines: Iterator<Item = String>;

    fn read_from_file(&self, path: RelativePath) -> Self::Lines;
}
