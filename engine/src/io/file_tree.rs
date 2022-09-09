use std::path::{Path, PathBuf};

use relative_path::RelativePath;

use super::types::*;

pub struct FileTree<'a>(&'a Vec<PathBuf>);

impl<'a> FileTree<'a> {
    pub fn with_roots<F, A>(roots: Vec<impl AsRef<Path>>, f: F) -> A
    where
        F: FnOnce(FileTree<'_>) -> A,
    {
        let roots = roots.iter().map(|p| p.as_ref().to_owned()).collect();

        f(FileTree(&roots))
    }
}

impl<'a> FileTreeProvider for FileTree<'a> {
    fn search_and_load_file<K, E>(
        &self,
        path: &RelativePath,
        current_directory: Option<impl AsRef<Path>>,
        kind: K,
    ) -> Result<(PathBuf, K::Payload), E>
    where
        E: ErrorHandler,
        K: Kind,
    {
        todo!()
    }

    fn run_process<K, E>(
        &self,
        exe: &RelativePath,
        args: Vec<String>,
        current_directory: Option<impl AsRef<Path>>,
        kind: K,
    ) -> Result<(PathBuf, K::Payload), E>
    where
        E: ErrorHandler,
        K: Kind,
    {
        todo!()
    }
}
