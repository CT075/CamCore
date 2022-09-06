use std::path::{Path, PathBuf};

use relative_path::RelativePath;

use super::types::*;

pub struct FileTree<'a> {
    roots: &'a Vec<PathBuf>,
    current_dir: Option<PathBuf>,
}

impl<'a> FileTree<'a> {
    pub fn with_roots<F, A>(roots: Vec<impl AsRef<Path>>, f: F) -> A
    where
        F: FnOnce(FileTree<'_>) -> A,
    {
        let roots = roots.iter().map(|p| p.as_ref().to_owned()).collect();

        f(FileTree {
            roots: &roots,
            current_dir: None,
        })
    }
}

impl<'a> FileTreeProvider for FileTree<'a> {
    fn with_current_directory<F, A>(&self, dir: impl AsRef<Path>, f: F) -> A
    where
        F: FnOnce(&Self) -> A,
    {
        f(&FileTree {
            roots: self.roots,
            current_dir: Some(dir.as_ref().to_owned()),
        })
    }

    fn search_and_load_file<K, E>(
        &self,
        path: &RelativePath,
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
        kind: K,
    ) -> Result<(PathBuf, K::Payload), E>
    where
        E: ErrorHandler,
        K: Kind,
    {
        todo!()
    }
}
