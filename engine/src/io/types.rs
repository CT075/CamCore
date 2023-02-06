use std::path::{Path, PathBuf};

use relative_path::RelativePath;

pub struct FileContents<P> {
    pub source: P,
    pub contents: String,
}

impl<P> FileContents<P>
where
    P: AsRef<Path>,
{
    pub fn new(source: P, contents: String) -> Self {
        Self { source, contents }
    }
}

pub trait ErrorHandler: 'static {
    fn os_error(underlying: std::io::Error) -> Self;

    fn multiple_candidates(found: Vec<PathBuf>) -> Self;
}

pub struct Text;
pub struct Binary;

// [https://rust-lang.github.io/api-guidelines/future-proofing.html]
mod private {
    pub trait Sealed {}

    impl Sealed for super::Text {}
    impl Sealed for super::Binary {}
}

pub trait Kind: private::Sealed {
    type Payload;
}

impl Kind for Text {
    type Payload = String;
}

impl Kind for Binary {
    type Payload = Vec<u8>;
}

pub trait FileTreeProvider {
    fn search_and_load_file<K, E>(
        &self,
        path: &RelativePath,
        current_directory: Option<impl AsRef<Path>>,
        kind: K,
    ) -> Result<(PathBuf, K::Payload), E>
    where
        E: ErrorHandler,
        K: Kind;

    fn run_process<K, E>(
        &self,
        exe: &RelativePath,
        args: Vec<String>,
        current_directory: Option<impl AsRef<Path>>,
        kind: K,
    ) -> Result<(PathBuf, K::Payload), E>
    where
        E: ErrorHandler,
        K: Kind;
}
