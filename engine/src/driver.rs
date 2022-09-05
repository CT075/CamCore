use std::path::{Path, PathBuf};

use relative_path::RelativePath;

use crate::{
    io::ErrorHandler as IOErrorHandler,
    lang::{
        preprocess as pp,
        syntax::{span::Source, Span, Spanned, TokenGroup},
    },
    plumbing::*,
    types::{string_with_vars as swv, StringWithVars},
};

// TODO: make this a trait?
struct Driver<E> {
    message_log: Vec<String>,
    error_log: Vec<E>,
}

impl<E> pp::Driver<E> for Driver<E>
where
    E: pp::ErrorHandler + swv::RenderErrorHandler,
{
    fn push_message(
        &mut self,
        msg: &StringWithVars,
        lookup_symbol: impl Fn(&String) -> (Option<String>, Option<E>),
        span: Span,
    ) {
        match msg.render(lookup_symbol) {
            Ok(msg) => self.message_log.push(msg),
            Err(e) => self.error_log.extend(e),
        }
    }

    fn push_line<'a>(
        &'a mut self,
        definitions: pp::Definitions<'a>,
        line: pp::Events,
        original_line: &'a Vec<TokenGroup>,
    ) {
        todo!()
    }

    /// Push a preprocessing error to the driver.
    fn push_error(&mut self, err: E) {
        self.error_log.push(err)
    }

    fn register_symbol(&mut self, symbol: String, span: Span) {
        todo!()
    }

    fn push_binary_file(
        &mut self,
        path: &RelativePath,
        current_dir: Option<impl AsRef<Path>>,
        span: &Span,
    ) {
        todo!()
    }

    /// Ask the driver to locate a file for the purpose of `#include`, then
    /// return its contents if possible.
    ///
    /// Unlike `push_binary_file`, the preprocessor needs to be able to read
    /// the output of, so errors need to be caught and registered by the
    /// preprocessor itself.
    fn request_file<IOError>(
        &self,
        path: &RelativePath,
        current_dir: Option<impl AsRef<Path>>,
    ) -> Result<(PathBuf, String), IOError>
    where
        IOError: IOErrorHandler,
    {
        todo!()
    }

    /// Run a process in `current_dir` and capture its binary output. See
    /// `push_binary_file`.
    fn push_binary_process_run(
        &mut self,
        exe: &RelativePath,
        args: &Vec<StringWithVars>,
        lookup_symbol: impl Fn(&String) -> (Option<String>, Option<E>),
        current_dir: Option<impl AsRef<Path>>,
        span: &Span,
    ) {
        todo!()
    }

    /// Run a process in `current_dir` and capture its output for event
    /// processing. See `request_file`.
    fn request_process_run<IOError>(
        // this is mutable so that any errors from rendering [args] can be
        // reported properly
        &mut self,
        exe: &RelativePath,
        args: &Vec<StringWithVars>,
        lookup_symbol: impl Fn(&String) -> (Option<String>, Option<E>) + Copy,
        current_dir: Option<impl AsRef<Path>>,
    ) -> Result<(String, Source), IOError>
    where
        IOError: IOErrorHandler,
    {
        todo!()
    }
}
