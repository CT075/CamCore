use std::collections::HashMap;

use super::directive::DirectiveError;

pub enum Definition {
    Empty,
    Rename(String),
    Macro(String),
}

pub struct State {
    defines: HashMap<String, Definition>,
    pub(super) ifstack: Vec<bool>,
}

impl State {
    pub fn new() -> Self {
        State {
            defines: HashMap::new(),
            ifstack: Vec::new(),
        }
    }

    pub fn define(&mut self, s: String, d: Definition) -> Result<(), DirectiveError> {
        if let Some(_) = self.defines.insert(s, d) {
            Err(DirectiveError::DefineDuplicate)
        } else {
            Ok(())
        }
    }

    pub fn active(&self) -> bool {
        if let Some(&b) = self.ifstack.last() {
            b
        } else {
            true
        }
    }

    pub fn push(&mut self, b: bool) {
        self.ifstack.push(b)
    }

    pub fn pop(&mut self) -> Option<bool> {
        self.ifstack.pop()
    }
}
