use std::collections::HashSet;

use crate::lang::types::{Game};

pub struct EACode {
    name: String,
    id: u32,
    length: u32,
    langs: HashSet<Game>,
    repeatable: bool,
    unsafe_: bool,
    index_mode: u32,
    // TODO
    terminating_list: Option<u32>,
    offset_mod: u32,
    doc: String,
    params: Vec<CodeParam>
}

pub struct CodeParam {
    name: String,
    pos: u32,
    length: u32,
    pointer: bool,
    coords: u32,
    fixed: bool,
    signed: bool,
    doc: String,
}

impl EACode {
    pub fn new(name: String, id: u32, length: u32) -> Self {
        EACode {
            name: name,
            id: id,
            length: length,
            langs: HashSet::new(),
            repeatable: false,
            unsafe_: false,
            index_mode: 1,
            terminating_list: None,
            offset_mod: 4,
            doc: "".to_string(),
            params: Vec::new(),
        }
    }

    pub fn add_game(self, g: Game) -> Self {
        let mut langs = self.langs;
        langs.insert(g);
        EACode { langs: langs, ..self }
    }

    pub fn set_unsafe(self) -> Self {
        EACode { unsafe_: true, ..self }
    }

    pub fn set_repeatable(self) -> Self {
        EACode { repeatable: true, ..self }
    }

    pub fn set_index_mode(self, i: u32) -> Self {
        EACode { index_mode: i, ..self }
    }

    pub fn set_terminator(self, v: u32) -> Self {
        EACode { terminating_list: Some(v), ..self }
    }

    pub fn set_offset_mod(self, v: u32) -> Self {
        EACode { offset_mod: v, ..self }
    }

    pub fn set_params(self, v: Vec<CodeParam>) {
        EACode { params: v, ..self }
    }
}

