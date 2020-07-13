// Because Rust doesn't have a nice cross-platform path library, we need to
// roll our own.

// for ease of refactoring when the way we currently handle paths ends up being
// woefully inadequate.

pub(super) fn valid_segment_character(c: char) -> bool {
    match c {
        '\0'..='\x1f' | '<' | '>' | ':' | '"' | '/' | '\\' | '|' | '?' | '*' => false,
        _ => true,
    }
}

pub(super) fn separator(c: char) -> bool {
    match c {
        '/' | '\\' => true,
        _ => false,
    }
}
