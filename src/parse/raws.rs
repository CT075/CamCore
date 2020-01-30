
//use chomp::parse;
use chomp::prelude::{U8Input, sep_by, or, string, token, many1, option};
use chomp::types::{Buffer};
use chomp::ascii::{is_alphanumeric, decimal};
use chomp::combinators::{skip_many};

use crate::raws::types::{EACode, CodeParam};
use crate::lang::types::{Game};

use super::common::*;

enum CodeFlag {
    Lang(Vec<Game>),
    Repeatable,
    Unsafe,
    IndexMode(u32),
    TerminatingList(u32),
    OffsetMod(u32),
}

enum ParamFlag {
    Pointer,
    Coords(u32, u32),
    PreferredBase(u8),
    Signed,
}

fn cmd_header<I:U8Input>(i:I) -> ParseResult<I, EACode> {
    parse!{i;
        let name = word();
        comma_sep();
        let id = number();
        comma_sep();
        let len = number();
        comma_sep();
        // TODO: flags
        ret EACode::new(name, id as u32, len as u32)
    }
}

fn lang_single<I:U8Input>(i:I) -> ParseResult<I, Game> {
    use Game::*;
    parse!{i;
        token(b':');

        const_item(b"fe6", FE6) <|>
        const_item(b"fe7", FE7) <|>
        const_item(b"fe8", FE8) <|>
        wrap_item(Other)
    }
}

fn lang_flag<I:U8Input>(i:I) -> ParseResult<I, CodeFlag> {
    parse!{i;
        token(b'-');
        string(b"lang") <|> string(b"game");
        let flags : Vec<Game> = many1(lang_single);

        ret CodeFlag::Lang(flags);
    }
}

fn index_flag<I:U8Input>(i:I) -> ParseResult<I, CodeFlag> {
    fn argument<I:U8Input>(i:I) -> ParseResult<I, CodeFlag> {
        token(i,b':').then(|i|
        number(i).bind(|i,v|
            i.ret(CodeFlag::IndexMode(v as u32))
        ))
    }

    parse!{i;
        token(b'-');
        string(b"indexMode");
        option(argument, CodeFlag::IndexMode(8))
    }
}

fn offset_mod_flag<I:U8Input>(i:I) -> ParseResult<I, CodeFlag> {
    fn argument<I:U8Input>(i:I) -> ParseResult<I, CodeFlag> {
        token(i,b':').then(|i|
        number(i).bind(|i,v|
            i.ret(CodeFlag::OffsetMod(v as u32))
        ))
    }

    parse!{i;
        token(b'-');
        string(b"offsetMod");
        option(argument, CodeFlag::OffsetMod(4))
    }
}

fn terminating_list_flag<I:U8Input>(i:I) -> ParseResult<I, CodeFlag> {
    parse!{i;
        token(b'-');
        string(b"terminatingList");
        token(b'-');
        let v = number();

        ret CodeFlag::TerminatingList(v as u32);
    }
}

fn unimplemented_code_flags<I:U8Input>(i:I) -> ParseResult<I, ()> {
    fn priority<I:U8Input>(i:I) -> ParseResult<I, ()> {
        parse!{i;
            token(b'-');
            string(b"priority");
            token(b':');
            word();

            ret ();
        }
    }

    fn others<I:U8Input>(i:I) -> ParseResult<I, ()> {
        parse!{i;
            token(b'-');
            string(b"end") <|>
            string(b"noAssembly") <|>
            string(b"noDisassembly");

            ret ();
        }
    }

    parse!{i;
        horizontal_space();
        priority() <|> others();
        ret ();
    }
}

fn code_flag<I:U8Input>(i:I) -> ParseResult<I, CodeFlag> {
    use CodeFlag::*;

    parse!{i;
        skip_many(unimplemented_code_flags);

        lang_flag() <|>
        const_item(b"repeatable", Repeatable) <|>
        const_item(b"unsafe", Unsafe) <|>
        index_flag() <|>
        terminating_list_flag() <|>
        offset_mod_flag()
    }
}

fn cmd_flags<I:U8Input>(i:I, c:EACode) -> ParseResult<I, EACode> {
    use CodeFlag::*;
    sep_by(i, code_flag, horizontal_space).map(|v : Vec<_>| {
        v.into_iter().fold(c, |c, f|
            match f {
                Lang(langs) =>
                    langs.into_iter().fold(c, |c, g| c.add_game(g)),
                Repeatable => c.set_repeatable(),
                Unsafe => c.set_unsafe(),
                IndexMode(v) => c.set_index_mode(v),
                TerminatingList(v) => c.set_terminator(v),
                OffsetMod(v) => c.set_offset_mod(v),
            }
        )
    })
}

fn param<I:U8Input>(i:I) -> ParseResult<I, CodeParam> {
    parse!{i;
        horizontal_space();
        let name = word();
        comma_sep();
        let position = number();
        comma_sep();
        let length = number();
    }
}

fn cmd<I:U8Input>(i:I) -> ParseResult<I, EACode> {
    parse!{i;
        let c = cmd_header();
        cmd_flags(c)
    }
}

