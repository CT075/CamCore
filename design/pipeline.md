
# Overview

This document outlines some high-level technical details of how this program
works. If you are interested in contributing to this project, this is a good
entry point. I will try to use terms that will be familiar to the GBA FE ROM
hacking community where possible. Otherwise, you may find it useful to look up
how a basic compiler pipeline works before trying to read this document.

We will consider the "front end" to be any part of the program dealing with the
EA language itself -- language raws, parsing, preprocessing, and translation
into hex.

The frontend's job is to take the project source (as a string, or more likely
as a tuple of `(file_tree, entry_filepath)`) and output a list of `(offset,
data)` tuples. The frontend will do no validation over these tuples (ie,
overlap checks).

The backend will handle translating these tuples into some other format. In the
immediate case, this will be Writing The ROM, but it wouldn't be out of the
question to plug in something else instead (DSA, merlinus???, etc).

# Frontend

## Lexing

Lexing is currently done by hand in `src/lang/lex.rs`, which does nothing but
tokenize the input using a similar process to [ColorzCore](https://github.com/FireEmblemUniverse/ColorzCore/blob/master/ColorzCore/Lexer/Tokenizer.cs).

It is difficult to use a traditional lexer-generator for a few reasons, the
biggest of which is that lexing is not context-free. Consider the following
inputs:

```
#include a/b/c

UNIT a/b/c
```

In the first case, we want to lex `a/b/c` as a single token (a filepath), but in
the second, we want to lex it as five (the identifiers `a`,`b`,`c` separated by
division tokens). There are a few ways around this, such as reconstructing the
file path after lexing, but all of them (as far as I can tell) would require
tracking whitespace (is `a b/c` a pair of expressions, or a single filepath
with a parent of `a b`?), which is, in my view, a much greater evil.

## Preprocessing

Preprocessing is broken into two stages. The first stage builds a data
structure holding contiguous blocks of tokens, separated by directives. The
output of this stage can be cached in a more structured format for easy lookup
(so we don't have to read it repeatedly for files that are included multiple
times).

With regards to caching intermediate reads, it's difficult to be much more
intelligent without sacrificing semantics. One idea might be to also collect
all definitions from included files (and the `ifdef`s they're underneath), but
I chose to go with the simpler approach for now.

The second stage of preprocessing processes this structure, potentially kicking
off new pipelines whenever an include (or `#inctext` etc) is encountered, and
otherwise transforming the token blocks.

