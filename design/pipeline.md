
# Overview

This document outlines some high-level technical details of how this program
works. If you are interested in contributing to this project, this is a good
entry point. I will try to use terms that will be familiar to the GBA FE ROM
hacking community where possible. Otherwise, you may find it useful to look up
how a basic compiler pipeline works before trying to read this document.

We will consider the "front end" to be any part of the program dealing with the
EA language itself -- language raws, parsing, preprocessing, and other
interaction with the file system. We'll also consider "translating statements
to binary" to be part of the front-end's job.

The frontend's job is to take the project source (as a string, or more likely
as a tuple of `(file_tree, entry_filepath)`) and output a list of `(offset,
data)` tuples. The frontend will do no validation over these tuples (ie,
overlap checks).

The backend will handle translating these tuples into some other format. In the
immediate case, this will be Writing The ROM, but it wouldn't be out of the
question to plug in something else instead (raw json, DSA, etc).

# Frontend

Parsing is done in a few different passes, primarily using the
[chumsky](https://github.com/zesterer/chumsky) parser combinator library.

The first pass is effectively a lexer, splitting the source into directives,
`MESSAGE`s (both of which are read verbatim-minus-comments), and more typical
tokens representing individual textual units.
