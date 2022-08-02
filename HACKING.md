# Contributing to this project

## Basic overview

TODO

## Low-hanging fruit

The following are issues/chores in the codebase that I didn't want to spend too
much time on. These would be good cleanup issues.

### Module structure

The module structure of `engine` is a complete disaster. I initially had a nice
idea where `lang` would contain the assembler front-end, from parsing to
preprocessing, and hand a list (stream?) of `Statement`s to the backend, which
would take care of the actual assembly. However, as the project progressed,
things got
