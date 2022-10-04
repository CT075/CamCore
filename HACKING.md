# Contributing to this project

## Basic overview

TODO

## Low-hanging fruit

The following are issues/chores in the codebase that I didn't want to spend too
much time on. These would be good cleanup issues.

### Empty/Non-empty containers

There are several places where we use the type `Option<Vec<T>>` for some `T`,
in which `Some([])` is invalid. In other places, we simply use `Vec<T>` to
represent a structure in which "empty list of args" gets special handling. In
both cases, we should instead use `Option<NonEmptyVec<T>>` to make the
semantics clearer.

