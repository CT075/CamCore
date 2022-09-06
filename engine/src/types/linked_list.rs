// Adapted from [https://rust-unofficial.github.io/too-many-lists]
//
// This assembler has a lot of "layered" structures with shared substructures
// that need to be repeated a bunch of times, making it awkward to use a [Vec].
// For example, verbose error reporting requires tracking the sequence of
// expanded macros leading to a parse tree, which can come from the same user
// source.

use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct List<T> {
    head: Link<T>,
}

type Link<T> = Option<Rc<Node<T>>>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Node<T> {
    value: T,
    next: Link<T>,
}

pub enum Direction {
    FirstOnTop,
    LastOnTop,
}

impl<T> List<T> {
    pub fn empty() -> Self {
        Self { head: None }
    }

    pub fn cons(&self, value: T) -> Self {
        let head = Some(Rc::new(Node {
            value,
            next: self.head.clone(),
        }));

        Self { head }
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a T> {
        Iter {
            next: self.head.as_deref(),
        }
    }

    // [cons] has type [(&Self, T) -> Self], but [fold] requires a function of
    // type [(A, B) -> A], which [cons] doesn't fit (because [&Self] and [Self]
    // are different types), so we create this version for convenience.
    fn cons_owned(self, value: T) -> Self {
        self.cons(value)
    }

    pub fn from_vec(v: Vec<T>, direction: Direction) -> Self {
        let fold = match direction {
            Direction::FirstOnTop => std::vec::IntoIter::rfold,
            Direction::LastOnTop => std::vec::IntoIter::fold,
        };

        fold(v.into_iter(), Self::empty(), Self::cons_owned)
    }

    pub fn singleton(value: T) -> Self {
        Self::empty().cons(value)
    }
}

struct Iter<'a, T> {
    next: Option<&'a Node<T>>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|node| {
            self.next = node.next.as_deref();
            &node.value
        })
    }
}
