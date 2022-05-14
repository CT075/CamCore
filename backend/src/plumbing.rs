// various utility functions for programming convenience

pub fn id<A>(x: A) -> A {
    x
}

pub fn id2<A, B>(x: A, y: B) -> (A, B) {
    (x, y)
}

pub fn map_first<A, A2, B>((x, y): (A, B), f: impl FnOnce(A) -> A2) -> (A2, B) {
    (f(x), y)
}

pub fn map_second<A, B, B2>(
    (x, y): (A, B),
    f: impl FnOnce(B) -> B2,
) -> (A, B2) {
    (x, f(y))
}

pub fn map_both<A, A2, B, B2>(
    (x, y): (A, B),
    f1: impl FnOnce(A) -> A2,
    f2: impl FnOnce(B) -> B2,
) -> (A2, B2) {
    // this could be [map_first(f1, map_second(f2, (x,y)))], but that's
    // probably a little crazy
    (f1(x), f2(y))
}

pub fn fst<A, B>((x, _): (A, B)) -> A {
    x
}

pub fn snd<A, B>((_, y): (A, B)) -> B {
    y
}
