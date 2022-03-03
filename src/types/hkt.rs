// Implementation of "Lightweight Higher Kinded Types" by Jeremie Yallop and
// Leo White.
//
// We could, in theory, use

pub trait Witness<A>: Sized {
    type This;

    // This is to enforce that the witness is phantom-only. This isn't
    // foolproof; you can also easily use [panic!()] or an infinite loop, but
    // the idea is the same.
    fn absurd(self) -> std::convert::Infallible;
}

pub struct Apply<F: Witness<A>, A> {
    prj: F::This,
}

impl<F, A> Apply<F, A>
where
    F: Witness<A>,
{
    fn inj(this: F::This) -> Self {
        Apply { prj: this }
    }

    fn prj(self) -> F::This {
        self.prj
    }
}

pub enum ConstW {}
impl<A> Witness<A> for ConstW {
    type This = A;

    fn absurd(self) -> std::convert::Infallible {
        match self {}
    }
}

pub enum OptionW {}
impl<A> Witness<A> for OptionW {
    type This = Option<A>;

    fn absurd(self) -> std::convert::Infallible {
        match self {}
    }
}

pub enum VecW {}
impl<A> Witness<A> for VecW {
    type This = Vec<A>;

    fn absurd(self) -> std::convert::Infallible {
        match self {}
    }
}

pub trait Functor {
    fn fmap<A, B, F>(this: Apply<Self, A>, f: F) -> Apply<Self, B>
    where
        Self: Witness<A> + Witness<B>,
        F: Fn(A) -> B;
}

impl Functor for ConstW {
    fn fmap<A, B, F>(this: Apply<Self, A>, f: F) -> Apply<Self, B>
    where
        F: Fn(A) -> B,
    {
        Apply::inj(f(this.prj()))
    }
}

impl Functor for OptionW {
    fn fmap<A, B, F>(this: Apply<Self, A>, f: F) -> Apply<Self, B>
    where
        F: Fn(A) -> B,
    {
        Apply::inj(this.prj().map(f))
    }
}

impl Functor for VecW {
    fn fmap<A, B, F>(this: Apply<Self, A>, f: F) -> Apply<Self, B>
    where
        F: Fn(A) -> B,
    {
        Apply::inj(this.prj().into_iter().map(f).collect())
    }
}
