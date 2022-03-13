// Implementation of "Lightweight Higher Kinded Types" by Jeremie Yallop and
// Leo White.
//
// In the future, this could be rewritten to use GATs which would simplify a
// lot, particularly the duplicate trait bounds. While I'm not, in general,
// afraid of unstabilized features, I think that there's a lot of space to
// explore the GAT approach and don't have much time to deal with any issues
// that arise from them.
//
// In particular, we could probably just do this:
//
//   pub trait Witness: Sized {
//     type This<A>;
//   }
//
//   pub struct Apply<F: Witness, A> {
//     prj: F::This<A>;
//   }
//
// and then just use [F: Witness] bounds everywhere instead of needing this
// clunky [F: Witness<A> + Witness<B>] bound (which in turn requires a lot of
// [<F as Witness<A>>::This] boilerplate).

pub trait Witness<A>: Sized {
    // We can work around the unsafety required by the OCaml embedding because
    // Rust has type familes
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

pub enum IdentityW {}
impl<A> Witness<A> for IdentityW {
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

pub enum VoidW {}
impl<A> Witness<A> for VoidW {
    type This = std::convert::Infallible;

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

impl Functor for IdentityW {
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

impl Functor for VoidW {
    fn fmap<A, B, F>(_this: Apply<Self, A>, _: F) -> Apply<Self, B>
    where
        Self: Witness<A> + Witness<B>,
        F: Fn(A) -> B,
    {
        panic!("fmap::<VoidW>: somehow had value of type [Apply<VoidW, A>]")
    }
}
