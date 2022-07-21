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
    // Rust has associated types, which is sufficient.
    type This;
}

pub struct Apply<F: Witness<A>, A> {
    prj: F::This,
}

impl<F, A> Apply<F, A>
where
    F: Witness<A>,
{
    pub fn inj(this: F::This) -> Self {
        Apply { prj: this }
    }

    pub fn prj(self) -> F::This {
        self.prj
    }
}

pub enum IdentityW {}
impl<A> Witness<A> for IdentityW {
    type This = A;
}

pub enum OptionW {}
impl<A> Witness<A> for OptionW {
    type This = Option<A>;
}

pub enum VecW {}
impl<A> Witness<A> for VecW {
    type This = Vec<A>;
}

pub enum VoidW {}
impl<A> Witness<A> for VoidW {
    type This = std::convert::Infallible;
}

// For some reason, if we try to inline this function to the definition of
// [fmap] in [Functor], it can't unify
//
//   <VoidW as Witness<A>> == Infallible
// and
//   Infallible = <VoidW as Witness<B>>
//
// but if we write the function here, it works.
impl VoidW {
    fn transform<A, B>(
        x: <Self as Witness<A>>::This,
    ) -> <Self as Witness<B>>::This {
        x
    }
}

pub struct ConstW<T> {
    phantom: std::marker::PhantomData<T>,
}

impl<T, A> Witness<A> for ConstW<T> {
    type This = T;
}

impl<T> ConstW<T> {
    fn transform<A, B>(
        x: <Self as Witness<A>>::This,
    ) -> <Self as Witness<B>>::This {
        x
    }
}

pub struct ComposeW<F, G> {
    phantom: std::marker::PhantomData<(F, G)>,
}

impl<F, G, A> Witness<A> for ComposeW<F, G>
where
    F: Witness<A>,
    G: Witness<F::This>,
{
    type This = G::This;
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
    fn fmap<A, B, F>(this: Apply<Self, A>, _: F) -> Apply<Self, B>
    where
        Self: Witness<A> + Witness<B>,
        F: Fn(A) -> B,
    {
        Apply::inj(Self::transform::<A, B>(this.prj()))
    }
}

impl<T> Functor for ConstW<T> {
    fn fmap<A, B, F>(this: Apply<Self, A>, _: F) -> Apply<Self, B>
    where
        Self: Witness<A> + Witness<B>,
        F: Fn(A) -> B,
    {
        Apply::inj(Self::transform::<A, B>(this.prj()))
    }
}
