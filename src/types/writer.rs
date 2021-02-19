pub trait MonoidMut {
    fn add(&mut self, other: Self) -> ();

    fn mempty() -> Self;
}

pub struct Context<W>(W);

impl<W: MonoidMut> Context<W> {
    pub fn new(v: W) -> Self {
        Context(v)
    }

    pub fn write(&mut self, v: W) {
        self.0.add(v);
    }

    pub fn join(&mut self, errs: Self) {
        self.write(errs.0);
    }

    pub fn extract(self) -> W {
        self.0
    }

    pub fn map<F, W2>(self, f: F) -> Context<W2>
    where
        F: FnOnce(W) -> W2,
    {
        Context(f(self.0))
    }
}

pub struct Writer<T, W> {
    value: T,
    context: Context<W>,
}

macro_rules! impl_and_then_vararg {
    ( $t:ident, $w:ident, $name:ident, $($argname:ident: $tyname:ident),+ ) => {
        fn $name<$($tyname),+, T2, FnTy>(
            self, f:FnTy,
            $($argname : $tyname),+
        ) -> Writer<T2, W>
        where
            FnTy: FnOnce($($tyname),+, $t, &mut Context<$w>) -> T2
        {
            #[allow(unused_parens)]
            self.and_then_generic(
                |($($argname),+), t, ctx| f($($argname),+,t,ctx),
                ($($argname),+)
            )
        }
    };
}

#[allow(dead_code)]
impl<T, W: MonoidMut> Writer<T, W> {
    pub fn pure(value: T) -> Self {
        Writer {
            value,
            context: Context(W::mempty()),
        }
    }

    pub fn with_context(value: T, context: Context<W>) -> Self {
        Writer { value, context }
    }

    pub fn bind<T2, F>(mut self, f: F) -> Writer<T2, W>
    where
        F: FnOnce(T) -> Writer<T2, W>,
    {
        let Writer { value, context } = f(self.value);

        self.context.join(context);

        Writer {
            value,
            context: self.context,
        }
    }

    pub fn and_then_generic<Args, T2, F>(mut self, f: F, args: Args) -> Writer<T2, W>
    where
        F: FnOnce(Args, T, &mut Context<W>) -> T2,
    {
        let value = f(args, self.value, &mut self.context);

        Writer {
            value,
            context: self.context,
        }
    }

    pub fn and_then<T2, F>(mut self, f: F) -> Writer<T2, W>
    where
        F: FnOnce(T, &mut Context<W>) -> T2,
    {
        let value = f(self.value, &mut self.context);

        Writer {
            value,
            context: self.context,
        }
    }

    pub fn and_then_mut<F>(&mut self, f: F) -> ()
    where
        F: FnOnce(&mut T, &mut Context<W>) -> (),
    {
        f(&mut self.value, &mut self.context);
    }

    impl_and_then_vararg!(T, W, and_then2, a: A);
    impl_and_then_vararg!(T, W, and_then3, a: A, b: B);
    impl_and_then_vararg!(T, W, and_then4, a: A, b: B, c: C);
    impl_and_then_vararg!(T, W, and_then5, a: A, b: B, c: C, d: D);
    impl_and_then_vararg!(T, W, and_then6, a: A, b: B, c: C, d: D, e: E);

    pub fn extract(self) -> (T, W) {
        (self.value, self.context.extract())
    }
}

impl<T> MonoidMut for Vec<T> {
    fn add(&mut self, other: Self) -> () {
        self.extend(other);
    }

    fn mempty() -> Self {
        Vec::new()
    }
}

pub type Logger<T, E> = Writer<T, Vec<E>>;
pub type LoggerContext<E> = Context<Vec<E>>;

impl<E> LoggerContext<E> {
    pub fn log(&mut self, e: E) -> () {
        self.write(vec![e]);
    }

    pub fn log_many(&mut self, es: Vec<E>) -> () {
        self.write(es);
    }
}

impl<T, E> Logger<T, E> {
    pub fn log(&mut self, e: E) -> () {
        self.context.log(e);
    }

    pub fn log_many(&mut self, es: Vec<E>) -> () {
        self.context.log_many(es);
    }
}
