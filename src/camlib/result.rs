
pub trait CommonBind<T, E> {
    fn common_bind<U, E2, F: FnOnce(T) -> Result<U, E2>>(self, op: F)
            -> Result<U, E>
        where E: std::convert::From<E2>;
}

impl<T, E1, E: std::convert::From<E1>> CommonBind<T, E> for Result<T, E1> {
    fn common_bind<U, E2, F: FnOnce(T) -> Result<U, E2>>(self, op: F)
            -> Result<U, E>
        where E: std::convert::From<E2>
    {
        match self {
            Ok(t) => op(t).map_err(From::from),
            Err(e) => Err(From::from(e)),
        }
    }
}


