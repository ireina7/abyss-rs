use super::core::*;
use std::ops::BitOr;
use std::ops::Shr;


macro_rules! impl_BitOr {
    ($t:ty) => {
        type Output = Or<Self, $t>;
        fn bitor(self, rhs: $t) -> Self::Output {
            Or::new(self, rhs)
        }
    }
}

macro_rules! impl_Shr {
    ($t:ty) => {
        type Output = AndThen<Self, $t>;
        fn shr(self, rhs: $t) -> Self::Output {
            AndThen::new(self, rhs)
        }
    }
}


impl<T: Parser>            BitOr<T> for Char          { impl_BitOr!(T); }
impl<T: Parser, F>         BitOr<T> for Satisfy<F>    { impl_BitOr!(T); }
impl<T: Parser, A, B>      BitOr<T> for Or<A, B>      { impl_BitOr!(T); }
impl<T: Parser, A, B>      BitOr<T> for And<A, B>     { impl_BitOr!(T); }
impl<T: Parser, P, F>      BitOr<T> for AndThen<P, F> { impl_BitOr!(T); }
impl<T: Parser, P, F>      BitOr<T> for Map<P, F>     { impl_BitOr!(T); }
impl<T: Parser, P>         BitOr<T> for Many<P>       { impl_BitOr!(T); }
impl<T: Parser, P>         BitOr<T> for Many1<P>      { impl_BitOr!(T); }
impl<T: Parser, A: Clone>  BitOr<T> for Pure<A>       { impl_BitOr!(T); }
impl<T: Parser, P: Parser> BitOr<T> for Wrapper<P>    { impl_BitOr!(T); }


impl<B: Parser, F: Fn(char) -> B> Shr<F>    for Char       { impl_Shr!(F); }
impl<B: Parser, F: Fn(char) -> B, G> Shr<F> for Satisfy<G> { impl_Shr!(F); }
impl<A, B, C, F> Shr<F> for Or<A, B> where
    A: Parser,
    B: Parser,
    C: Parser,
    F: Fn(A::Output) -> C
{ impl_Shr!(F); }

impl<A, B, C, F> Shr<F> for And<A, B> where
    A: Parser,
    B: Parser,
    C: Parser,
    F: Fn(B::Output) -> C
{ impl_Shr!(F); }

impl<A, B, G, C, F> Shr<F> for AndThen<A, G> where
    A: Parser,
    B: Parser,
    G: Fn(A::Output) -> B,
    C: Parser,
    F: Fn(B::Output) -> C
{ impl_Shr!(F); }

impl<A, B, G, C, F> Shr<F> for Map<A, G> where
    A: Parser,
    G: Fn(A::Output) -> B,
    C: Parser,
    F: Fn(B) -> C
{ impl_Shr!(F); }

impl<P, C, F> Shr<F> for Many<P> where
    P: Parser,
    C: Parser,
    F: Fn(Vec<P::Output>) -> C
{ impl_Shr!(F); }

impl<P, C, F> Shr<F> for Many1<P> where
    P: Parser,
    C: Parser,
    F: Fn(Vec<P::Output>) -> C
{ impl_Shr!(F); }

impl<A, C, F> Shr<F> for Pure<A> where
    A: Clone,
    C: Parser,
    F: Fn(A) -> C
{ impl_Shr!(F); }

impl<P, C, F> Shr<F> for Logger<P> where
    P: Parser,
    C: Parser,
    F: Fn(P::Output) -> C
{ impl_Shr!(F); }

impl<P, C, F> Shr<F> for Wrapper<P> where
    P: Parser,
    C: Parser,
    F: Fn(P::Output) -> C
{ impl_Shr!(F); }
