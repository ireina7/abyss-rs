use std::collections::HashMap;


pub trait Unifiable {
    type Key;
    fn unify(&self, other: &Self) -> HashMap<Self::Key, Self>
        where Self: Sized;
}
