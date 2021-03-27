use super::object::Object;
use crate::logic::*;
use std::collections::HashMap;


impl Unifiable for Object {

    type Key = Object;
    fn unify(&self, other: &Self) -> HashMap<Self::Key, Self>
    where Self: Sized {
        use Object::*;
        let mut _ans = HashMap::new();
        match (self, other) {
            _ => { }
        }
        _ans
    }
}
