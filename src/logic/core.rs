use std::collections::HashMap;


#[derive(Debug, Clone, PartialEq)]
pub struct UnifyError {
    pub msg: String
}

pub trait Unifiable {
    type Key;
    fn unify(&self, other: &Self) -> Result<HashMap<Self::Key, Self>, UnifyError>
        where Self: Sized;
}
