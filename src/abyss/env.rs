//! Environment of Abyss programming
use std::collections::HashMap;
use std::hash::Hash;
use std::borrow::Borrow;
//use super::object::Object;


#[allow(dead_code)]
pub struct Environment<K, V> {
    env: HashMap<K, V>
}

#[allow(dead_code)]
impl<K, V> Environment<K, V> 
where
    K: Eq + Hash
{
    pub fn new() -> Self {
        Self { env: HashMap::new() }
    }

    pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.env.get(k)
    }

    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        self.env.insert(k, v)
    }

    pub fn contains_key<Q: ?Sized>(&self, k: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.env.contains_key(k)
    }

    pub fn iter(&self) -> impl Iterator + '_ {
        self.env.iter()
    }

    pub fn keys(&self) -> impl Iterator + '_ {
        self.env.keys()
    }
}



impl<K, V> Clone for Environment<K, V>
where
    K: Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        Environment { env: self.env.clone() }
    }
}

impl<K, V> Extend<(K, V)> for Environment<K, V>
where
    K: Eq + Hash,
{
    fn extend<T: IntoIterator<Item=(K, V)>>(&mut self, iter: T) {
        self.env.extend(iter)
    }
}


