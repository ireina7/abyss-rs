//! Environment of Abyss programming
use std::collections::HashMap;
use std::hash::Hash;
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

    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        self.env.insert(k, v)
    }

    pub fn contains_key(&self, k: &K) -> bool {
        self.env.contains_key(k)
    }

    pub fn iter(&self) -> impl Iterator + '_ {
        self.env.iter()
    }

    pub fn keys(&self) -> impl Iterator + '_ {
        self.env.keys()
    }
}



