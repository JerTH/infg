//! Fully type erased structures

use std::any::TypeId;
use std::marker::PhantomData;
use std::alloc::Layout;

trait ErasedCollection {
    fn new<K, V>() -> Self;
    fn insert<K, V>(&mut self, key: &K, value: V);
    fn remove<K, V>(&mut self, key: &K) -> Option<V>;
    fn get<K, V>(&self, key: &K) -> &V;
    fn move_element<K>(&mut self, dest: &mut Self, index: K);
}

#[derive(Debug)]
pub struct HomogeneousMap {
    key_type: TypeId,
    val_type: TypeId,
    layout: Layout,
}

impl ErasedCollection for HomogeneousMap {
    fn new<K, V>() -> Self {
        todo!()
    }

    fn insert<K, V>(&mut self, key: &K, value: V) {
        todo!()
    }

    fn remove<K, V>(&mut self, key: &K) -> Option<V> {
        todo!()
    }

    fn get<K, V>(&self, key: &K) -> &V {
        todo!()
    }

    fn move_element<K>(&mut self, dest: &mut Self, index: K) {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_new() {
        let hmap = HomogeneousMap::new::<usize, i32>();

    }

    #[test]
    fn test_get() {

    }

    #[test]
    fn test_move_element() {

    }
}
