use core::fmt;
use std::{marker::PhantomData, ops::Index};

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Idx<T> {
    pub raw: usize,
    __tag: PhantomData<T>,
}

impl<T> Idx<T> {
    pub fn new(raw: usize) -> Self {
        raw.into()
    }
}
impl<T> fmt::Debug for Idx<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Idx").field(&self.raw).finish()
    }
}

impl<T> From<usize> for Idx<T> {
    fn from(raw: usize) -> Self {
        Idx {
            raw,
            __tag: PhantomData,
        }
    }
}

impl<T> Index<Idx<T>> for Vec<T> {
    type Output = T;

    fn index(&self, idx: Idx<T>) -> &Self::Output {
        &self[idx.raw]
    }
}
impl<T> Index<Idx<T>> for [T] {
    type Output = T;

    fn index(&self, idx: Idx<T>) -> &Self::Output {
        &self[idx.raw]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(PartialEq, Debug)]
    struct Thing(usize);

    #[test]
    fn test() {
        let mut things: Vec<Thing> = Vec::new();
        things.push(Thing(0));
        let idx: Idx<Thing> = 0.into();
        assert_eq!(Thing(0), things[idx]);
        assert_eq!(Thing(0), things[Idx::new(0)]);
    }
}
