use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct Index<I> {
    _phantom_type: PhantomData<I>,
    value: usize,
}
impl<T> Clone for Index<T> {
    fn clone(&self) -> Index<T> {
        Index {
            value: self.value,
            _phantom_type: PhantomData,
        }
    }
}
impl<T> Copy for Index<T> {}
impl<I> std::fmt::Debug for Index<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Index").field(&self.value).finish()
    }
}
impl<I> From<usize> for Index<I> {
    fn from(i: usize) -> Self {
        Index {
            _phantom_type: PhantomData,
            value: i,
        }
    }
}
impl<I> From<Index<I>> for usize {
    fn from(i: Index<I>) -> Self {
        i.value
    }
}
impl<I> ::core::ops::Add for Index<I> {
    type Output = Index<I>;

    fn add(self, rhs: Index<I>) -> Index<I> {
        Index {
            _phantom_type: PhantomData,
            value: self.value + rhs.value,
        }
    }
}
impl<I> ::core::ops::Sub for Index<I> {
    type Output = Index<I>;

    fn sub(self, rhs: Index<I>) -> Index<I> {
        Index {
            _phantom_type: PhantomData,
            value: self.value - rhs.value,
        }
    }
}
impl<I> ::core::ops::AddAssign for Index<I> {
    fn add_assign(&mut self, rhs: Index<I>) {
        self.value.add_assign(rhs.value);
    }
}
impl<I> ::core::ops::SubAssign for Index<I> {
    fn sub_assign(&mut self, rhs: Index<I>) {
        self.value.sub_assign(rhs.value);
    }
}
impl<I> Hash for Index<I> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}
