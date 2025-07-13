use crate::ast::typed::{Arena, HasIdMapInArena};
use crate::LiteralValue;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::marker::PhantomData;

/// Basically a pointer without the complexity of lifetimes. You need an [Arena] to deref it.
pub struct Id<T: HasIdMapInArena>(usize, PhantomData<T>);

impl<T: HasIdMapInArena> Id<T> {
    pub fn new(id: usize) -> Self {
        Id(id, PhantomData)
    }

    pub fn deref<'a>(self, arena: &'a Arena) -> Option<&'a T> {
        T::get_id_map(arena).get(self)
    }
}

impl<T: HasIdMapInArena> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Id::<{}>({})", std::any::type_name::<T>(), self.0)
    }
}

impl<T: HasIdMapInArena> Clone for Id<T> {
    fn clone(&self) -> Self {
        Self::new(self.0)
    }
}

impl<T: HasIdMapInArena> Copy for Id<T> {}

impl<T: HasIdMapInArena> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}

impl<T: HasIdMapInArena> Eq for Id<T> {}

impl<T: HasIdMapInArena> LiteralValue for Id<T> {
    fn literal_value(&self) -> String {
        format!("Id::new({})", self.0)
    }
}

impl<T: HasIdMapInArena> Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

pub struct IdMap<T> {
    map: Vec<Option<T>>,
}

impl<T: HasIdMapInArena> Default for IdMap<T> {
    fn default() -> Self {
        IdMap::new()
    }
}

impl<T: HasIdMapInArena> IdMap<T> {
    pub fn new() -> Self {
        IdMap { map: Vec::new() }
    }

    fn make_id(&self, index: usize) -> Id<T> {
        Id(index, PhantomData)
    }

    fn unpack_id(&self, id: Id<T>) -> usize {
        id.0
    }

    pub fn push(&mut self, value: T) -> Id<T> {
        self.map.push(Some(value));
        self.make_id(self.map.len() - 1)
    }

    pub fn push_none(&mut self) -> Id<T> {
        self.map.push(None);
        self.make_id(self.map.len() - 1)
    }

    pub fn get(&self, id: Id<T>) -> Option<&T> {
        match self.map.get(self.unpack_id(id)) {
            None => None,
            Some(Some(value)) => Some(value),
            Some(None) => None,
        }
    }

    pub fn get_mut(&mut self, id: Id<T>) -> Option<&mut T> {
        let id = self.unpack_id(id);
        match self.map.get_mut(id) {
            None => None,
            Some(Some(value)) => Some(value),
            Some(None) => None,
        }
    }

    pub fn set(&mut self, id: Id<T>, value: T) {
        let index = self.unpack_id(id);
        self.map[index] = Some(value);
    }

    pub fn set_none(&mut self, id: Id<T>) {
        let index = self.unpack_id(id);
        self.map[index] = None;
    }

    pub fn iter(&self) -> impl Iterator<Item = (Id<T>, &T)> {
        self.map.iter().enumerate().filter_map(|(index, value)| {
            if let Some(value) = value {
                Some((self.make_id(index), value))
            } else {
                None
            }
        })
    }
}
