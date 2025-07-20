use crate::container::{cast, Container, Id, Item};
use crate::container::{DecodeError, EncodeError, RebindError};
use std::any::TypeId;
use std::{collections::HashMap, marker::PhantomData};

/// A homogeneous [`Container`] that stores equal items
/// only once and returns an [`Id`] for each unique item.
#[derive(Debug)]
pub struct Interner<T: Item> {
    data: Vec<T>,
    indices: HashMap<T, usize>,
}

impl<T: Item> Default for Interner<T> {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            indices: HashMap::new(),
        }
    }
}

impl<T: Item> Container for Interner<T> {
    #[inline(always)]
    fn decode_extra<I: Item>(&self, id: Id<I>) -> Result<I, DecodeError> {
        if TypeId::of::<T>() != TypeId::of::<I>() {
            return Err(DecodeError::UnsupportedType);
        }
        let index = id.0;
        match self.data.get(index) {
            Some(item) => {
                let item_t: T = item.clone();
                let item_i: I = unsafe { cast::<T, I>(item_t) };
                Ok(item_i)
            }
            None => Err(DecodeError::InvalidId),
        }
    }

    #[inline(always)]
    fn encode_extra<I: Item>(&mut self, item: I) -> Result<Id<I>, EncodeError<I>> {
        if TypeId::of::<T>() != TypeId::of::<I>() {
            return Err(EncodeError::UnsupportedType(item));
        }
        let item = unsafe { cast::<I, T>(item) };
        if let Some(&index) = self.indices.get(&item) {
            Ok(Id(index, PhantomData))
        } else {
            let index = self.data.len();
            self.data.push(item.clone());
            self.indices.insert(item, index);
            Ok(Id(index, PhantomData))
        }
    }

    #[inline(always)]
    fn rebind_extra<I: Item>(&mut self, id: Id<I>, item: I) -> Result<(), RebindError<I>> {
        Err(RebindError::UnsupportedType(item))
    }
}
