use crate::container::{
    cast, Id, Item, {Container, DecodeError, EncodeError, RebindError},
};
use std::{any::TypeId, marker::PhantomData};

#[derive(Debug)]
/// A homogeneous [`Container`] that allows rebinding of IDs.
pub struct Arena<T: Item> {
    data: Vec<T>,
}

impl<T: Item> Default for Arena<T> {
    fn default() -> Self {
        Self { data: Vec::new() }
    }
}

impl<T: Item> Container for Arena<T> {
    #[inline(always)]
    fn decode_ex<I: Item>(&self, id: Id<I>) -> Result<I, DecodeError> {
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
    fn encode_ex<I: Item>(&mut self, item: I) -> Result<Id<I>, EncodeError<I>> {
        if TypeId::of::<T>() != TypeId::of::<I>() {
            return Err(EncodeError::UnsupportedType(item));
        }
        let new_id = Id(self.data.len(), PhantomData);
        let item_t: T = unsafe { cast::<I, T>(item) };
        self.data.push(item_t);
        Ok(new_id)
    }

    #[inline(always)]
    fn rebind_ex<I: Item>(&mut self, id: Id<I>, item: I) -> Result<(), RebindError<I>> {
        if TypeId::of::<T>() != TypeId::of::<I>() {
            return Err(RebindError::UnsupportedType(item));
        }
        match self.data.get_mut(id.0) {
            Some(slot) => {
                let item_t: T = unsafe { cast::<I, T>(item) };
                *slot = item_t;
                Ok(())
            }
            None => Err(RebindError::InvalidId),
        }
    }
}
