use crate::container::{Container, DecodeError, EncodeError, Id, Item, RebindError};
#[derive(Debug)]
pub struct DoubleContainer<C1: Container, C2: Container> {
    pub container1: C1,
    pub container2: C2,
}

/// Merge multiple containers into a single container.
/// 
/// Example: `multicontainer!(Arena::default(), Interner::default())`
#[macro_export]
macro_rules! multicontainer {
    ($container:expr) => {
        $container
    };
    ($first:expr, $($rest:expr),+) => {
        crate::container::DoubleContainer {
            container1: $first,
            container2: multicontainer!($($rest),+),
        }
    };
}

impl<C1: Container, C2: Container> Container for DoubleContainer<C1, C2> {
    #[inline(always)]
    fn decode_extra<I: Item>(&self, id: Id<I>) -> Result<I, DecodeError> {
        match self.container1.decode_extra(id) {
            Err(DecodeError::UnsupportedType) => self.container2.decode_extra(id),
            Ok(item) => Ok(item),
            Err(e) => Err(e),
        }
    }

    #[inline(always)]
    fn encode_extra<I: Item>(&mut self, item: I) -> Result<Id<I>, EncodeError<I>> {
        match self.container1.encode_extra(item) {
            Err(EncodeError::UnsupportedType(item)) => self.container2.encode_extra(item),
            Ok(item) => Ok(item),
        }
    }

    #[inline(always)]
    fn rebind_extra<I: Item>(&mut self, id: Id<I>, item: I) -> Result<(), RebindError<I>> {
        match self.container1.rebind_extra(id, item) {
            Err(RebindError::UnsupportedType(item)) => self.container2.rebind_extra(id, item),
            Ok(item) => Ok(item),
            Err(e) => Err(e),
        }
    }
}
