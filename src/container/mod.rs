//! Module for [`Id`]-based [`Container`]s.
//!
//! An element inside a Container is called an [`Item`], and is
//! identified by an [`Id`]. You can use [`Container::encode`] and
//!  [`Container::decode`] to convert between `Item` and `Id`.
//!
//! Containers are suitable for storing graphs since it does not
//! contain references or lifetimes.
//!
//! Items in a container are dropped together when the container is dropped.
//! But currently we do not have a mechanism to invalidate the [`Id`]s
//! out in the wild, so be careful.
//!
mod arena;
mod interner;
mod sum;

pub use arena::Arena;
pub use interner::Interner;
pub use sum::SumContainer;
pub use sum::SumId;

use crate::LiteralValue;
use core::fmt;
use std::{fmt::Debug, hash::Hash, marker::PhantomData};
/// Move from `src` of type `Src` into type `Dst`.
///
/// `Src` and `Dst` are assumed to have the same type.
/// Otherwise it is **undefined behavior**.
pub unsafe fn cast<Src, Dst>(src: Src) -> Dst {
    let dst: Dst = std::mem::transmute_copy(&src);
    std::mem::forget(src); // Prevent double drop
    dst
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DecodeError {
    UnsupportedType,
    InvalidId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EncodeError<I: Item> {
    /// Unsupported type. The item is refunded as-is in the error.
    UnsupportedType(I),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RebindError<I: Item> {
    /// Unsupported type. The item is refunded as-is in the error.
    UnsupportedType(I),
    InvalidId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Error<I> {
    /// Unsupported type. The item is refunded as-is in the error.
    UnsupportedType(I),
    InvalidId,
}

/// An item in the [Container].
pub trait Item: Debug + Sized + Clone + Eq + Hash + 'static {}
impl<T: Debug + Sized + Clone + Eq + Hash + 'static> Item for T {}

/// A [`Container`] can hold [`Item`]s of any type.
///
/// Containers can only grow monotonically, meaning items can only be added, not removed.
///
/// Containers are in general heterogeneous, meaning they can hold items of different types.
/// But they can also reject some types at runtime by returning an [`Error::UnsupportedType`].
///
/// For more information, read [container](`crate::container`).
///
/// Design Note: We do NOT wish to have the item type as a generic parameter.
/// Because this will make the recursive types (e.g. expr nodes) take an unreadable
/// amount of type parameters which often ends up referring to Self and freaking
/// out the compiler.
/// As a reminder, look at one of the atrocities that we created going down that route:
/// ```ignore
/// #[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
/// pub struct ApplicationExpr<'c, A: Arena<Self>> {
///     pub callee: Id<'c, Expr<'c, A>, A>,
///     pub arg: Id<'c, Expr<'c, A>, A>,
///     pub ty: TypeId,
///     pub span: Option<Span>,
/// }
/// ```
/// For that reason, we also do not wish to have any lifetime parameters in the Containers
/// and the Id's.
/// Instead, we give up on some safety by removing the lifetime parameter, making
/// it possible to dereference an Id on a different container than the one it was created from,
/// and also give up on some performance by checking whether the container can accept
/// the type of the item at runtime.
pub trait Container: Sized + Debug {
    /// Decode the ID to get the pointed-to value.
    ///
    #[doc = include_str!("doc/_ex.md")]
    fn decode_ex<I: Item>(&self, id: Id<I>) -> Result<I, DecodeError>;
    /// Encode the item to get an ID that points to it.
    ///
    #[doc = include_str!("doc/_ex.md")]
    fn encode_ex<I: Item>(&mut self, item: I) -> Result<Id<I>, EncodeError<I>>;
    /// Rebind an ID to a new value of the same type.
    ///
    #[doc = include_str!("doc/_ex.md")]
    #[must_use]
    fn rebind_ex<I: Item>(&mut self, id: Id<I>, item: I) -> Result<(), RebindError<I>>;

    /// Decode the ID to get the pointed-to value.
    #[inline(always)]
    fn decode<I: Item>(&self, id: Id<I>) -> Option<I> {
        match self.decode_ex(id) {
            Ok(item) => Some(item),
            Err(x) => None,
        }
    }
    /// Encode the item to get an ID that points to it.
    #[must_use]
    #[inline(always)]
    fn encode<I: Item>(&mut self, item: I) -> Option<Id<I>> {
        match self.encode_ex(item) {
            Ok(id) => Some(id),
            Err(_) => None,
        }
    }
    /// Rebind an ID to a new value of the same type.
    #[must_use]
    #[inline(always)]
    fn rebind<I: Item>(&mut self, id: Id<I>, item: I) -> Option<()> {
        match self.rebind_ex(id, item) {
            Ok(()) => Some(()),
            Err(_) => None,
        }
    }

    /// Decode the ID to get the pointed-to value.
    ///
    #[doc = include_str!("doc/_f.md")]
    #[inline(always)]
    fn decode_f<I: Item>(&self, id: Id<I>) -> I {
        self.decode(id)
            .expect("Failed to decode ID using the container")
    }
    /// Encode the item to get an ID that points to it.
    ///
    #[doc = include_str!("doc/_f.md")]
    #[inline(always)]
    fn encode_f<I: Item>(&mut self, item: I) -> Id<I> {
        self.encode(item)
            .expect("Failed to encode item using the container")
    }
    /// Rebind an ID to a new value of the same type.
    ///
    #[doc = include_str!("doc/_f.md")]
    #[inline(always)]
    fn rebind_f<I: Item>(&mut self, id: Id<I>, item: I) {
        self.rebind(id, item)
            .expect("Failed to rebind ID using the container");
    }
}

/// A pointer to an [`Item`] in a [`Container`].
///
/// To get an [`Id`], you usually need to store an `Item` into a `Container`, and
/// the `Container` will return an `Id` that points to the `Item`.
///
/// Currently the [`Container`] can be an [`Arena`](super::Arena) or [`Interner`](super::Interner).
/// The internal usize is only visible inside the `container` module.
/// This is intentional to prevent creation of invalid Ids.
pub struct Id<I: Item>(pub(super) usize, pub(super) PhantomData<(I,)>);

impl<I: Item> Id<I> {
    /// Same as [`Container::decode_ex`]
    ///
    #[inline(always)]
    pub fn decode_ex<C: Container>(self, container: &C) -> Result<I, DecodeError> {
        container.decode_ex(self)
    }
    /// Same as [`Container::decode_f`]
    ///
    #[inline(always)]
    pub fn decode_f<C: Container>(self, container: &C) -> I {
        container.decode_f(self)
    }
    /// Same as [`Container::decode`]
    ///
    #[inline(always)]
    pub fn decode<C: Container>(self, container: &C) -> Option<I> {
        container.decode(self)
    }
}

impl<I: Item> fmt::Debug for Id<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Id({})", self.0)
    }
}

impl<I: Item> Clone for Id<I> {
    fn clone(&self) -> Self {
        Id(self.0, PhantomData)
    }
}

impl<I: Item> Copy for Id<I> {}

impl<I: Item> PartialEq for Id<I> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<I: Item> Eq for Id<I> {}

impl<I: Item> LiteralValue for Id<I> {
    fn literal_value(&self) -> String {
        format!("Id::new({})", self.0)
    }
}

impl<I: Item> Hash for Id<I> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}
