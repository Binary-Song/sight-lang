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
mod v2;

pub use v2::Arena;
pub use v2::ArenaItem;
pub use v2::ArenaLike;
pub use v2::Container;
pub use v2::Id;
pub use v2::Interner;
pub use v2::InternerItem;
pub use v2::InternerLike;
pub use v2::Uid;

use crate::LiteralValue;
use core::fmt;
use std::{fmt::Debug, hash::Hash, marker::PhantomData};
