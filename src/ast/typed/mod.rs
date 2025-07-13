mod arena;
mod binding;
mod expr;
mod pattern;
mod stmt;
mod r#type;

pub use arena::*;
pub use binding::*;
pub use expr::*;
pub use pattern::*;
pub use r#type::*;
pub use stmt::*;

use crate::ast::id::Id;
/// A pointer to binding. When it is referred to as the "binding head",
/// this means it is supposed to be the head of a linked list/tree
/// (like in git) .
pub type BindingId = Id<Binding>;
pub type Body = Block;
pub type BodyId = Id<Body>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintKind {
    Let,
    App,
    Unify,
}
