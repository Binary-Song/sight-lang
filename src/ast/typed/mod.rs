// mod binding;
// mod expr;
// mod pattern;
// mod stmt;
mod r#type;
use trait_set::trait_set;
use crate::container::Arena;
use crate::container::Id;
/// A pointer to binding. When it is referred to as the "binding head",
/// this means it is supposed to be the head of a linked list/tree
/// (like in git) .

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintKind {
    Let,
    App,
    Unify,
}
