mod binding;
mod expr;
// mod pattern;
mod stmt;
mod ty;
pub use binding::*;
pub use expr::*;
pub use stmt::*;
pub use ty::*;
use crate::container::Container;
use crate::container::Id;

pub trait GetTy {
    fn get_ty(&self, c: &mut impl Container) -> Id<Type>;
}

