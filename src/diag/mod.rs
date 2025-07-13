use crate::{ast::typed, sema::typing, utils::interning::Interner, LiteralValue};
use typing::TypeError;

pub fn print_error(e: typing::TypeError, code: &str, type_interner: &mut Interner<typed::Type>) -> String {
    match e {
        TypeError::UnboundVar { name, span } => {
            format!("Unbound variable `{}`", name.static_unintern(),)
        }
        TypeError::DuplicateBinding { binding } =>  {
            format!("Duplicate `{}`", binding.literal_value(),)
        }
        TypeError::CannotUnify { lhs, rhs } =>   {
            format!("Cannot unify `{:?}` `{:?}`", lhs.unintern(type_interner), rhs.unintern(type_interner),)
        }
    }
}