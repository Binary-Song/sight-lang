use crate::{
    ast::typed::{expr::Block, ty::Type},
    container::*,
};

/// A singly linked tree, where you get pointers to parents
/// but not children (like a git history graph)
/// This makes it easy to append defs to any existing def.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Binding {
    pub parent: Option<Id<Binding>>,
    pub data: BindingData,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BindingData {
    Empty,
    FunctionDecl {
        name: Id<String>,
        param_tys: Vec<Id<Type>>,
        ret_ty: Id<Type>,
        body_id: Option<Id<Block>>,
    },
    Variable {
        name: Id<String>,
        ty: Id<Type>,
    },
}

impl BindingData {
    pub fn name(&self) -> Option<Id<String>> {
        match self {
            BindingData::FunctionDecl { name, .. } => Some(*name),
            BindingData::Variable { name, .. } => Some(*name),
            BindingData::Empty => None,
        }
    }
}

impl Id<Binding> {
    /// Creates and returns a new binding whose parent is this binding id.
    pub fn derive(self, data: BindingData) -> Binding {
        Binding {
            parent: Some(self),
            data,
        }
    }
}
