use crate::{ast::typed::*, utils::interning::InternString};

/// A singly linked tree, where you get pointers to parents
/// but not children (like a git history graph)
/// This makes it easy to append defs to any existing def.
pub struct Binding {
    pub parent: Option<BindingId>,
    pub data: BindingData,
}

pub enum BindingData {
    Empty,
    FunctionDecl {
        name: InternString,
        param_type: TypeId,
        return_type: TypeId,
        body_id: BodyId,
    },
    Variable {
        name: InternString,
        ty: TypeId,
    },
}

impl Binding {
    pub fn name(&self) -> Option<InternString> {
        match self.data {
            BindingData::FunctionDecl { name, .. } => Some(name),
            BindingData::Variable { name, .. } => Some(name),
            BindingData::Empty => None,
        }
    }
}

impl BindingId {
    /// Creates and returns a new binding whose parent is this binding id.
    pub fn derive(self, data: BindingData) -> Binding {
        Binding {
            parent: Some(self),
            data,
        }
    }
}
