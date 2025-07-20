use crate::{ast::typed::TypeId, container::*};

/// A singly linked tree, where you get pointers to parents
/// but not children (like a git history graph)
/// This makes it easy to append defs to any existing def.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Binding<'a, A: Arena<Self>> {
    pub parent: Option<Id<'a, Self, A>>,
    pub data: BindingData<'a, A>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BindingData<'a, A: Arena<Self>> {
    Empty,
    FunctionDecl {
        name: StringId<'a>,
        param_type: TypeId<'a,A>,
        return_type: TypeId<'a,A>,
        body_id: Id<'a, Block<'a, A>, A>,
    },
    Variable {
        name: StringId<'a>,
        ty: TypeId,
    },
}

impl<'a, A: Arena<Self>> Binding<'a, A> {
    pub fn name(&self) -> Option<StringId<'a>> {
        match self.data {
            BindingData::FunctionDecl { name, .. } => Some(name),
            BindingData::Variable { name, .. } => Some(name),
            BindingData::Empty => None,
        }
    }
}

impl<'a, A: Arena<Self>> BindingId<'a, A> {
    /// Creates and returns a new binding whose parent is this binding id.
    pub fn derive(self, data: BindingData<'a, A>) -> Binding<'a, A> {
        Binding {
            parent: Some(self),
            data,
        }
    }
}
