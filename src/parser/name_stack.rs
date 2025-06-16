use crate::{
    ast::typed::{self, Name, QualifiedName},
    parser::PropertyStack,
};

pub struct NameStack {
    stack: Vec<Name>,
}

impl Default for NameStack {
    fn default() -> Self {
        NameStack::new()
    }
}

impl NameStack {
    pub fn new() -> Self {
        NameStack { stack: Vec::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn push(&mut self, name: Name) {
        self.stack.push(name)
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn prefix(&self) -> QualifiedName {
        self.stack
            .iter()
            .fold(QualifiedName::new(), |mut acc, name| {
                acc.names.push(name.clone());
                acc
            })
    }

    pub fn qualify(&self, suffix: typed::Name) -> QualifiedName {
        let mut qualified = self.prefix();
        qualified.names.push(suffix);
        qualified
    }
}

impl Drop for NameStack {
    fn drop(&mut self) {
        if !self.stack.is_empty() {
            panic!(
                "Logic error: NameStack should be empty on drop. Push/pop imbalance is a serious bug!"
            );
        }
    }
}
