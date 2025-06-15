use crate::{ast::typed::ScopeName, parser::PropertyStack};

pub struct NameStack {
    stack: PropertyStack<ScopeName>,
}

impl Default for NameStack {
    fn default() -> Self {
        NameStack::new(ScopeName::Index(0))
    }
}

impl NameStack {
    pub fn new(init: ScopeName) -> Self {
        NameStack {
            stack: PropertyStack::new(init),
        }
    }

    pub fn push(&mut self, name: ScopeName) {
        self.stack.push(name)
    }

    pub fn pop(&mut self) {
        self.stack.pop()
    }

    pub fn set(&mut self, name: ScopeName) {
        self.stack.set(name)
    }

    pub fn prefix(&self) -> String {
        self.stack.iter().fold(String::new(), |mut acc, name| {
            if !acc.is_empty() {
                acc.push('.');
            }
            match name {
                ScopeName::Name(n) => acc.push_str(&n),
                ScopeName::Index(i) => acc.push_str(&i.to_string()),
            }
            acc
        })
    }

    pub fn qualify_name(&self, suffix: &str) -> String {
        format!("{}.{}", self.prefix(), suffix)
    }

    pub fn guarded_push<R>(&mut self, value: ScopeName, scope: impl FnOnce() -> R) -> R {
        self.push(value);
        let result = scope();
        self.pop();
        result
    }
}
