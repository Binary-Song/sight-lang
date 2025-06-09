use crate::ast::typed::Type;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bindable {
    Var(Type),
    Func(Type),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding(pub String, pub Bindable);

#[derive(Clone)]
pub enum Context<'p> {
    Basic {
        bindings: Vec<Binding>,
        parent: Option<&'p Context<'p>>,
    },
    FnFilter {
        filter: fn(&Binding) -> bool,
        parent: &'p Context<'p>,
    },
    ClosureFilter {
        filter: Rc<dyn Fn(&Binding) -> bool>,
        parent: &'p Context<'p>,
    },
}

#[derive(Clone)]
pub struct ContextIter<'a> {
    ctx: &'a Context<'a>,
    rev_index: usize,
}

enum GetResult<T> {
    Ok(T),
    Filtered,
    OutOfBounds,
}

impl<'a> Context<'a> {
    pub fn new() -> Context<'a> {
        Context::Basic {
            bindings: vec![],
            parent: None,
        }
    }

    pub fn make_child(&'a self) -> Context<'a> {
        Context::Basic {
            bindings: vec![],
            parent: Some(self),
        }
    }

    pub fn add_binding(&self, binding: Binding) -> Context {
        Context::Basic {
            bindings: vec![binding],
            parent: Some(self),
        }
    }

    pub fn add_bindings(&self, bindings: Vec<Binding>) -> Context {
        Context::Basic {
            bindings,
            parent: Some(self),
        }
    }

    pub fn add_fn_filter<F>(&self, filter: fn(&Binding) -> bool) -> Context {
        Context::FnFilter {
            filter,
            parent: self,
        }
    }

    pub fn add_closure_filter<F>(&self, filter: Rc<dyn Fn(&Binding) -> bool>) -> Context {
        Context::ClosureFilter {
            filter,
            parent: self,
        }
    }

    /// Create an iterator that iterates over `&Binding` s
    pub fn iter(&self) -> ContextIter {
        ContextIter {
            ctx: self,
            rev_index: 0,
        }
    }

    pub fn get(&self, rev_index: usize) -> GetResult<&Binding> {
        match self {
            Context::Basic { bindings, parent } => {
                if let Some(r) = bindings.get(bindings.len() - 1 - rev_index) {
                    GetResult::Ok(r)
                } else {
                    match parent {
                        Some(p) => {
                            let index = rev_index - bindings.len();
                            p.get(index)
                        }
                        None => GetResult::OutOfBounds,
                    }
                }
            }
            Context::FnFilter { filter, parent } => match parent.get(rev_index) {
                GetResult::Ok(binding) => {
                    if filter(binding) {
                        GetResult::Ok(binding)
                    } else {
                        GetResult::Filtered
                    }
                }
                others => others,
            },
            Context::ClosureFilter { filter, parent, .. } => match parent.get(rev_index) {
                GetResult::Ok(binding) => {
                    if (*filter)(binding) {
                        GetResult::Ok(binding)
                    } else {
                        GetResult::Filtered
                    }
                }
                others => others,
            },
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Context::Basic { bindings, parent } => bindings.len() + parent.map_or(0, |p| p.len()),
            Context::FnFilter { parent, .. } => parent.len(),
            Context::ClosureFilter { parent, .. } => parent.len(),
        }
    }

    pub fn find_by_name(&self, name: &str) -> Option<&Binding> {
        let mut i = 0;
        loop {
            match self.get(i) {
                GetResult::Ok(binding) => {
                    if binding.0 == name {
                        return Some(binding);
                    }
                }
                GetResult::Filtered => (),
                GetResult::OutOfBounds => return None,
            }
            i += 1;
        }
    }
}

impl<'a> Iterator for ContextIter<'a> {
    type Item = &'a Binding;
    fn next(&mut self) -> Option<Self::Item> {
        let binding = self.ctx.get(self.rev_index);
        self.rev_index += 1;
        match binding {
            GetResult::Ok(b) => Some(b),
            GetResult::Filtered => self.next(),
            GetResult::OutOfBounds => None,
        }
    }
}

impl<'a> IntoIterator for &'a Context<'a> {
    type IntoIter = ContextIter<'a>;
    type Item = &'a Binding;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl Bindable {
    pub fn get_type(&self) -> Type {
        match self {
            Bindable::Var(ty) => ty.clone(),
            Bindable::Func(ty) => ty.clone(),
        }
    }
}
