use crate::ast::typed::Type;
use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bindable {
    Var(Type),
    Func(Type),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constraint {
    pub lhs: Type,
    pub rhs: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding(pub String, pub Bindable);

#[derive(Clone)]
pub enum Context {
    Basic {
        bindings: Vec<Binding>,
        parent: Option<Rc<Context>>,
        next_type_var: Rc<RefCell<u32>>,
        constraints: Rc<RefCell<Vec<Constraint>>>,
    },
    FnFilter {
        filter: fn(&Binding) -> bool,
        parent: Rc<Context>,
        next_type_var: Rc<RefCell<u32>>,
        constraints: Rc<RefCell<Vec<Constraint>>>,
    },
    ClosureFilter {
        filter: Rc<dyn Fn(&Binding) -> bool>,
        parent: Rc<Context>,
        next_type_var: Rc<RefCell<u32>>,
        constraints: Rc<RefCell<Vec<Constraint>>>,
    },
}

impl std::fmt::Debug for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Context::Basic {
                bindings, parent, ..
            } => {
                write!(
                    f,
                    "Context::Basic {{ bindings: {:?}, parent: {:?} }}",
                    bindings, parent
                )
            }
            Context::FnFilter {
                filter: _, parent, ..
            } => {
                write!(f, "Context::FnFilter {{ parent: {:?} }}", parent)
            }
            Context::ClosureFilter {
                filter: _, parent, ..
            } => {
                write!(f, "Context::ClosureFilter {{ parent: {:?} }}", parent)
            }
        }
    }
}

#[derive(Clone)]
pub struct ContextIter<'a> {
    ctx: &'a Context,
    rev_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GetResult<T: std::fmt::Debug> {
    Ok(T),
    Filtered,
    OutOfBounds,
}

impl Context {
    pub fn new() -> Rc<Self> {
        Rc::new(Context::Basic {
            bindings: vec![],
            parent: None,
            next_type_var: Rc::new(RefCell::new(0)),
            constraints: Rc::new(RefCell::new(vec![])),
        })
    }

    pub fn make_child(self: Rc<Self>) -> Rc<Self> {
         self.add_bindings(vec![])
    }

    pub fn add_binding(self: Rc<Self>, binding: Binding) -> Rc<Self> {
        self.add_bindings(vec![binding])
    }

    pub fn add_bindings(self: Rc<Self>, bindings: Vec<Binding>) -> Rc<Self> {
        Rc::new(Context::Basic {
            bindings,
            next_type_var: self.next_type_var().clone(),
            constraints: self.constraints().clone(),
            parent: Some(self),
        })
    }

    pub fn add_fn_filter(self: Rc<Self>, filter: fn(&Binding) -> bool) -> Rc<Self> {
        Rc::new(Context::FnFilter {
            filter,
            next_type_var: self.next_type_var().clone(),
            constraints: self.constraints().clone(),
            parent: self,
        })
    }

    pub fn add_closure_filter(self: Rc<Self>, filter: Rc<dyn Fn(&Binding) -> bool>) -> Rc<Self> {
        Rc::new(Context::ClosureFilter {
            filter,
            next_type_var: self.next_type_var().clone(),
            constraints: self.constraints().clone(),
            parent: self,
        })
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
            Context::Basic {
                bindings, parent, ..
            } => {
                if rev_index < bindings.len() {
                    let item = bindings.get(bindings.len() - 1 - rev_index);
                    GetResult::Ok(item.unwrap())
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
            Context::FnFilter { filter, parent, .. } => match parent.get(rev_index) {
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
            Context::Basic {
                bindings, parent, ..
            } => bindings.len() + parent.as_ref().map_or(0, |p| p.len()),
            Context::FnFilter { parent, .. } => parent.len(),
            Context::ClosureFilter { parent, .. } => parent.len(),
        }
    }

    pub fn find_self_by_name(&self, name: &str) -> Option<&Binding> {
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

    pub fn constraints(&self) -> Rc<RefCell<Vec<Constraint>>> {
        match self {
            Context::Basic { constraints, .. }
            | Context::FnFilter { constraints, .. }
            | Context::ClosureFilter { constraints, .. } => constraints.clone(),
        }
    }
    
    pub fn add_constraint(&self, constraint: Constraint)
    {
        self.constraints().borrow_mut().push(constraint);
    }

    pub fn next_type_var(&self) -> Rc<RefCell<u32>> {
        match self {
            Context::Basic { next_type_var, .. }
            | Context::FnFilter { next_type_var, .. }
            | Context::ClosureFilter { next_type_var, .. } => next_type_var.clone(),
        }
    }

    pub fn fresh_var(&self) -> u32 {
        let next_var = self.next_type_var();
        let mut val = next_var.borrow_mut();
        let current = *val;
        *val += 1;
        current
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

impl<'a> IntoIterator for &'a Context {
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

#[cfg(test)]
mod test {
    use super::*;

    fn dummy_type() -> Type {
        Type::Unit
    }

    #[test]
    fn test_new_context() {
        let ctx = Context::new();
        assert_eq!(ctx.len(), 0);
    }

    #[test]
    fn test_make_child() {
        let parent = Context::new();
        let child = parent.make_child();
        assert_eq!(child.len(), 0);
    }

    #[test]
    fn test_add_binding_and_len() {
        let ctx = Context::new();
        let binding = Binding("x".to_string(), Bindable::Var(dummy_type()));
        let ctx2 = ctx.add_binding(binding.clone());
        assert_eq!(ctx2.len(), 1);
        match ctx2.get(0) {
            GetResult::Ok(b) => assert_eq!(b, &binding),
            _ => panic!("Binding not found"),
        }
    }

    #[test]
    fn test_add_bindings() {
        let ctx = Context::new();
        let b1 = Binding("a".to_string(), Bindable::Var(dummy_type()));
        let b2 = Binding("b".to_string(), Bindable::Var(dummy_type()));
        let ctx2 = ctx.add_bindings(vec![b1.clone(), b2.clone()]);
        assert_eq!(ctx2.len(), 2);
        assert_eq!(ctx2.get(0), GetResult::Ok(&b2));
        assert_eq!(ctx2.get(1), GetResult::Ok(&b1));
    }

    fn filter_only_x(binding: &Binding) -> bool {
        binding.0 == "x"
    }

    #[test]
    fn test_add_fn_filter() {
        let binding = Context::new();
        let ctx = binding.add_binding(Binding("x".to_string(), Bindable::Var(dummy_type())));
        let filtered_ctx = ctx.add_fn_filter(filter_only_x);
        assert!(matches!(filtered_ctx.get(0), GetResult::Ok(_)));
    }

    #[test]
    fn test_add_closure_filter() {
        let binding = Context::new();
        let ctx = binding.add_binding(Binding("y".to_string(), Bindable::Var(dummy_type())));
        let filter = Rc::new(|b: &Binding| b.0 == "y");
        let filtered_ctx = ctx.add_closure_filter(filter);
        assert!(matches!(filtered_ctx.get(0), GetResult::Ok(_)));
    }

    #[test]
    fn test_iter() {
        let binding = Context::new();
        let ctx = binding.add_bindings(vec![
            Binding("a".to_string(), Bindable::Var(dummy_type())),
            Binding("b".to_string(), Bindable::Var(dummy_type())),
        ]);
        let names: Vec<_> = ctx.iter().map(|b| b.0.clone()).collect();
        assert_eq!(names, vec!["b".to_string(), "a".to_string()]);
    }

    #[test]
    fn test_get_out_of_bounds() {
        let ctx = Context::new();
        assert!(matches!(ctx.get(0), GetResult::OutOfBounds));
    }

    #[test]
    fn test_find_self_by_name() {
        let binding = Context::new();
        let ctx = binding.add_bindings(vec![
            Binding("foo".to_string(), Bindable::Var(dummy_type())),
            Binding("bar".to_string(), Bindable::Var(dummy_type())),
        ]);
        let found = ctx.find_self_by_name("foo");
        assert!(found.is_some());
        assert_eq!(found.unwrap().0, "foo");
        assert!(ctx.find_self_by_name("baz").is_none());
    }

    #[test]
    fn test_bindable_get_type() {
        let ty = dummy_type();
        let b = Bindable::Var(ty.clone());
        assert_eq!(b.get_type(), ty);
        let b = Bindable::Func(ty.clone());
        assert_eq!(b.get_type(), ty);
    }

    // #[test]
    // fn test_next_type_var_and_fresh_var() {
    //     let ctx = Context::new();
    //     assert_eq!(*ctx.next_type_var(), 0);
    //     let v1 = ctx.fresh_var();
    //     assert_eq!(v1, 0);
    //     let v2 = ctx.fresh_var();
    //     assert_eq!(v2, 1);
    //     assert_eq!(*ctx.next_type_var(), 2);
    // }

    #[test]
    fn test_into_iter() {
        let binding = Context::new();
        let ctx = binding.add_binding(Binding("z".to_string(), Bindable::Var(dummy_type())));
        let mut iter = (&ctx).into_iter();
        let b = iter.next().unwrap();
        assert_eq!(b.0, "z");
        assert!(iter.next().is_none());
    }
}
