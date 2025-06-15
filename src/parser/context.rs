use crate::{
    ast::typed::{ScopeName, Type},
    parser::{NameStack, PropertyStack},
    LiteralValue,
};
use std::{cell::RefCell, collections::VecDeque, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bindable {
    Var { ty: Type, local_var_index: usize },
    Func { ty: Type, global_name: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintKind {
    Let,
    App,
    Unify,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constraint {
    pub lhs: Type,
    pub rhs: Type,
    pub kind: ConstraintKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
    pub name: String,
    pub bindable: Bindable,
    pub span: Option<(usize, usize)>,
}

#[derive(Default)]
pub struct ContextSharedState {
    pub next_type_var: u32,
    pub constraints: VecDeque<Constraint>,
    pub local_var_count: PropertyStack<usize>,
    pub name_stack: NameStack,
}

#[derive(Clone)]
pub enum Context {
    Basic {
        bindings: Vec<Binding>,
        parent: Option<Rc<Context>>,
        state: Rc<RefCell<ContextSharedState>>,
    },
    FnFilter {
        filter: fn(&Binding) -> bool,
        parent: Rc<Context>,
        state: Rc<RefCell<ContextSharedState>>,
    },
    ClosureFilter {
        filter: Rc<dyn Fn(&Binding) -> bool>,
        parent: Rc<Context>,
        state: Rc<RefCell<ContextSharedState>>,
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

/// An opaque handle to a Constraint.
/// Currently only used to remind myself to add a Constraint
/// when creating certain typed::Exprs, e.g. Let and App.
///
/// Thus, the wrapped usize is intentionally private and unused.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstraintHandle(usize);

#[cfg(test)]
impl ConstraintHandle {
    pub fn new(handle: usize) -> Self {
        ConstraintHandle(handle)
    }
}

impl LiteralValue for ConstraintHandle {
    fn literal_value(&self) -> String {
        format!("ConstraintHandle::new({handle})", handle = self.0)
    }
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
            state: Rc::new(RefCell::new(ContextSharedState::default())),
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
            state: self.state().clone(),
            parent: Some(self),
        })
    }

    pub fn add_fn_filter(self: Rc<Self>, filter: fn(&Binding) -> bool) -> Rc<Self> {
        Rc::new(Context::FnFilter {
            filter,
            state: self.state(),
            parent: self,
        })
    }

    pub fn add_closure_filter(self: Rc<Self>, filter: Rc<dyn Fn(&Binding) -> bool>) -> Rc<Self> {
        Rc::new(Context::ClosureFilter {
            filter,
            state: self.state(),
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
                    if binding.name == name {
                        return Some(binding);
                    }
                }
                GetResult::Filtered => (),
                GetResult::OutOfBounds => return None,
            }
            i += 1;
        }
    }

    pub fn add_constraint(&self, constraint: Constraint) -> ConstraintHandle {
        self.state()
            .borrow_mut()
            .constraints
            .push_back(constraint.clone());
        let cons_len = self.state().borrow().constraints.len();
        ConstraintHandle(cons_len - 1)
    }

    pub fn state(&self) -> Rc<RefCell<ContextSharedState>> {
        match self {
            Context::Basic { state, .. }
            | Context::FnFilter { state, .. }
            | Context::ClosureFilter { state, .. } => state.clone(),
        }
    }

    pub fn state_ref(&self) -> std::cell::Ref<ContextSharedState> {
        match self {
            Context::Basic { state, .. }
            | Context::FnFilter { state, .. }
            | Context::ClosureFilter { state, .. } => state.borrow(),
        }
    }

    pub fn state_refmut(&self) -> std::cell::RefMut<ContextSharedState> {
        match self {
            Context::Basic { state, .. }
            | Context::FnFilter { state, .. }
            | Context::ClosureFilter { state, .. } => state.borrow_mut(),
        }
    }

    pub fn alloc_type_var(&self) -> u32 {
        self.state().borrow_mut().next_type_var += 1;
        self.state().borrow().next_type_var - 1
    }

    pub fn alloc_local_var(&self) -> usize {
        let mut state = self.state_refmut();
        let index = state.local_var_count.value();
        state.local_var_count.set(index + 1);
        index
    }

    pub fn qualify_name(&self, name: &str) -> String {
        self.state_ref().name_stack.qualify_name(name)
    }

    // pub fn push_function_scope
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
            Bindable::Var { ty, .. } => ty.clone(),
            Bindable::Func { ty, .. } => ty.clone(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn dummy_type() -> Type {
        Type::unit()
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
        let binding = Binding {
            name: "x".to_string(),
            bindable: Bindable::Var {
                ty: (dummy_type()),
                local_var_index: 0,
            },
            span: None,
        };
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
        let b1 = Binding {
            name: "a".to_string(),
            bindable: Bindable::Var {
                ty: (dummy_type()),
                local_var_index: 0,
            },
            span: None,
        };
        let b2 = Binding {
            name: "b".to_string(),
            bindable: Bindable::Var {
                ty: (dummy_type()),
                local_var_index: 0,
            },
            span: None,
        };
        let ctx2 = ctx.add_bindings(vec![b1.clone(), b2.clone()]);
        assert_eq!(ctx2.len(), 2);
        assert_eq!(ctx2.get(0), GetResult::Ok(&b2));
        assert_eq!(ctx2.get(1), GetResult::Ok(&b1));
    }

    fn filter_only_x(binding: &Binding) -> bool {
        binding.name == "x"
    }

    #[test]
    fn test_add_fn_filter() {
        let binding = Context::new();
        let ctx = binding.add_binding(Binding {
            name: "x".to_string(),
            bindable: Bindable::Var {
                ty: (dummy_type()),
                local_var_index: 0,
            },
            span: None,
        });
        let filtered_ctx = ctx.add_fn_filter(filter_only_x);
        assert!(matches!(filtered_ctx.get(0), GetResult::Ok(_)));
    }

    #[test]
    fn test_add_closure_filter() {
        let binding = Context::new();
        let ctx = binding.add_binding(Binding {
            name: "y".to_string(),
            bindable: Bindable::Var {
                ty: (dummy_type()),
                local_var_index: 0,
            },
            span: None,
        });
        let filter = Rc::new(|b: &Binding| b.name == "y");
        let filtered_ctx = ctx.add_closure_filter(filter);
        assert!(matches!(filtered_ctx.get(0), GetResult::Ok(_)));
    }

    #[test]
    fn test_iter() {
        let binding = Context::new();
        let ctx = binding.add_bindings(vec![
            Binding {
                name: "a".to_string(),
                bindable: Bindable::Var {
                    ty: (dummy_type()),
                    local_var_index: 0,
                },
                span: None,
            },
            Binding {
                name: "b".to_string(),
                bindable: Bindable::Var {
                    ty: (dummy_type()),
                    local_var_index: 0,
                },
                span: None,
            },
        ]);
        let names: Vec<_> = ctx.iter().map(|b| b.name.clone()).collect();
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
            Binding {
                name: "foo".to_string(),
                bindable: Bindable::Var {
                    ty: (dummy_type()),
                    local_var_index: 0,
                },
                span: None,
            },
            Binding {
                name: "bar".to_string(),
                bindable: Bindable::Var {
                    ty: dummy_type(),
                    local_var_index: 0,
                },
                span: None,
            },
        ]);
        let found = ctx.find_self_by_name("foo");
        assert!(found.is_some());
        assert_eq!(found.unwrap().name, "foo");
        assert!(ctx.find_self_by_name("baz").is_none());
    }

    #[test]
    fn test_bindable_get_type() {
        let ty = dummy_type();
        let b = Bindable::Var {
            ty: (ty.clone()),
            local_var_index: 0,
        };
        assert_eq!(b.get_type(), ty);
        let b = Bindable::Func {
            ty: ty.clone(),
            global_name: "".to_string(),
        };
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
        let ctx = binding.add_binding(Binding {
            name: "z".to_string(),
            bindable: Bindable::Var {
                ty: (dummy_type()),
                local_var_index: 0,
            },
            span: None,
        });
        let mut iter = (&ctx).into_iter();
        let b = iter.next().unwrap();
        assert_eq!(b.name, "z");
        assert!(iter.next().is_none());
    }
}
