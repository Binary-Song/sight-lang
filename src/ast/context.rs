// use crate::{
//     ast::typed::{self, Type},
//     parser::{NameStack, PropertyStack},
//     LiteralValue,
// };
// use sight_macros::LiteralValue;
// use std::{cell::RefCell, collections::VecDeque, rc::Rc};

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum ConstraintKind {
//     Let,
//     App,
//     Unify,
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct Constraint {
//     pub lhs: Type,
//     pub rhs: Type,
//     pub kind: ConstraintKind,
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct Binding {
//     pub name: String,
//     pub path: Path,
//     pub ty: Type,
//     pub span: Option<(usize, usize)>,
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct State {
//     pub next_type_var: usize,
//     pub constraints: VecDeque<Constraint>,
//     pub name_stack: NameStack,
//     pub local_var_count: usize,
// }

// #[derive(Clone)]
// pub struct Context {
//     bindings: Vec<Binding>,
//     parent: Option<Rc<Context>>,
// }

// impl std::fmt::Debug for Context {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Context::Basic {
//                 bindings, parent, ..
//             } => {
//                 write!(
//                     f,
//                     "Context::Basic {{ bindings: {:?}, parent: {:?} }}",
//                     bindings, parent
//                 )
//             }
//             Context::FnFilter {
//                 filter: _, parent, ..
//             } => {
//                 write!(f, "Context::FnFilter {{ parent: {:?} }}", parent)
//             }
//             Context::ClosureFilter {
//                 filter: _, parent, ..
//             } => {
//                 write!(f, "Context::ClosureFilter {{ parent: {:?} }}", parent)
//             }
//         }
//     }
// }

// #[derive(Clone)]
// pub struct ContextIter<'a> {
//     ctx: &'a Context,
//     rev_index: usize,
// }

// /// An opaque handle to a Constraint.
// /// Currently only used to remind myself to add a Constraint
// /// when creating certain typed::Exprs, e.g. Let and App.
// ///
// /// Thus, the wrapped usize is intentionally private and unused.
// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct ConstraintHandle(usize);

// #[cfg(test)]
// impl ConstraintHandle {
//     pub fn new(handle: usize) -> Self {
//         ConstraintHandle(handle)
//     }
// }

// impl LiteralValue for ConstraintHandle {
//     fn literal_value(&self) -> String {
//         format!("ConstraintHandle::new({handle})", handle = self.0)
//     }
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum GetResult<T: std::fmt::Debug> {
//     Ok(T),
//     Filtered,
//     OutOfBounds,
// }

// impl Context {
//     pub fn new() -> Rc<Self> {
//         Rc::new(Context::Basic {
//             bindings: vec![],
//             parent: None,
//         })
//     }

//     pub fn make_child(self: Rc<Self>) -> Rc<Self> {
//         self.add_bindings(vec![])
//     }

//     pub fn add_binding(self: Rc<Self>, binding: Binding) -> Rc<Self> {
//         self.add_bindings(vec![binding])
//     }

//     pub fn add_bindings(self: Rc<Self>, bindings: Vec<Binding>) -> Rc<Self> {
//         Rc::new(Context::Basic {
//             bindings,
//             parent: Some(self),
//         })
//     }

//     pub fn add_fn_filter(self: Rc<Self>, filter: fn(&Binding) -> bool) -> Rc<Self> {
//         Rc::new(Context::FnFilter {
//             filter,
//             parent: self,
//         })
//     }

//     pub fn add_closure_filter(self: Rc<Self>, filter: Rc<dyn Fn(&Binding) -> bool>) -> Rc<Self> {
//         Rc::new(Context::ClosureFilter {
//             filter,
//             parent: self,
//         })
//     }

//     /// Create an iterator that iterates over `&Binding` s
//     pub fn iter(&self) -> ContextIter {
//         ContextIter {
//             ctx: self,
//             rev_index: 0,
//         }
//     }

//     pub fn get(&self, rev_index: usize) -> GetResult<&Binding> {
//         match self {
//             Context::Basic {
//                 bindings, parent, ..
//             } => {
//                 if rev_index < bindings.len() {
//                     let item = bindings.get(bindings.len() - 1 - rev_index);
//                     GetResult::Ok(item.unwrap())
//                 } else {
//                     match parent {
//                         Some(p) => {
//                             let index = rev_index - bindings.len();
//                             p.get(index)
//                         }
//                         None => GetResult::OutOfBounds,
//                     }
//                 }
//             }
//             Context::FnFilter { filter, parent, .. } => match parent.get(rev_index) {
//                 GetResult::Ok(binding) => {
//                     if filter(binding) {
//                         GetResult::Ok(binding)
//                     } else {
//                         GetResult::Filtered
//                     }
//                 }
//                 others => others,
//             },
//             Context::ClosureFilter { filter, parent, .. } => match parent.get(rev_index) {
//                 GetResult::Ok(binding) => {
//                     if (*filter)(binding) {
//                         GetResult::Ok(binding)
//                     } else {
//                         GetResult::Filtered
//                     }
//                 }
//                 others => others,
//             },
//         }
//     }

//     pub fn len(&self) -> usize {
//         match self {
//             Context::Basic {
//                 bindings, parent, ..
//             } => bindings.len() + parent.as_ref().map_or(0, |p| p.len()),
//             Context::FnFilter { parent, .. } => parent.len(),
//             Context::ClosureFilter { parent, .. } => parent.len(),
//         }
//     }

//     pub fn lookup(&self, name: &str) -> Option<&Binding> {
//         let mut i = 0;
//         loop {
//             match self.get(i) {
//                 GetResult::Ok(binding) => {
//                     if binding.name == name {
//                         return Some(binding);
//                     }
//                 }
//                 GetResult::Filtered => (),
//                 GetResult::OutOfBounds => return None,
//             }
//             i += 1;
//         }
//     }

//     // pub fn push_function_scope
// }

// impl<'a> Iterator for ContextIter<'a> {
//     type Item = &'a Binding;
//     fn next(&mut self) -> Option<Self::Item> {
//         let binding = self.ctx.get(self.rev_index);
//         self.rev_index += 1;
//         match binding {
//             GetResult::Ok(b) => Some(b),
//             GetResult::Filtered => self.next(),
//             GetResult::OutOfBounds => None,
//         }
//     }
// }

// impl<'a> IntoIterator for &'a Context {
//     type IntoIter = ContextIter<'a>;
//     type Item = &'a Binding;
//     fn into_iter(self) -> Self::IntoIter {
//         self.iter()
//     }
// }
// #[cfg(test)]
// mod tests {
//     use super::*;

//     fn dummy_binding(name: &str) -> Binding {
//         Binding {
//             name: name.to_string(),
//             path: Path::LocalVar { index: 0 },
//             ty: Type::unit(),
//             span: None,
//         }
//     }

//     #[test]
//     fn test_context_add_and_lookup_binding() {
//         let ctx = Context::new();
//         let binding = dummy_binding("x");
//         let ctx = ctx.add_binding(binding.clone());
//         assert_eq!(ctx.lookup("x"), Some(&binding));
//         assert_eq!(ctx.lookup("y"), None);
//     }

//     #[test]
//     fn test_context_len() {
//         let ctx = Context::new();
//         assert_eq!(ctx.len(), 0);
//         let ctx = ctx.add_binding(dummy_binding("a"));
//         assert_eq!(ctx.len(), 1);
//         let ctx = ctx.add_binding(dummy_binding("b"));
//         assert_eq!(ctx.len(), 2);
//     }

//     #[test]
//     fn test_context_iter() {
//         let ctx = Context::new()
//             .add_binding(dummy_binding("a"))
//             .add_binding(dummy_binding("b"));
//         let names: Vec<_> = ctx.iter().map(|b| b.name.clone()).collect();
//         assert_eq!(names, vec!["b".to_string(), "a".to_string()]);
//     }

//     #[test]
//     fn test_context_parent_lookup() {
//         let parent = Context::new().add_binding(dummy_binding("p"));
//         let child = parent.clone().add_binding(dummy_binding("c"));
//         assert_eq!(child.lookup("p").unwrap().name, "p");
//         assert_eq!(child.lookup("c").unwrap().name, "c");
//         assert_eq!(parent.lookup("c"), None);
//     }

//     #[test]
//     fn test_context_fn_filter() {
//         let ctx = Context::new()
//             .add_binding(dummy_binding("a"))
//             .add_binding(dummy_binding("b"));
//         let filter = |b: &Binding| b.name == "b";
//         let filtered_ctx = ctx.add_fn_filter(filter);
//         assert_eq!(filtered_ctx.lookup("b").unwrap().name, "b");
//         assert_eq!(filtered_ctx.lookup("a"), None);
//     }

//     #[test]
//     fn test_context_closure_filter() {
//         let ctx = Context::new()
//             .add_binding(dummy_binding("x"))
//             .add_binding(dummy_binding("y"));
//         let filter = Rc::new(|b: &Binding| b.name == "x");
//         let filtered_ctx = ctx.add_closure_filter(filter);
//         assert_eq!(filtered_ctx.lookup("x").unwrap().name, "x");
//         assert_eq!(filtered_ctx.lookup("y"), None);
//     }
// }
