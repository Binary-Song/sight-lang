
/// use std::{collections::HashMap, fmt::format};
use crate::ast::*;
use crate::testing::into_tree_with_context::TreeContext;
use crate::{span::Span, typing::*};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tree {
    None,
    Bool(bool),
    Int(i32),
    String(String),
    Object(TreeObject),
    Array(TreeArray),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TreeObject {
    fields: Vec<(String, Tree)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TreeArray {
    array: Vec<Tree>,
}

impl Into<Tree> for i32 {
    fn into(self) -> Tree {
        Tree::Int(self)
    }
}

impl Into<Tree> for bool {
    fn into(self) -> Tree {
        Tree::Bool(self)
    }
}

impl Into<Tree> for String {
    fn into(self) -> Tree {
        Tree::String(self)
    }
}
impl Into<Tree> for &str {
    fn into(self) -> Tree {
        Tree::String(self.to_string())
    }
}
impl Into<Tree> for TreeObject {
    fn into(self) -> Tree {
        Tree::Object(self)
    }
}

impl Into<Tree> for TreeArray {
    fn into(self) -> Tree {
        Tree::Array(self)
    }
}

impl TreeObject {
    pub fn new() -> TreeObject {
        TreeObject { fields: Vec::new() }
    }
    pub fn new_basic(type_name: &str, ctx: &TreeContext) -> TreeObject {
        TreeObject::new().add_field("type_name", type_name.to_string())
    }
    pub fn add_field<T: Into<Tree>>(mut self, key: &str, value: T) -> TreeObject {
        self.fields.push((key.to_string(), value.into()));
        self
    }
    pub fn get(&self, key: &str) -> Option<&Tree> {
        self.fields.iter().find(|(k, _)| k == key).map(|(_, v)| v)
    }
    pub fn iter(&self) -> impl Iterator<Item = (&String, &Tree)> {
        self.fields.iter().map(|(k, v)| (k, v))
    }
}

impl TreeArray {
    pub fn new() -> TreeArray {
        TreeArray { array: Vec::new() }
    }
    pub fn add_item<T: Into<Tree>>(mut self, item: T) -> TreeArray {
        self.array.push(item.into());
        self
    }
    pub fn get(&self, index: usize) -> Option<&Tree> {
        self.array.get(index)
    }
    pub fn iter(&self) -> impl Iterator<Item = &Tree> {
        self.array.iter()
    }
}
