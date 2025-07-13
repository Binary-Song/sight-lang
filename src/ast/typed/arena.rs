use crate::ast::{id::*, typed::*};
use sight_macros::{Internable, LiteralValue, StaticInternable};
use std::{collections::HashMap, hash::Hash};

/// Where id maps are stored. With Arena, you can get the value pointed to by an [Id].
#[derive(Default)]
pub struct Arena {
    // EXPR
    pub idmap_literal_expr: IdMap<LiteralExpr>,
    pub idmap_variable_expr: IdMap<VariableExpr>,
    pub idmap_application_expr: IdMap<ApplicationExpr>,
    pub idmap_block_expr: IdMap<BlockExpr>,
    pub idmap_tuple_expr: IdMap<TupleExpr>,

    // PATTERN
    pub idmap_variable_pattern: IdMap<VariablePattern>,
    pub idmap_tuple_pattern: IdMap<TuplePattern>,

    // STMT
    pub idmap_let_stmt: IdMap<LetStmt>,
    pub idmap_function_stmt: IdMap<FunctionStmt>,
    pub idmap_empty_stmt: IdMap<EmptyStmt>,

    // defs
    pub idmap_def: IdMap<Binding>,

    // OTHERS
    pub idmap_block: IdMap<Block>,
    pub idmap_constraint: IdMap<Constraint>,
}

pub trait HasIdMapInArena: Sized {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self>;
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self>;
}

impl HasIdMapInArena for Constraint {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_constraint
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_constraint
    }
}

impl HasIdMapInArena for LiteralExpr {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_literal_expr
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_literal_expr
    }
}

impl HasIdMapInArena for VariableExpr {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_variable_expr
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_variable_expr
    }
}

impl HasIdMapInArena for ApplicationExpr {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_application_expr
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_application_expr
    }
}

impl HasIdMapInArena for Block {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_block
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_block
    }
}

impl HasIdMapInArena for BlockExpr {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_block_expr
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_block_expr
    }
}

impl HasIdMapInArena for TupleExpr {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_tuple_expr
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_tuple_expr
    }
}

impl HasIdMapInArena for VariablePattern {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_variable_pattern
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_variable_pattern
    }
}

impl HasIdMapInArena for TuplePattern {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_tuple_pattern
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_tuple_pattern
    }
}

impl HasIdMapInArena for LetStmt {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_let_stmt
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_let_stmt
    }
}

impl HasIdMapInArena for FunctionStmt {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_function_stmt
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_function_stmt
    }
}

impl HasIdMapInArena for EmptyStmt {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_empty_stmt
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_empty_stmt
    }
}

impl HasIdMapInArena for Binding {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Binding> {
        &context.idmap_def
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Binding> {
        &mut context.idmap_def
    }
}

impl Arena {
    pub fn new() -> Self {
        Arena::default()
    }

    pub fn bind_new_id_to<T: HasIdMapInArena>(&mut self, node: T) -> Id<T> {
        T::get_mut_id_map(self).push(node)
    }

    pub fn bind_id_to<T: HasIdMapInArena>(&mut self, id: Id<T>, value: T) {
        T::get_mut_id_map(self).set(id, value);
    }

    pub fn unbound_id<T: HasIdMapInArena>(&mut self) -> Id<T> {
        T::get_mut_id_map(self).push_none()
    }

    pub fn unbound_body_id(&mut self) -> BodyId {
        Body::get_mut_id_map(self).push_none()
    }

    pub fn deref<T: HasIdMapInArena>(&self, id: Id<T>) -> &T {
        T::get_id_map(self).get(id).unwrap()
    }

    pub fn deref_mut<T: HasIdMapInArena>(&mut self, id: Id<T>) -> &mut T {
        T::get_mut_id_map(self).get_mut(id).unwrap()
    }

    pub fn lookup_name(&self, name: InternString, search_start: BindingId) -> Option<BindingId> {
        let mut def_id = search_start;
        loop {
            if let Some(def_name) = self.deref(def_id).name() {
                if name == def_name {
                    return Some(def_id);
                }
            }
            def_id = self.deref(def_id).parent?;
        }
    }
}
