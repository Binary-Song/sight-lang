use crate::{ast::{id::*, typed::*}, sema::inference::Constraint, utils::interning::InternString};

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

pub trait GetArena {
    fn get_arena(&self) -> &Arena;
    fn get_arena_mut(&mut self) -> &mut Arena;
}

impl GetArena for Arena {
    fn get_arena(&self) -> &Arena {
        self
    }

    fn get_arena_mut(&mut self) -> &mut Arena {
        self
    }
}

pub trait ArenaItem: Clone {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self>;
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self>;
    fn new_id(self, arena: &mut impl GetArena) -> Id<Self> {
        arena.get_arena_mut().bind_new_id_to(self)
    }
}

impl ArenaItem for Constraint {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_constraint
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_constraint
    }
}

impl ArenaItem for LiteralExpr {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_literal_expr
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_literal_expr
    }
}

impl ArenaItem for VariableExpr {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_variable_expr
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_variable_expr
    }
}

impl ArenaItem for ApplicationExpr {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_application_expr
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_application_expr
    }
}

impl ArenaItem for Block {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_block
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_block
    }
}

impl ArenaItem for BlockExpr {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_block_expr
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_block_expr
    }
}

impl ArenaItem for TupleExpr {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_tuple_expr
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_tuple_expr
    }
}

impl ArenaItem for VariablePattern {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_variable_pattern
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_variable_pattern
    }
}

impl ArenaItem for TuplePattern {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_tuple_pattern
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_tuple_pattern
    }
}

impl ArenaItem for LetStmt {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_let_stmt
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_let_stmt
    }
}

impl ArenaItem for FunctionStmt {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_function_stmt
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_function_stmt
    }
}

impl ArenaItem for EmptyStmt {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_empty_stmt
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_empty_stmt
    }
}

impl ArenaItem for Binding {
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

    pub fn bind_new_id_to<T: ArenaItem>(&mut self, node: T) -> Id<T> {
        T::get_mut_id_map(self).push(node)
    }

    pub fn bind_id_to<T: ArenaItem>(&mut self, id: Id<T>, value: T) {
        T::get_mut_id_map(self).set(id, value);
    }

    pub fn unbound_id<T: ArenaItem>(&mut self) -> Id<T> {
        T::get_mut_id_map(self).push_none()
    }

    pub fn unbound_body_id(&mut self) -> BodyId {
        Body::get_mut_id_map(self).push_none()
    }

    pub fn deref_mut<T: ArenaItem>(&mut self, id: Id<T>) -> &mut T {
        T::get_mut_id_map(self).get_mut(id).unwrap()
    }

    pub fn lookup_name(&self, name: InternString, search_start: BindingId) -> Option<BindingId> {
        let mut def_id = search_start;
        loop {
            if let Some(def_name) = def_id.de(self).name() {
                if name == def_name {
                    return Some(def_id);
                }
            }
            def_id = def_id.de(self).parent?;
        }
    }
}
