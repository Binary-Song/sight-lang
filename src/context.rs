use crate::{
    ast::{
        id::Id,
        typed::{Arena, BindingId, GetArena, Type},
    },
    backend::Instruction,
    utils::interning::{GetInterner, Interner},
};

pub struct Context {
    pub next_type_var: usize,
    pub arena: Arena,
    pub type_interner: Interner<Type>,
    pub binding_targets: std::collections::HashMap<BindingId, Id<Instruction>>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            next_type_var: 0,
            arena: Arena::new(),
            type_interner: Interner::new(),
            binding_targets: std::collections::HashMap::new(),
        }
    }
}

impl GetArena for Context {
    fn get_arena(&self) -> &Arena {
        &self.arena
    }

    fn get_arena_mut(&mut self) -> &mut Arena {
        &mut self.arena
    }
}

impl GetInterner<Type> for Context {
    fn get_interner(&self) -> &Interner<Type> {
        &self.type_interner
    }

    fn get_interner_mut(&mut self) -> &mut Interner<Type> {
        &mut self.type_interner
    }
}
