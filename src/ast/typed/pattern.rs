use crate::container::*;
use sight_macros::LiteralValue;

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct VariablePattern<'c, A: Arena<Self>> {
    pub binding_id: BindingId,
    pub ty: TypeId,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct TuplePattern<'c, A: Arena<Self>> {
    pub elems: Vec<PatternId>,
    pub ty: TypeId,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, LiteralValue)]
pub enum PatternId<'c, A: Arena<Self>> {
    Variable(Id<VariablePattern>),
    Tuple(Id<TuplePattern>),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Pattern<'c, A: Arena<Self>> {
    Variable(VariablePattern),
    Tuple(TuplePattern),
}

impl<'c, A: Arena<Self>> PatternId<'c, A> {
    pub fn de(self, arena: &impl GetArena) -> Pattern<'c, A> {
        match self {
            PatternId::Variable(id) => Pattern::Variable(id.de(arena)),
            PatternId::Tuple(id) => Pattern::Tuple(id.de(arena)),
        }
    }
}