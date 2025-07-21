use crate::{
    ast::{
        span::Span,
        typed::{binding::Binding, r#type::Type},
    },
    container::*,
    sum_id,
};
use sight_macros::LiteralValue;

pub type PatternSumId = sum_id!(VariablePattern, TuplePattern);

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct VariablePattern {
    pub binding_id: Id<Binding>,
    pub ty: Id<Type>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct TuplePattern {
    pub elems: Vec<PatternSumId>,
    pub ty: Id<Type>,
    pub span: Option<Span>,
}
