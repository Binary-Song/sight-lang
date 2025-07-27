use crate::{
    ast::{
        span::Span,
        typed::{binding::Binding, ty::Type},
    },
    container::*,
};
use sight_macros::{make_sum_id, LiteralValue};

make_sum_id!(
    target_type: Pattern,
    id_type: PatternSumId,
    VariablePattern: VariablePattern,
    TuplePattern: TuplePattern,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct VariablePattern {
    pub binding_id: IdBinding,
    pub ty: Id<Type>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct TuplePattern {
    pub elems: Vec<PatternSumId>,
    pub ty: Id<Type>,
    pub span: Option<Span>,
}
