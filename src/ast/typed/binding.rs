use crate::{
    ast::typed::{expr::Block, ty::Type, FunctionType},
    container::*,
};
use sight_macros::{IdEnum, Item, LiteralValue};

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, IdEnum)]
pub enum Binding {
    Empty(EmptyBinding),
    Func(FuncBinding),
    Var(VarBinding),
    Param(ParamBinding),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue,  )]
pub struct EmptyBinding {
    pub parent: Option<IdBinding>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue,  )]
pub struct FuncBinding {
    pub parent: Option<IdBinding>,
    pub name: Uid<String>,
    pub param_tys: Vec<Uid<Type>>,
    pub param_names: Vec<Uid<String>>,
    pub ret_ty: Uid<Type>,
    /// This data is only available after processing
    /// the body (the second pass).
    /// The rest is available after processing the
    /// prototype (the first pass).
    pub full_data: Option<FuncFullData>,
}

impl FuncBinding {
    pub fn ty(&self, c: &mut Container) -> Uid<Type> {
        FunctionType {
            lhs: self.param_tys.clone(),
            rhs: self.ret_ty.clone(),
        }
        .to_type()
        .int(c)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
pub struct FuncFullData {
    pub local_vars: Vec<Id<VarBinding>>,
    pub params: Vec<Id<ParamBinding>>,
    pub body_id: Id<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue,  )]
pub struct VarBinding {
    pub parent: Option<IdBinding>,
    pub name: Uid<String>,
    pub ty: Uid<Type>,
    pub index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue,  )]
pub struct ParamBinding {
    pub parent: Option<IdBinding>,
    pub name: Uid<String>,
    pub ty: Uid<Type>,
    pub index: usize,
}

impl Binding {
    pub fn name(&self) -> Option<Uid<String>> {
        match self {
            Binding::Func(FuncBinding { name, .. })
            | Binding::Var(VarBinding { name, .. })
            | Binding::Param(ParamBinding { name, .. }) => Some(name.clone()),
            Binding::Empty(_) => None,
        }
    }
    pub fn parent(&self) -> Option<IdBinding> {
        match self {
            Binding::Func(FuncBinding { parent, .. })
            | Binding::Var(VarBinding { parent, .. })
            | Binding::Param(ParamBinding { parent, .. }) => parent.clone(),
            Binding::Empty(_) => None,
        }
    }
}

impl IdBinding {
    /// Creates and returns a new binding whose parent is this binding id.
    pub fn derive(self, from: Binding) -> Binding {
        match from {
            Binding::Empty(b) => Binding::Empty(EmptyBinding {
                parent: Some(self),
                ..b
            }),
            Binding::Func(b) => Binding::Func(FuncBinding {
                parent: Some(self),
                ..b
            }),
            Binding::Var(b) => Binding::Var(VarBinding {
                parent: Some(self),
                ..b
            }),
            Binding::Param(b) => Binding::Param(ParamBinding {
                parent: Some(self),
                ..b
            }),
        }
    }
}
