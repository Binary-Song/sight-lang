use crate::container::DecodeError;
use crate::container::EncodeError;
use crate::container::Id;
use crate::{
    ast::typed::{Expr, LitExpr, Literal, VarExpr},
    container::{Container, Item},
    LiteralValue,
};
use sight_macros::make_sum_id;
use sight_macros::Item;
use sight_macros::LiteralValue;

make_sum_id!(
    target_type: Inst,
    id_type: IdInst,
    Int: IntInst,
    Bool: BoolInst,
    Add: AddInst,
    Param: ParamInst,
    Ret: RetInst,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
struct IntInst(i32);

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
struct BoolInst(bool);

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
struct AddInst(IdInst, IdInst);

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
struct ParamInst(usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
struct RetInst(IdInst);

#[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue, Item)]
struct BasicBlock {
    insts: Vec<IdInst>,
}

impl BasicBlock {
    fn push_inst(&mut self, inst: Inst, container: &mut impl Container) -> IdInst {
        let inst_id = inst.encode_f(container);
        self.insts.push(inst_id);
        inst_id
    }
}

fn gen_expr(expr: Expr, bb: Id<BasicBlock>, container: &mut impl Container) -> Id<BasicBlock> {
    match expr {
        Expr::Lit(LitExpr { value, span }) => {
            match value {
                Literal::Int(val) => container
                    .dec(bb)
                    .push_inst(IntInst(val).upcast(), container),
                Literal::Bool(val) => container
                    .decode_mut_f(bb)
                    .push_inst(BoolInst(val).upcast(), container),
            };
            todo!()
        }
        _ => todo!(),
    }
}
