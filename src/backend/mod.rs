use crate::ast::typed::FunctionStmt;
use crate::ast::typed::FunctionType;
use crate::ast::typed::Type;
use crate::backend::ir::ParamValue;
use crate::container::DecodeError;
use crate::container::EncodeError;
use crate::container::Id;
use crate::{
    ast::typed::{Expr, LitExpr, Literal, VarExpr},
    container::{Container, Item},
    LiteralValue,
};
use core::panic;
use sight_macros::make_sum_id;
use sight_macros::Item;
use sight_macros::LiteralValue;
use std::cell::RefCell;
use std::hash::Hash;
use std::rc::Rc;
pub mod ir;

impl Id<Type> {

}

fn ty_id_to_ir(ty: Id<Type>, c: &mut Container) -> ir::IdTy {
    ty_to_ir(ty.dec(c), c)
}

fn ty_to_ir(ty: Type, c: &mut Container) -> ir::IdTy {
    match ty {
        Type::Primitive(primitive_type) => match primitive_type {
            crate::ast::typed::PrimitiveType::Bool => ir::Ty::Int(ir::IntTy::Int1).enc(c),
            crate::ast::typed::PrimitiveType::Int => ir::Ty::Int(ir::IntTy::Int32).enc(c),
        },
        Type::Function(function_type) => {
            let mut param_tys = vec![];
            for ty in function_type.lhs {
                let ty = ty.dec(c);
                let ty = ty_to_ir(ty, c);
                param_tys.push(ty);
            }
            let ret_ty = function_type.rhs.dec(c);
            let ret_ty = ty_to_ir(ret_ty, c);
            let t = ir::FuncTy { param_tys, ret_ty }.upcast().enc(c);
            t
        }
        Type::Tuple(tuple_type) => {
            let mut tys = vec![];
            for (idx, ty) in tuple_type.elems.iter().enumerate() {
                let ty = ty.dec(c);
                let ty = ty_to_ir(ty, c);
                tys.push((format!("_{idx}").enc(c), ty));
            }
            let t = ir::StructTy { elems: tys }.upcast().enc(c);
            t
        }
        Type::Unknown(unknown_type) => {
            panic!("Unknown type should be solved at this point")
        }
    }
}

fn function_to_ir(func: FunctionStmt, c: &mut Container) -> Id<ir::Function> {
    let mut ir_params = vec![];
    let func = func.binding.dec(c);
    let mut ir_func = ir::Function {
        params: {
            let mut params = vec![];
            for (ty, name) in func.param_tys.iter().zip(func.param_names) {
                let ty = ty_to_ir(ty, c);
                let p = ParamValue {
                    ty,
                    name_hint: name,
                };
                let p = p.enc(c);
                params.push(p);
            }
            params
        },
        bbs: vec![],
        entry: None,
        ret_ty: func_ir_ty.ret_ty,
    }
    .enc(c);
}

// fn gen_expr(expr: Expr, bb: Id<BasicBlock>, container: &mut Container) -> Id<BasicBlock> {
//     match expr {
//         Expr::Lit(LitExpr { value, span }) => {
//             match value {
//                 Literal::Int(val) => container
//                     .dec(bb)
//                     .push_inst(IntInst(val).upcast(), container),
//                 Literal::Bool(val) => container
//                     .dec(bb)
//                     .push_inst(BoolInst(val).upcast(), container),
//             };
//             bb
//         }
//         _ => todo!(),
//     }
// }
