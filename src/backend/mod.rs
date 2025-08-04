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

fn ty_id_to_ir(ty: Id<Type>, c: &mut impl Container) -> ir::IdTy {
    ty_to_ir(ty.decode_f(c), c)
}

fn ty_to_ir(ty: Type, c: &mut impl Container) -> ir::IdTy {
    match ty {
        Type::Primitive(primitive_type) => match primitive_type {
            crate::ast::typed::PrimitiveType::Bool => ir::Ty::Int(ir::IntTy::Int1).encode_f(c),
            crate::ast::typed::PrimitiveType::Int => ir::Ty::Int(ir::IntTy::Int32).encode_f(c),
        },
        Type::Function(function_type) => {
            let mut param_tys = vec![];
            for ty in function_type.lhs {
                let ty = ty.decode_f(c);
                let ty = ty_to_ir(ty, c);
                param_tys.push(ty);
            }
            let ret_ty = function_type.rhs.decode_f(c);
            let ret_ty = ty_to_ir(ret_ty, c);
            let t = ir::FuncTy { param_tys, ret_ty }.upcast().encode_f(c);
            t
        }
        Type::Tuple(tuple_type) => {
            let mut tys = vec![];
            for (idx, ty) in tuple_type.elems.iter().enumerate() {
                let ty = ty.decode_f(c);
                let ty = ty_to_ir(ty, c);
                tys.push((format!("_{idx}").encode_f(c), ty));
            }
            let t = ir::StructTy { elems: tys }.upcast().encode_f(c);
            t
        }
        Type::Unknown(unknown_type) => {
            panic!("Unknown type should be solved at this point")
        }
    }
}

fn function_to_ir(func: FunctionStmt, c: &mut impl Container) -> Id<ir::Function> {
    let mut ir_params = vec![];
    let func = func.binding.decode_f(c);
    let mut ir_func = ir::Function {
        params: {
            let mut params = vec![];
            for (ty, name) in func.param_tys.iter().zip(func.param_names) {
                let ty = ty_to_ir(ty, c);
                let p = ParamValue {
                    ty,
                    name_hint: name,
                };
                let p = p.encode_f(c);
                params.push(p);
            }
            params
        },
        bbs: vec![],
        entry: None,
        ret_ty: func_ir_ty.ret_ty,
    }
    .encode_f(c);
}

// fn gen_expr(expr: Expr, bb: Id<BasicBlock>, container: &mut impl Container) -> Id<BasicBlock> {
//     match expr {
//         Expr::Lit(LitExpr { value, span }) => {
//             match value {
//                 Literal::Int(val) => container
//                     .decode_f(bb)
//                     .push_inst(IntInst(val).upcast(), container),
//                 Literal::Bool(val) => container
//                     .decode_f(bb)
//                     .push_inst(BoolInst(val).upcast(), container),
//             };
//             bb
//         }
//         _ => todo!(),
//     }
// }
