use crate::{
    ast::{
        id::{Id, IdMap},
        typed::{self as t, Arena, ArenaItem, GetArena, ProjectionExpr},
    },
    context::Context,
    utils::interning::GetInterner,
};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
    I1(bool),
    I32(i32),
}

impl ArenaItem for Instruction {
    fn get_id_map<'a>(context: &'a Arena) -> &'a IdMap<Self> {
        &context.idmap_inst
    }
    fn get_mut_id_map<'a>(context: &'a mut Arena) -> &'a mut IdMap<Self> {
        &mut context.idmap_inst
    }
}

type CodegenResult<T> = Result<T, ()>;

impl Context {
    pub fn codegen_expr(&mut self, lhs: t::ExprId) -> CodegenResult<Id<Instruction>> {
        let lhs = lhs.de(self);
        match lhs {
            t::Expr::Literal(t::LiteralExpr { value, span, ty }) => match value {
                t::Literal::Bool(v) => Ok(vec![Instruction::I1(v)]),
                t::Literal::Int(v) => Ok(vec![Instruction::I32(v)]),
            },
            t::Expr::Variable(t::VariableExpr {
                target,
                name,
                ty,
                span,
            }) => Ok(vec![]),
            t::Expr::Application(t::ApplicationExpr {
                callee,
                arg,
                ty,
                constraint,
                span,
            }) => {
                // Handle function application
                Ok(vec![])
            }
            t::Expr::Block(t::BlockExpr { block }) => {
                // Handle block expression
                Ok(vec![])
            }
            t::Expr::Tuple(tuple_expr) => {
                // Handle tuple expression
                Ok(vec![])
            }
            t::Expr::Projection(proj_expr) => {
                // Handle projection expression
                Ok(vec![])
            }
        }
    }

    pub fn codegen_let(&mut self, lhs: t::PatternId, rhs: t::ExprId) -> CodegenResult<()> {
        let lhs = lhs.de(self);
        match lhs {
            t::Pattern::Variable(t::VariablePattern {
                binding_id,
                ty,
                span,
            }) => {
                let rhs_inst = self.codegen_expr(rhs)?;
                self.binding_targets.insert(binding_id, rhs_inst);
                Ok(())
            }
            t::Pattern::Tuple(t::TuplePattern { elems, span, ty }) => {
                for (idx, elem) in elems.iter().enumerate() {
                    let rhs_ty = match rhs.de(self).ty(self).de(self) {
                        t::Type::Tuple(t::TupleType { elems }) => elems[idx],
                        _ => return Err(()),
                    };
                    let rhs =  (t::ProjectionExpr {
                        target: rhs,
                        index: idx,
                        span: (0, 0),
                        ty: rhs_ty,
                    }).new_id(self);
                    let rhs = t::ExprId::Projection(rhs);
                    self.codegen_let(elem.clone(), rhs);
                }
                Ok(())
            }
        }
    }

    pub fn codegen_stmt(&mut self,
        ast: t::Stmt,
        out: &mut dyn std::io::Write,
    ) {
        match ast {
            t::Stmt::Let(let_stmt) => {
                todo!()
            }
            t::Stmt::Function(fn_stmt) => {
                // Handle function definition
            }
            t::Stmt::Block(block) => {
                // Handle nested blocks
            }
            t::Stmt::Expr(expr_id) => {}
            t::Stmt::Empty(_) => {}
        }
    }
}

// pub fn codegen_expr(
//     ctx: &(impl GetArena + GetInterner<t::Type>),
//     binding_names: &mut HashMap<t::BindingId, String>,
//     ast: t::Expr,
//     out: &mut dyn std::io::Write,
// ) {
//     match ast {
//         t::Expr::Literal(t::LiteralExpr { value, span, ty }) => match value {
//             t::Literal::Bool(v) => {
//                 write!(
//                     out,
//                     "{}",
//                     match v {
//                         true => "True",
//                         false => "False",
//                     }
//                 )imwheel -b "4 5" &
//                 .unwrap();
//             }
//             t::Literal::Int(v) => {
//                 write!(out, "{}", v).unwrap();
//             }
//         },
//         t::Expr::Variable(t::VariableExpr {
//             target,
//             name,
//             ty,
//             span,
//         }) => {
//             // binding_names
//             target.de(ctx)
//         },
//         t::Expr::Application(t::ApplicationExpr {
//             callee, arg, ty, constraint, span
//         }) => {

//         }
//         t::Expr::Block(t::BlockExpr { block }) => {
//             let block = block.de(ctx);
//             for stmt_id in &block.stmts {
//                 let stmt = stmt_id.deref(ctx);
//                 match stmt {
//                     t::Stmt::Let(let_stmt) => {
//                         let lhs = let_stmt.lhs.deref(ctx);
//                         let rhs = let_stmt.rhs.deref(ctx);
//                         write!(out, "{} = ", lhs).unwrap();
//                         translate_to_python(ctx, binding_names, rhs, out);
//                         write!(out, "\n").unwrap();
//                     }
//                     t::Stmt::Function(fn_stmt) => {
//                         // Handle function definition
//                     }
//                     t::Stmt::Block(block) => {
//                         // Handle nested blocks
//                     }
//                     t::Stmt::Expr(expr_id) => {
//                         translate_to_python(ctx, binding_names, expr_id.de(ctx), out);
//                         write!(out, "\n").unwrap();
//                     }
//                     t::Stmt::Empty(_) => {}
//                 }
//             }
//             write!(out, ")\n").unwrap();
//         }
//         t::Expr::Tuple(tuple_expr) => {}
//     }
// }
