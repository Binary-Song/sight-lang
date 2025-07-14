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
    Nop(),
    I1(bool),
    I32(i32),
    Call(Vec<Id<Instruction>>),
    Function(Id<Instruction>),
    Param(usize),
    Tuple(Vec<Id<Instruction>>),
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
    pub fn get_nop(&mut self) -> Id<Instruction> {
        Instruction::Nop().new_id(self)
    }

    pub fn codegen_block(&mut self, ast: t::Block) -> CodegenResult<Id<Instruction>> {
        let (last, stmts) = match ast.stmts.split_last() {
            Some(x) => x,
            None => return Ok(self.get_nop()),
        };

        for stmt_id in stmts {
            let stmt = stmt_id.de(self);
            self.codegen_stmt(stmt)?;
        }

        let last = last.de(self);
        match last {
            t::Stmt::Expr(ex) => {
                let inst = self.codegen_expr(ex)?;
                Ok(inst)
            }
            _ => Ok(self.get_nop()),
        }
    }

    pub fn codegen_stmt(&mut self, ast: t::Stmt) -> CodegenResult<Id<Instruction>> {
        match ast {
            t::Stmt::Let(let_stmt) => {
                self.codegen_let(let_stmt.lhs, let_stmt.rhs)?;
                Ok(self.get_nop())
            }
            t::Stmt::Function(fn_stmt) => {
                self.codegen_param(fn_stmt.param, &mut 0)?;
                let body_inst = self.codegen_block(fn_stmt.body.de(self))?;
                let fn_inst = Instruction::Function(body_inst).new_id(self);
                self.binding_targets.insert(fn_stmt.new_fn_id, fn_inst);
                Ok(fn_inst)
            }
            t::Stmt::Block(block) => Ok(self.codegen_block(block)?),
            t::Stmt::Expr(expr_id) => self.codegen_expr(expr_id),
            t::Stmt::Empty(_) => Ok(self.get_nop()),
        }
    }

    pub fn codegen_expr(&mut self, expr: t::ExprId) -> CodegenResult<Id<Instruction>> {
        let lhs = expr.de(self);
        match lhs {
            t::Expr::Literal(t::LiteralExpr { value, span, ty }) => match value {
                t::Literal::Bool(v) => Ok(Instruction::I1(v).new_id(self)),
                t::Literal::Int(v) => Ok(Instruction::I32(v).new_id(self)),
            },
            t::Expr::Variable(t::VariableExpr {
                target,
                name,
                ty,
                span,
            }) => match self.binding_targets.get(&target) {
                Some(inst) => Ok(inst.clone()),
                None => Err(()),
            },
            t::Expr::Application(t::ApplicationExpr {
                callee,
                arg,
                ty,
                constraint,
                span,
            }) => {
                let callee_inst = self.codegen_expr(callee)?;
                let arg_inst = self.codegen_expr(arg)?;
                let call_inst = Instruction::Call(vec![callee_inst, arg_inst]).new_id(self);
                Ok(call_inst)
            }
            t::Expr::Block(t::BlockExpr { block }) => Ok(self.codegen_block(block.de( self))?),
            t::Expr::Tuple(tuple_expr) => {
                
                Ok(vec![])
            }
            t::Expr::Projection(proj_expr) => {
                // Handle projection expression
                Ok(vec![])
            }
        }
    }

    pub fn codegen_param(
        &mut self,
        param: t::PatternId,
        param_index: &mut usize,
    ) -> CodegenResult<()> {
        let param = param.de(self);
        match param {
            t::Pattern::Variable(t::VariablePattern {
                binding_id,
                ty,
                span,
            }) => {
                let param_inst = Instruction::Param(*param_index).new_id(self);
                *param_index += 1;
                self.binding_targets.insert(binding_id, param_inst);
                Ok(())
            }
            t::Pattern::Tuple(t::TuplePattern { elems, span, ty }) => {
                for elem in elems {
                    self.codegen_param(elem, param_index);
                }
                Ok(())
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
                    let rhs = (t::ProjectionExpr {
                        target: rhs,
                        index: idx,
                        span: (0, 0),
                        ty: rhs_ty,
                    })
                    .new_id(self);
                    let rhs = t::ExprId::Projection(rhs);
                    self.codegen_let(elem.clone(), rhs);
                }
                Ok(())
            }
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
