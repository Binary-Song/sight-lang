
use crate::ast::AST;

pub trait Visitor<E> {
    fn visit_expr(&self, _: &mut crate::ast::Expr) -> Result<(), E> {
        Ok(())
    }

    fn visit_stmt(&self, _: &mut crate::ast::Stmt) -> Result<(), E> {
        Ok(())
    }

    fn visit_pat(&self, _: &mut crate::ast::Pattern) -> Result<(), E> {
        Ok(())
    }

    fn visit_type_expr(&self, _: &mut crate::ast::TypeExpr) -> Result<(), E> {
        Ok(())
    }

    // typed AST
    fn visit_texpr(&self, _: &mut crate::ast::typed::Expr) -> Result<(), E> {
        Ok(())
    }

    fn visit_tpat(&self, _: &mut crate::ast::typed::Pattern) -> Result<(), E> {
        Ok(())
    }

    fn visit_ttype(&self, _: &mut crate::ast::typed::Type) -> Result<(), E> {
        Ok(())
    }
}

impl AST for crate::ast::TypeExpr {
    fn accept<E, V: Visitor<E>>(&mut self, visitor: &V) -> Result<(), E> {
        visitor.visit_type_expr(self)?;
        match self {
            super::TypeExpr::Unit { span: _ }
            | super::TypeExpr::Bool { span: _ }
            | super::TypeExpr::Int { span: _ } => Ok(()),
            super::TypeExpr::Arrow { lhs, rhs, span: _ } => {
                lhs.accept(visitor)?;
                rhs.accept(visitor)?;
                Ok(())
            }
            super::TypeExpr::Tuple { elems, span: _ } => {
                for elem in elems {
                    elem.accept(visitor)?;
                }
                Ok(())
            }
        }
    }
}

impl AST for crate::ast::Pattern {
    fn accept<E, V: Visitor<E>>(&mut self, visitor: &V) -> Result<(), E> {
        visitor.visit_pat(self)?;
        match self {
            super::Pattern::Unit { span: _ } => Ok(()),
            super::Pattern::Var {
                name: _,
                ty,
                span: _,
            } => {
                if let Some(ty) = ty {
                    ty.accept(visitor)?;
                }
                Ok(())
            }
            super::Pattern::Tuple { elems, span: _ } => {
                for elem in elems {
                    elem.accept(visitor)?;
                }
                Ok(())
            }
        }
    }
}

impl AST for crate::ast::Stmt {
    fn accept<E, V: Visitor<E>>(&mut self, visitor: &V) -> Result<(), E> {
        visitor.visit_stmt(self)?;
        match self {
            super::Stmt::Let { lhs, rhs, span: _ } => {
                lhs.accept(visitor)?;
                rhs.accept(visitor)?;
                Ok(())
            }
            super::Stmt::Func(func) => {
                let super::Func {
                    name: _,
                    param,
                    ret_ty,
                    body,
                    span: _,
                } = func.as_mut();
                param.accept(visitor)?;
                ret_ty.accept(visitor)?;
                body.accept(visitor)?;
                Ok(())
            }
            super::Stmt::Block(block) => {
                for stmt in &mut block.stmts {
                    stmt.accept(visitor)?;
                }
                Ok(())
            }
            super::Stmt::Expr { expr, span: _ } => {
                expr.accept(visitor)?;
                Ok(())
            }
            super::Stmt::Empty { span: _ } => Ok(()),
        }
    }
}

impl AST for crate::ast::Expr {
    fn accept<E, V: Visitor<E>>(&mut self, visitor: &V) -> Result<(), E> {
        visitor.visit_expr(self)?;
        match self {
            super::Expr::Unit { span: _ }
            | super::Expr::Int { value: _, span: _ }
            | super::Expr::Bool { value: _, span: _ }
            | super::Expr::Var { name: _, span: _ } => Ok(()),
            super::Expr::UnaryOp {
                op: _,
                arg,
                span: _,
                op_span: _,
            } => {
                arg.accept(visitor)?;
                Ok(())
            }
            super::Expr::BinaryOp {
                op: _,
                lhs,
                rhs,
                span: _,
                op_span: _,
            } => {
                lhs.accept(visitor)?;
                rhs.accept(visitor)?;
                Ok(())
            }
            super::Expr::App { func, arg, span: _ } => {
                func.accept(visitor)?;
                arg.accept(visitor)?;
                Ok(())
            }
            super::Expr::Tuple { elems, span: _ } => {
                for elem in elems {
                    elem.accept(visitor)?;
                }
                Ok(())
            }
            super::Expr::Block(block) => {
                for stmt in &mut block.stmts {
                    stmt.accept(visitor)?;
                }
                Ok(())
            }
        }
    }
}

// typed AST
impl AST for crate::ast::typed::Type {
    fn accept<E, V: Visitor<E>>(&mut self, visitor: &V) -> Result<(), E> {
        visitor.visit_ttype(self)?;
        match self {
            crate::ast::typed::Type::Unit
            | crate::ast::typed::Type::Bool
            | crate::ast::typed::Type::Int
            | crate::ast::typed::Type::TypeVar { index: _ } => Ok(()),
            crate::ast::typed::Type::Arrow { lhs, rhs } => {
                lhs.accept(visitor)?;
                rhs.accept(visitor)?;
                Ok(())
            }
            crate::ast::typed::Type::Tuple { elems } => {
                for elem in elems {
                    elem.accept(visitor)?;
                }
                Ok(())
            }
        }
    }
}

impl AST for crate::ast::typed::Pattern {
    fn accept<E, V: Visitor<E>>(&mut self, visitor: &V) -> Result<(), E> {
        visitor.visit_tpat(self)?;
        match self {
            crate::ast::typed::Pattern::Unit { span: _ } => Ok(()),
            crate::ast::typed::Pattern::Var { name: _, ty, span: _ } => {
                ty.accept(visitor)?;
                Ok(())
            }
            crate::ast::typed::Pattern::Tuple { elems, ty, span: _   } => {
                ty.accept(visitor)?;
                for elem in elems {
                    elem.accept(visitor)?;
                }
                Ok(())
            }
        }
    }
}

impl AST for crate::ast::typed::Expr {
    fn accept<E, V: Visitor<E>>(&mut self, visitor: &V) -> Result<(), E> {
        visitor.visit_texpr(self)?;
        match self {
            crate::ast::typed::Expr::Lit { value: _, span: _ } => Ok(()),
            crate::ast::typed::Expr::Var { name: _, span: _, ty } => {
                ty.accept(visitor)?;
                Ok(())
            },
            crate::ast::typed::Expr::Application {
                callee, arg, ty,
                cons: _,
                span: _,  
            } => {
                callee.accept(visitor)?;
                arg.accept(visitor)?;
                ty.accept(visitor)?;
                Ok(())
            }
            crate::ast::typed::Expr::Let { lhs, rhs, body, span:_, cons:_   } => {
                lhs.accept(visitor)?;
                rhs.accept(visitor)?;
                body.accept(visitor)
            }
            crate::ast::typed::Expr::Seq { seq, ty, span:_, } => {
                ty.accept(visitor)?;
                for expr in seq {
                    expr.accept(visitor)?;
                }
                Ok(())
            }
            crate::ast::typed::Expr::Tuple { elems, ty, span:_, } => {
                ty.accept(visitor)?;
                for elem in elems {
                    elem.accept(visitor)?;
                }
                Ok(())
            }
            crate::ast::typed::Expr::Func { func } => {
                let crate::ast::typed::Func {
                    name: _,
                    param,
                    ret_ty,
                    func_ty,
                    body,
                    span: _,
                } = func.as_mut();
                param.accept(visitor)?;
                ret_ty.accept(visitor)?;
                func_ty.accept(visitor)?;
                body.accept(visitor)?;
                Ok(())
            }
        }
    }
}
