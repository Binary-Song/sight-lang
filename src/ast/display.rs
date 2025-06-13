use crate::ast::{typed, BinaryOp, Block, Expr, Pattern, Stmt, TypeExpr, UnaryOp};
use std::fmt::Display;
use std::fmt::{self};
use typed::Expr as TExpr;

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}
impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}
pub struct Parenthesized<T: Display>(T, Option<(char, char)>);

pub trait WithPrec<TPrec: Into<usize>>: Display {
    /// Returns the precedence of the ast.
    /// None means never put parentheses around it, but always put parentheses around its children.
    fn prec(&self) -> Option<TPrec>;
    fn paren_if_needed<'a>(&'a self, parent: &Self) -> Parenthesized<&'a Self> {
        let is_paren_needed = match (self.prec(), parent.prec()) {
            (Some(child_prec), Some(parent_prec)) => {
                let child_prec: usize = child_prec.into();
                let parent_prec: usize = parent_prec.into();
                child_prec >= parent_prec
            }
            (None, Some(_)) => false,
            (Some(_), None) => true,
            (None, None) => true,
        };
        if is_paren_needed {
            Parenthesized(self, Some(('(', ')')))
        } else {
            Parenthesized(self, None)
        }
    }
}

impl TypeExpr {}

impl<T: Display> Display for Parenthesized<&T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (content, parens) = (self.0, self.1);
        match parens {
            Some((open, close)) => write!(f, "{}{}{}", open, content, close),
            None => write!(f, "{}", content),
        }
    }
}

impl Display for TypeExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeExpr::Unit { .. } => write!(f, "()"),
            TypeExpr::Int { .. } => write!(f, "int"),
            TypeExpr::Bool { .. } => write!(f, "bool"),
            TypeExpr::Tuple { elems, .. } => {
                for e in elems {
                    write!(f, "{}, ", e.paren_if_needed(self))?;
                }
                Ok(())
            }
            TypeExpr::Arrow { lhs, rhs, .. } => {
                write!(
                    f,
                    "{} -> {}",
                    lhs.paren_if_needed(self),
                    rhs.paren_if_needed(self)
                )
            }
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Unit { .. } => write!(f, "()"),
            Pattern::Var { name, .. } => write!(f, "{name}"),
            Pattern::Tuple { elems, .. } => {
                for e in elems {
                    write!(f, "{}, ", e.paren_if_needed(self))?;
                }
                Ok(())
            }
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Let { lhs, rhs, .. } => write!(f, "let {} = {}; ", lhs, rhs),
            Stmt::Func(func) => {
                let super::Func {
                    name,
                    param,
                    ret_ty,
                    body,
                    ..
                } = func.as_ref();
                write!(
                    f,
                    "fn {name}({param}) -> {ret_ty} {{ {body} }}",
                    name = name,
                    param = param,
                    ret_ty = ret_ty,
                    body = body
                )?;
                Ok(())
            }
            Stmt::Expr { expr, .. } => write!(f, "{}; ", expr),
            Stmt::Block(block) => write!(f, "{}", block),
            Stmt::Empty { .. } => write!(f, ";"),
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.stmts {
            write!(f, "{stmt};")?;
        }
        Ok(())
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Unit { .. } => write!(f, "()"),
            Expr::Int { value, .. } => write!(f, "{value}"),
            Expr::Bool { value, .. } => write!(f, "{value}"),
            Expr::Var { name, .. } => write!(f, "{name}"),
            Expr::UnaryOp { op, arg, .. } => write!(f, "{op}{arg}"),
            Expr::BinaryOp { op, lhs, rhs, .. } => write!(
                f,
                "{lhs} {op} {rhs}",
                lhs = lhs.paren_if_needed(self),
                rhs = rhs.paren_if_needed(self),
            ),
            Expr::App { func, arg, .. } => {
                write!(
                    f,
                    "{} {}",
                    func.paren_if_needed(self),
                    arg.paren_if_needed(self)
                )
            }
            Expr::Tuple { elems, .. } => {
                for e in elems {
                    write!(f, "{}, ", e.paren_if_needed(self))?;
                }
                Ok(())
            }
            Expr::Block(block) => {
                write!(f, "{}", block)
            }
        }
    }
}

impl Display for typed::Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            typed::Lit::Unit => write!(f, "()"),
            typed::Lit::Int(i) => write!(f, "{i}"),
            typed::Lit::Bool(b) => write!(f, "{b}"),
        }
    }
}

impl Display for typed::Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn {}({}) -> {} {{ {} }}",
            self.name, self.param, self.ret_ty, self.body
        )
    }
}

impl Display for typed::Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            typed::Pattern::Unit { .. } => write!(f, "()"),
            typed::Pattern::Var { name, .. } => write!(f, "{name}"),
            typed::Pattern::Tuple { elems, .. } => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for typed::Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            typed::Type::Unit => write!(f, "()"),
            typed::Type::Bool => write!(f, "bool"),
            typed::Type::Int => write!(f, "int"),
            typed::Type::Arrow { lhs, rhs } => write!(f, "{} -> {}", lhs, rhs),
            typed::Type::Tuple { elems } => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            typed::Type::TypeVar { index } => write!(f, "T{}", index),
        }
    }
}

impl Display for typed::Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TExpr::Lit { value, .. } => write!(f, "{value}"),
            TExpr::Var { name, .. } => write!(f, "{name}"),
            TExpr::Application { callee, arg, .. } => write!(f, "{} {}", callee, arg),
            TExpr::Let { lhs, rhs, body, .. } => {
                write!(f, "let {} = {} in ({})", lhs, rhs, body)
            }
            TExpr::Seq { seq, .. } => {
                for expr in seq {
                    write!(f, "{};", expr)?;
                }
                Ok(())
            }
            TExpr::Tuple { elems, .. } => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            TExpr::Func { func } => write!(f, "{}", func),
        }
    }
}
