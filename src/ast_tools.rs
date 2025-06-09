use super::ast::*;

impl Expr {
    pub fn span(self: &Expr) -> (usize, usize) {
        
        match self {
            Expr::UnitLit { span } => *span,
            Expr::IntLit { span, .. } => *span,
            Expr::BoolLit { span, .. } => *span,
            Expr::Var { span, .. } => *span,
            Expr::UnaryOp { span, .. } => *span,
            Expr::BinaryOp { span, .. } => *span,
            Expr::Seq { span, .. } => *span,
            Expr::App { span, .. } => *span,
            Expr::Func { span, .. } => *span,
            Expr::Let { span, .. } => *span,
        }
    }
}
