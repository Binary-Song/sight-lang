use crate::{
    parser::{ParseErr, Parser},
    sema::typing::{self, TypingErr},
};

pub enum FrontEndErr {
    ParseError(ParseErr),
    TypingError(TypingErr),
}

pub fn source_to_typed_ast(src: &str) -> Result<crate::ast::typed::Expr, FrontEndErr> {
    let mut parser = Parser::new(src);
    let expr = parser.expr().map_err(|e| FrontEndErr::ParseError(e))?;
    let expr = expr.to_typed().map_err(|e| FrontEndErr::TypingError(e))?;
    Ok(expr)
}
