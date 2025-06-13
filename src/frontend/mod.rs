use crate::{
    parser::{ParseErr, Parser},
    sema::typing::{ TypingErr},
};

pub enum FrontEndErr {
    ParseErr(ParseErr),
    TypingErr(TypingErr),
}

pub fn source_to_typed_ast(src: &str) -> Result<crate::ast::typed::Expr, FrontEndErr> {
    let mut parser = Parser::new(src);
    let expr = parser.expr().map_err(|e| FrontEndErr::ParseErr(e))?;
    let expr = expr.to_typed().map_err(|e| FrontEndErr::TypingErr(e))?;
    Ok(expr)
}
