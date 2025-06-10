use crate::ast::*;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::parser::*;
use crate::span::Span;
use function_name::named;
use std::collections::VecDeque;
use std::rc::Rc;
use std::vec;
use tracing::instrument;

#[derive(Debug, Clone, PartialEq, Eq)]
enum ParseStmtResult {
    /// A Stmt was successfully parsed.
    Ok(Stmt),
    /// A `}` was immediately encountered and consumed. No Stmt parsed.
    BlockEnded,
    /// An trailing expr was parsed, then `}` was encountered and consumed.
    BlockEndedWithExpr(Expr),
    /// A deep or shallow failure.
    Err(ParseErr),
}

impl<'a> Parser<'a> {
    #[named]
    #[instrument(ret)]
    pub fn let_stmt(&mut self) -> Result<Stmt, ParseErr> {
        let rule = function_name!();
        let _let = self.expect(TokenType::Let, rule)?;
        let pattern = self.pattern_with_optional_type_anno()?;
        let _eq = self.expect(TokenType::Eq, rule)?;
        let rhs = self.expr()?;
        let _semi = self.expect(TokenType::Semicolon, rule)?;
        let span = (_let.span().0, rhs.span().1);
        Ok(Stmt::Let {
            lhs: pattern,
            rhs: rhs,
            span,
        })
    }

    #[named]
    #[instrument(ret)]
    pub fn fn_stmt(&mut self) -> Result<Stmt, ParseErr> {
        let rule = function_name!();
        let _fn = self.expect(TokenType::Fn, rule)?;
        let name = self.expect(TokenType::Ident, rule)?;
        let _lpran = self.peek();
        self.trials.push(Trial::SpecificTokenType {
            token_type: TokenType::LParen,
            rule_name: rule,
        });
        if _lpran.token_type() != TokenType::LParen {
            return Err(ParseErr::UnexpectedToken { got: _lpran });
        } else {
            self.trials.clear();
        }
        let param_pattern = self.pattern_with_max_prec(patterns::Prec::Primary)?;
        let _arrow = self.expect(TokenType::Arrow, rule)?;
        let ret_type = self.type_expr()?;
        let body = self.block()?;
        let name = if let Token::Ident(name, _) = name {
            name
        } else {
            panic!("Logic error: expected function name to be an identifier");
        };
        let span = (_fn.span().0, body.span.1);
        Ok(Stmt::Func(Rc::new(Func {
            name: name,
            param: param_pattern,
            ret_ty: ret_type,
            body: Expr::Block(Box::new(body)),
            span: span,
        })))
    }

    #[named]
    #[instrument(ret)]
    pub fn empty_stmt(&mut self) -> Result<Stmt, ParseErr> {
        let rule = function_name!();
        let semicolon = self.expect(TokenType::Semicolon, rule)?;
        Ok(Stmt::Empty {
            span: semicolon.span(),
        })
    }

    /// Parse a statement. We assume this function is only called inside a block. Because we use
    /// the "}" as a signal to end the block.
    #[named]
    #[instrument(ret)]
    fn stmt(&mut self) -> ParseStmtResult {
        let rule = function_name!();
        let old_pos: usize = self.lexer.pos;
        let parse_stmt_result = self.ll1_try_parse(&[
            &|parser: &mut Parser<'a>| parser.let_stmt(),
            &|parser: &mut Parser<'a>| parser.fn_stmt(),
            &|parser: &mut Parser<'a>| parser.block().map(|block| Stmt::Block(block)),
            &|parser: &mut Parser<'a>| parser.empty_stmt(),
        ]);
        let new_pos = self.lexer.pos;
        if let Ok(stmt) = parse_stmt_result {
            return ParseStmtResult::Ok(stmt);
        }
        // A "deep fail". See `ll1_try_parse` 's docs for an definition of a deep/shallow fail.
        if new_pos != old_pos {
            return ParseStmtResult::Err(parse_stmt_result.unwrap_err());
        }
        // A "shallow fail". Meaning the next token is not a beginning of a stmt like let, fn, etc.
        // If the next is "}", return.
        if TokenType::RBrace == self.peek().token_type() {
            return ParseStmtResult::BlockEnded;
        }
        // Otherwise, we try to parse an expression.
        let parse_expr_result = self.expr();
        if let Err(err) = parse_expr_result {
            return ParseStmtResult::Err(err);
        }
        let expr = parse_expr_result.unwrap();
        let next_token = self.peek();
        if next_token.token_type() == TokenType::Semicolon {
            // eat semi
            self.consume();
            let span: (usize, usize) = (expr.span().0, next_token.span().1);
            return ParseStmtResult::Ok(Stmt::Expr {
                expr: expr,
                span: span,
            });
        } else if next_token.token_type() == TokenType::RBrace {
            // do not eat `}`
            return ParseStmtResult::BlockEndedWithExpr(expr);
        } else {
            self.trials.push(Trial::SpecificTokenType {
                token_type: TokenType::Semicolon,
                rule_name: rule,
            });
            self.trials.push(Trial::SpecificTokenType {
                token_type: TokenType::RBrace,
                rule_name: rule,
            });
            return ParseStmtResult::Err(ParseErr::UnexpectedToken { got: next_token });
        }
    }

    // `{` -> awaiting stmt -> fn/let/... (exec simple ll1 rules) -> push stmt -> awaiting stmt
    //                       | ";" (exec empty stmt rule) -> push stmt  -> awaiting stmt
    //                       | "{" (exec block rule) -> push stmt -> awaiting stmt
    //                       | "}" (finish block) -> the block has no trailing expr -> return
    //                       | others (exec non-block expr rule) -> ";" -> push stmt -> awaiting stmt
    //                                                            | "}" -> the block has trailing expr -> return
    #[named]
    #[instrument(ret)]
    pub fn block(&mut self) -> Result<Block, ParseErr> {
        // if !self.allow_blocks() {
        //     return Err(ParseErr::UnexpectedToken {
        //         got: self.lexer.peek_token(),
        //     });
        // }
        let rule = function_name!();
        let mut stmts = vec![];
        let lbrace = self.expect(TokenType::LBrace, rule)?;
        let trailing_expr = loop {
            let stmt_result = self.stmt();
            match stmt_result {
                ParseStmtResult::Ok(stmt) => {
                    stmts.push(stmt);
                }
                ParseStmtResult::BlockEnded => {
                    break None;
                }
                ParseStmtResult::BlockEndedWithExpr(expr) => {
                    break Some(expr);
                }
                ParseStmtResult::Err(err) => {
                    return Err(err);
                }
            }
        };
        let rbrace = self.expect(TokenType::RBrace, rule)?;
        if let Some(e) = trailing_expr {
            let span = (lbrace.span().0, rbrace.span().1);
            stmts.push(Stmt::Expr {
                expr: e,
                span: span,
            });
        } else {
            // add a fake unit expr to the block if it has no trailing expr
            stmts.push(Stmt::Expr {
                expr: Expr::Unit {
                    span: rbrace.span(),
                },
                span: rbrace.span(),
            });
        }
        let span = (lbrace.span().0, rbrace.span().1);
        return Ok(Block { stmts, span });
    }
}
