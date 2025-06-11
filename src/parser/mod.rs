use crate::ast::*;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::span::Span;
use function_name::named;
use std::collections::VecDeque;
use std::fmt::Debug;
use std::vec;
pub mod context;
pub mod exprs;
pub mod patterns;
pub mod stmts;
#[cfg(test)]
mod testing;
mod type_exprs;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Trial {
    SpecificTokenType {
        token_type: TokenType,
        rule_name: &'static str,
    },
    PeekerFunction {
        description: &'static str,
        rule_name: &'static str,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErr {
    UnexpectedToken { got: Token },
    UnexpectedTokens { got: Vec<Token> },
}

struct PropertyStack<T> {
    stack: Vec<T>,
    bottom: T,
}

impl<T: Clone> PropertyStack<T> {
    pub fn new(init: T) -> Self {
        PropertyStack {
            stack: vec![],
            bottom: init,
        }
    }

    pub fn push(&mut self, value: T) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) {
        if self.stack.is_empty() {
            panic!("Logic error: PropertyStack cannot be popped when empty. Push/pop imbalance is a serious bug!");
        }
        self.stack.pop();
    }

    pub fn value(&self) -> T {
        self.stack
            .last()
            .map(|x| x.clone())
            .unwrap_or(self.bottom.clone())
    }

    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }
}

impl<T> Drop for PropertyStack<T> {
    fn drop(&mut self) {
        if !self.stack.is_empty() {
            panic!(
                "Logic error: PropertyStack should be empty on drop. Push/pop imbalance is a serious bug!"
            );
        }
    }
}

pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
    trials: Vec<Trial>,
    pub optional_type_anno_in_patterns: PropertyStack<bool>,
}

pub enum PeekerResult {
    /// Stop peeking and consume all previously peeked tokens.
    Success,
    /// Stop peeking and do not consume any previously peeked tokens.
    Fail,
    /// Continue peeking the next token.
    Continue,
}

impl<'a> Debug for Parser<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.lexer.input[self.lexer.pos..])
    }
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            lexer: Lexer::new(input),
            //allow_block_stack: vec![true],
            trials: vec![],
            optional_type_anno_in_patterns: PropertyStack::new(false),
        }
    }

    // fn push_allow_blocks(&mut self, value: bool) {
    //     self.allow_block_stack.push(value);
    // }

    // fn pop_allow_blocks(&mut self) {
    //     self.allow_block_stack.pop();
    // }

    // fn allow_blocks(&self) -> bool {
    //     *self
    //         .allow_block_stack
    //         .last()
    //         .expect("Logic error: allow_blocks stack should not be empty")
    // }

    pub fn peek(&mut self) -> Token {
        return self.lexer.peek_token();
    }

    pub fn consume(&mut self) -> Token {
        return self.lexer.next_token();
    }

    pub fn expect_any_with_peeker(
        &mut self,
        peeker: &mut impl FnMut(&Token) -> PeekerResult,
        description: &'static str,
        rule: &'static str,
    ) -> Result<Vec<Token>, ParseErr> {
        let fn_extend_trials = |parser: &mut Self| {
            parser.trials.push(Trial::PeekerFunction {
                description,
                rule_name: rule,
            })
        };
        let fn_clear_trials = |parser: &mut Self| {
            parser.trials.clear();
        };
        // start peeking
        let saved_pos = self.lexer.pos;
        let mut tokens = vec![];
        let consume: bool = loop {
            let token = self.lexer.next_token();
            tokens.push(token.clone());
            let peeker_result: PeekerResult = peeker(&token);
            match peeker_result {
                PeekerResult::Success => {
                    break true;
                }
                PeekerResult::Continue => (),
                PeekerResult::Fail => {
                    break false;
                }
            }
        };
        if consume {
            fn_clear_trials(self);
            return Ok(tokens);
        } else {
            self.lexer.pos = saved_pos;
            fn_extend_trials(self);
            return Err(ParseErr::UnexpectedTokens { got: tokens });
        }
    }

    pub fn expect_any(
        &mut self,
        token_types: &[TokenType],
        rule: &'static str,
    ) -> Result<Token, ParseErr> {
        let fn_extend_trials = |parser: &mut Self| {
            parser.trials.extend(
                token_types
                    .iter()
                    .map(|tt| Trial::SpecificTokenType {
                        token_type: tt.clone(),
                        rule_name: rule,
                    })
                    .collect::<Vec<_>>(),
            )
        };
        let fn_clear_trials = |parser: &mut Self| {
            parser.trials.clear();
        };
        let token = self.lexer.peek_token();
        if token_types.contains(&token.token_type()) {
            self.lexer.next_token();
            fn_clear_trials(self);
            return Ok(token);
        } else {
            fn_extend_trials(self);
            return Err(ParseErr::UnexpectedToken { got: token });
        }
    }

    pub fn expect(&mut self, token_type: TokenType, rule: &'static str) -> Result<Token, ParseErr> {
        self.expect_any(&[token_type], rule)
    }

    pub fn left_assoc_infix_op<TRes>(
        &mut self,
        op_token_types: &[TokenType],
        rule: &'static str,
        parse_operand: &impl Fn(&mut Self) -> Result<TRes, ParseErr>,
        combine_operands: &impl Fn(TRes, TRes, Token) -> TRes,
    ) -> Result<TRes, ParseErr> {
        let mut lhs = parse_operand(self)?;
        loop {
            if let Ok(op_token) = self.expect_any(op_token_types, rule.clone()) {
                let rhs = parse_operand(self)?;
                lhs = combine_operands(lhs, rhs, op_token);
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    pub fn right_assoc_infix_op<TRes>(
        &mut self,
        op_token_types: &[TokenType],
        rule: &'static str,
        parse_operand: &impl Fn(&mut Self) -> Result<TRes, ParseErr>,
        combine_operands: &impl Fn(TRes, TRes, Token) -> TRes,
    ) -> Result<TRes, ParseErr> {
        let lhs = parse_operand(self)?;
        if let Ok(op_token) = self.expect_any(op_token_types, rule) {
            let rhs =
                self.right_assoc_infix_op(op_token_types, rule, parse_operand, combine_operands)?;
            Ok(combine_operands(lhs, rhs, op_token))
        } else {
            Ok(lhs)
        }
    }

    pub fn variadic_op<T>(
        &mut self,
        op_token_type: TokenType,
        rule: &'static str,
        parse_operand: impl Fn(&mut Self) -> Result<T, ParseErr>,
        combine_operands: impl Fn(Vec<T>) -> T,
    ) -> Result<T, ParseErr> {
        let mut operands = vec![];
        operands.push(parse_operand(self)?);
        loop {
            if let Ok(_) = self.expect(op_token_type.clone(), rule.clone()) {
                operands.push(parse_operand(self)?);
            } else {
                break;
            }
        }
        if operands.len() == 1 {
            return Ok(operands.remove(0));
        }
        Ok(combine_operands(operands))
    }

    pub fn paren<TRes>(
        &mut self,
        rule: &'static str,
        parse_operand: &impl Fn(&mut Self) -> Result<TRes, ParseErr>,
    ) -> Result<TRes, ParseErr> {
        let _left_paren: Token = self.expect(TokenType::LParen, rule)?;
        let res = parse_operand(self)?;
        let _right_paren = self.expect(TokenType::RParen, rule)?;
        Ok(res)
    }

    pub fn ll1_try_parse<T>(
        &mut self,
        parse_fns: &[&dyn Fn(&mut Self) -> Result<T, ParseErr>],
    ) -> Result<T, ParseErr> {
        let saved_lexer_pos = self.lexer.pos;
        for parse_fn in parse_fns {
            match parse_fn(self) {
                Ok(res) => return Ok(res),
                // If the position did not change, it means we simply 'peeked' the next token
                // and decided this won't work. This is called a 'shallow failure'.
                // In this case, we should try the next parse function.
                //
                // If the position did change, it means we had chosen this parse function
                // and went forward just to find out there was a failure. For example, we
                // see "(" and say we want to choose the "(" expr ")" rule, just to find out
                // there was no ")" after that. This is called a 'deep failure'.
                // In this case, we should propagate the error
                // instead of trying the next parse function.
                Err(err) => {
                    if self.lexer.pos != saved_lexer_pos {
                        return Err(err);
                    }
                }
            }
        }
        Err(ParseErr::UnexpectedToken {
            got: self.lexer.peek_token(),
        })
    }

    // fn parse_fn_param_pattern_tuple_operator(&mut self) -> Result<Expr, ParseErr> {
    //     self.parse_left_assoc_binary_op(
    //         TokenType::Comma,
    //         Rule::FnParamPattern,
    //         |p| p.parse_fn_param(),
    //         |lhs, rhs, _op_token| Expr::Tuple {
    //             elems: vec![lhs, rhs],
    //             span: (lhs.span().0, rhs.span().1),
    //         },
    //     );
    //     let name = self.expect(TokenType::Ident, Rule::FnParamPattern)?;
    // }

    // fn parse_fn_param_pattern(&mut self) -> Result<Expr, ParseErr> {}

    // // annotated fn decl:
    // // MUST BE FULLY ANNOTATED
    // //
    // // fn is_even(n : int) -> bool  { n % 2 == 0 }
    // // fn is_greater_than(a: int, b: int) -> bool { a > b }
    // // fn funny(a: int, (b: int, c: int)) -> int ...
    // // --
    // // lambda expression:
    // //
    // // a => a + 1
    // // a: int => a + 1
    // //
    // fn parse_fn_expr(&mut self) -> Result<Expr, ParseErr> {
    //     let fn_kw = self.expect(TokenType::Fn, Rule::FnExpr)?;
    //     let fn_name = self.expect(TokenType::Ident, Rule::FnExpr)?;
    // }
}
