use crate::ast::raw::{
    BasicType, Block, Expr, Func, HasTupleSyntax, Lit, Param, Pattern, Stmt, TypeExpr,
};
use crate::ast::span::*;
use crate::container::Container;
use crate::container::Id;
use peg::error::ParseError;
use peg::str::LineCol;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Mutex;

fn binary_op<C: Container>(
    ctx: Rc<RefCell<C>>,
    op: &str,
    op_span: Span,
    mut lhs: Expr,
    mut rhs: Expr,
) -> Expr {
    let span = lhs.join_spans(&mut rhs);
    Expr::App {
        func: Box::new(Expr::Var {
            name: ctx.borrow_mut().encode_f(op.to_string()),
            span: Some(op_span),
        }),
        args: vec![lhs, rhs],
        span: span,
    }
}

fn join_into_tuple<C: Container, T: GetSpanRef + HasTupleSyntax>(
    ctx: Rc<RefCell<C>>,
    mut lhs: T,
    mut rhs: T,
) -> T {
    let span = lhs.join_spans(&rhs);
    match (lhs.break_tuple(), rhs.break_tuple()) {
        (Ok(mut l_elems), Ok(r_elems)) => {
            l_elems.extend(r_elems);
            T::make_tuple(l_elems, span)
        }
        (Ok(mut elems), Err(rhs)) => {
            elems.push(rhs);
            T::make_tuple(elems, span)
        }
        (Err(lhs), Ok(mut elems)) => {
            elems.insert(0, lhs);
            T::make_tuple(elems, span)
        }
        (Err(lhs), Err(rhs)) => T::make_tuple(vec![lhs, rhs], span),
    }
}

peg::parser! {
    grammar sight() for str {

        /// Optional whitespace
        rule _ = __ *

        /// Required whitespace
        rule __ = [' ' | '\t' | '\n' | '\r']

        /// Brackets that enclose content.
        /// Allows leading and trailing spaces.
        rule generic_brackets<T>(
            lb: rule<()>,
            rb: rule<()>,
            content: rule<T>
        ) -> T =
            lb() _ c:content() _ rb() { c }

        /// Parentheses that enclose content of trait HasTupleSyntax.
        /// Converts tuples in the content into closed tuples.
        rule paren< T: HasTupleSyntax>(
            content: rule<T>
        ) -> T =
            lpos:position!() p:generic_brackets(<"(">, <")">, content) rpos:position!() {
                match p.break_tuple() {
                    Ok(elems) => T::make_closed_tuple(elems, Some(Span(lpos, rpos))),
                    Err(not_tuple) => not_tuple
                }
            }

        /// Parentheses that enclose content.
        rule paren_raw<T>(
            content: rule<T>
        ) -> T =
            p:generic_brackets(<"(">, <")">, content) {
                p
            }

        /// Braces that enclose content.
        rule braced< T>(
            content: rule<T>
        ) -> T =
            generic_brackets(<"{">, <"}">, content)

        /// A comma-separated list.
        rule comma_list< T>(
            content: rule<T>
        ) -> Vec<T> =
            lpos:position!() c:content() ** (_ "," _) rpos:position!() {
                c
            }

        /// A name.
        rule name<C: Container,>(ctx: Rc<RefCell<C>>) -> Id<String> =
            n:$(quiet!{[c if c.is_alphabetic() || c == '_' ][c if c.is_alphanumeric()|| c == '_']*}
            / expected!("name")) {
                ctx.clone().borrow_mut().encode_f(n.to_string())
            }

        rule unit_lit<C: Container,>(ctx: Rc<RefCell<C>>) -> () =
            lpos:position!() paren_raw(<_()>) rpos:position!() {
            }

        rule int_lit<C: Container,>(ctx: Rc<RefCell<C>>) -> i32 =
            n:$(quiet!{['0'..='9']+}/expected!("integer literal")) {
                n.parse().unwrap()
            }

        rule bool_lit<C: Container,>(ctx: Rc<RefCell<C>>) -> bool =
            "true" { true }
            / "false" { false }

        /// A literal value can be used as an expression or a pattern.
        rule literal<C: Container,>(ctx: Rc<RefCell<C>>) -> Lit =
            unit_lit(ctx.clone()) { Lit::Unit }
            / v:bool_lit(ctx.clone()) { Lit::Bool(v) }
            / v:int_lit(ctx.clone()) { Lit::Int(v) }

        /// A basic type that does not have structure.
        rule basic_type<C: Container,>(ctx: Rc<RefCell<C>>) -> BasicType =
            unit_lit(ctx.clone()) { BasicType::Unit }
            / "bool" { BasicType::Bool }
            / "int" { BasicType::Int }

        rule literal_expr<C: Container,>(ctx: Rc<RefCell<C>>) -> Expr =
            l:position!() value:literal(ctx.clone()) r:position!() {
                Expr::Lit {
                    value: value,
                    span: Some(Span(l, r)),
                }
            }

        rule literal_pattern<C: Container,>(ctx: Rc<RefCell<C>>) -> Pattern =
            l:position!() value:literal(ctx.clone()) r:position!() {
                Pattern::Lit {
                    value: value,
                    span: Some(Span(l, r)),
                }
            }

        rule basic_type_expr<C: Container,>(ctx: Rc<RefCell<C>>) -> TypeExpr =
            l:position!() value:basic_type(ctx.clone()) r:position!() {
                TypeExpr::Basic {
                    t: value,
                    span: Some(Span(l, r)),
                }
            }

        rule var_expr<C: Container,>(ctx: Rc<RefCell<C>>) -> Expr =
            l:position!() var: name(ctx.clone()) r:position!() {
                Expr::Var {
                    name: var,
                    span: Some(Span(l, r)),
                }
            }

        rule var_pattern<C: Container,>(ctx: Rc<RefCell<C>>) -> Pattern =
            l:position!() var: name(ctx.clone()) r:position!() {
                Pattern::Var {
                    name: var,
                    span: Some(Span(l, r)),
                }
            }

        rule var_type_expr<C: Container,>(ctx: Rc<RefCell<C>>) -> TypeExpr =
            l:position!() var: name(ctx.clone()) r:position!() {
                TypeExpr::Var {
                    name: var,
                    span: Some(Span(l, r)),
                }
            }

        rule type_annotation<C: Container,>(ctx: Rc<RefCell<C>>) -> TypeExpr =
            ":" _ ty:type_expr(ctx.clone()) {
                ty
            }

        rule param<C: Container,>(ctx: Rc<RefCell<C>>) -> Param =
            lpos:position!() name:name(ctx.clone()) _ ty:type_annotation(ctx.clone()) rpos:position!() {
                Param {
                    name,
                    ty_ann: ty,
                    span: Some(Span(lpos, rpos)),
                }
            }

        rule param_list<C: Container,>(ctx: Rc<RefCell<C>>) -> (Vec<Param>, Span) =
            lpos:position!() params:comma_list( <param(ctx.clone())>) rpos:position!() {
                (params, Span(lpos, rpos))
            }

        rule param_list_with_paren<C: Container,>(ctx: Rc<RefCell<C>>) -> (Vec<Param>, Span) =
            params:paren_raw(<param_list(ctx.clone())>) {
                params
            }

        rule type_list<C: Container,>(ctx: Rc<RefCell<C>>) -> (Vec<TypeExpr>, Span) =
            lpos:position!() types:comma_list( <type_expr(ctx.clone())>) rpos:position!() {
                (types, Span(lpos, rpos))
            }

        rule type_list_with_paren<C: Container,>(ctx: Rc<RefCell<C>>) -> (Vec<TypeExpr>, Span) =
            types:paren_raw(<type_list(ctx.clone())>) {
                types
            }

        rule arg_list<C: Container,>(ctx: Rc<RefCell<C>>) -> (Vec<Expr>, Span) =
            lpos:position!() params:comma_list( <expr(ctx.clone())>) rpos:position!() {
                (params, Span(lpos, rpos))
            }

        rule arg_list_with_paren<C: Container,>(ctx: Rc<RefCell<C>>) -> (Vec<Expr>, Span) =
            params:paren_raw(<arg_list(ctx.clone())>) {
                params
            }

        pub rule expr<C: Container,>(ctx: Rc<RefCell<C>>) -> Expr = precedence!{
            // plus/minus
            lhs:(@) _ op_lpos: position!() op:$("+" / "-") op_rpos: position!() _ rhs:@ {
                binary_op(ctx.clone(), op, Span(op_lpos, op_rpos), lhs, rhs)
            }
            --
            // mul/div
            lhs:(@) _ op_lpos: position!() op:$("*" / "/") op_rpos: position!() _ rhs:@ {
                binary_op(ctx.clone(), op, Span(op_lpos, op_rpos), lhs, rhs)
            }
            --
            // function call
            lhs:(@) args: arg_list_with_paren(ctx.clone())  {
                let (args, args_span) = args;
                let span = lhs.get_span().map(|lhs_span| {
                   lhs_span + args_span
                });
                Expr::App {
                    func: Box::new(lhs),
                    args: args,
                    span: span,
                }
            }
            --
            e: paren(<expr(ctx.clone())>) { e }
            e: block_expr(ctx.clone()) { e }
            e: literal_expr(ctx.clone()) { e }
            e: var_expr(ctx.clone()) { e }
        }

        pub rule pattern<C: Container,>(ctx: Rc<RefCell<C>>) -> Pattern = precedence!{
            // tuple
            lhs:(@) _ op:$(",") _ rhs:@ {
                join_into_tuple(ctx.clone(), lhs, rhs)
            }
            --
            e: paren(<pattern(ctx.clone())>) { e }
            e: literal_pattern(ctx.clone()) { e }
            e: var_pattern(ctx.clone()) { e }
        }

        pub rule let_stmt<C: Container,>(ctx: Rc<RefCell<C>>) -> Stmt =
            "let" __ lpos:position!() lhs:name(ctx.clone()) rpos:position!() ty:type_annotation(ctx.clone())? _ "=" _ rhs:expr(ctx.clone()) _ ";" {
                Stmt::Let {
                    lhs,
                    ty_ann: ty,
                    rhs,
                    name_span: Some(Span(lpos, rpos)),
                }
            }

        pub rule if_stmt<C: Container>(ctx: Rc<RefCell<C>>) -> Stmt =
            "if" __
            cond:expr(ctx.clone()) _
            then_br: block(ctx.clone()) _
            "else" _
            else_br: block(ctx.clone()) {
                Stmt::If {
                    cond,
                    then_br,
                    else_br,
                }
            }

        pub rule while_stmt<C: Container>(ctx: Rc<RefCell<C>>) -> Stmt =
            "while" __
            cond:expr(ctx.clone()) _
            body: block(ctx.clone()) {
                Stmt::While { cond , body  }
            }

        pub rule func_stmt<C: Container,>(ctx: Rc<RefCell<C>>) -> Stmt =
            "fun" __
            lpos:position!() name:name(ctx.clone()) rpos:position!() _
            params:param_list_with_paren(ctx.clone()) _
            ret_ty: type_annotation(ctx.clone()) _
            body:block(ctx.clone()) {
                Stmt::Func {
                    func: Box::new(Func {
                        name,
                        params: params.0,
                        ret_ty_ann: ret_ty,
                        body,
                        name_span: Some(Span(lpos, rpos)),
                    }),
                }
            }

        pub rule expr_stmt<C: Container,>(ctx: Rc<RefCell<C>>) -> Stmt =
            e:expr(ctx.clone()) _ ";" {
                Stmt::Expr {
                    expr: e,
                }
            }

        pub rule stmt<C: Container,>(ctx: Rc<RefCell<C>>) -> Stmt =
            s: (
                let_stmt(ctx.clone())
                / func_stmt(ctx.clone())
                / expr_stmt(ctx.clone())
            ) { s }

        rule block_content<C: Container,>(ctx: Rc<RefCell<C>>) -> (Vec<Stmt>, Option<Expr>) =
            stmts: stmt(ctx.clone())**_ _ value: expr(ctx.clone())? {
                (stmts, value)
            }

        pub rule block<C: Container,>(ctx: Rc<RefCell<C>>) -> Block =
            lpos: position!() c:braced(<block_content(ctx.clone())>) rpos: position!() {
                let (stmts, value) = c;
                Block {
                    stmts,
                    value,
                    span: Some(Span(lpos, rpos)),
                }
            }

        rule block_expr<C: Container,>(ctx: Rc<RefCell<C>>) -> Expr =
            b:block(ctx.clone()) {
                Expr::Block(Box::new(b))
            }

        pub rule type_expr<C: Container,>(ctx: Rc<RefCell<C>>) -> TypeExpr = precedence!{
            // the magic rule
            lpos:position!() e:@ rpos:position!() {
                let mut e: TypeExpr = e;
                *e.get_span_mut() = Some(Span(lpos, rpos));
                e
            }
            --
            // arrow type: a -> b
            lhs:@ _ op:$("->") _ rhs:(@) {
                TypeExpr::Arrow {
                    span: lhs.join_spans(&rhs),
                    lhs: vec![lhs],
                    rhs: Box::new(rhs),
                }
            }
            // arrow type: (a, b) -> c
            lhs:type_list_with_paren(ctx.clone()) _ "->" _ rhs:(@) {
                TypeExpr::Arrow {
                    span: rhs.get_span().map(|rhs_span| Span(lhs.1.0, rhs_span.1)),
                    lhs: lhs.0,
                    rhs: Box::new(rhs),
                }
            }
            --
            e: paren(<type_expr(ctx.clone())>) { e }
            e: basic_type_expr(ctx.clone()) { e }
            e: var_type_expr(ctx.clone()) { e }
        }
    }
}

pub fn parse_expr(
    input: &str,
    ctx: Rc<RefCell<impl Container>>,
) -> Result<Expr, ParseError<LineCol>> {
    let a = sight::expr(input, ctx);
    a
}

pub fn parse_func(
    input: &str,
    ctx: Rc<RefCell<impl Container>>,
) -> Result<Stmt, ParseError<LineCol>> {
    let a = sight::func_stmt(input, ctx);
    a
}
