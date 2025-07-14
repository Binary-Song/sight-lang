use super::inference::unify;
use super::inference::Type as ConType;
use super::inference::TypeIdMapper;
use crate::ast as u;
use crate::ast::id::Id;
use crate::ast::typed::ArenaItem;
use crate::ast::typed::GetArena;
use crate::ast::typed::{self as t, BindingData, FunctionType};
use crate::ast::typed::{Arena, BindingId};
use crate::sema::inference::{Constraint, Solution};
use crate::utils::interning::GetInterner;
use crate::utils::interning::Internable;
use crate::utils::interning::{InternString, Interner, StaticInternable};
use core::panic;
use std::collections::VecDeque;
use crate::context::Context;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    UnboundVar {
        name: InternString,
        span: (usize, usize),
    },
    DuplicateBinding {
        binding: BindingId,
    },
    CannotUnify {
        lhs: t::TypeId,
        rhs: t::TypeId,
    },
}

pub type TypeRes<T> = Result<T, TypeError>;

impl Context {

    fn bump_fresh_type_var(&mut self) -> t::TypeId {
        let index = self.next_type_var;
        self.next_type_var += 1;
        t::VariableType { index }.to_type().en(self)
    }

    fn derive_binding(&mut self, binding_head: BindingId, data: BindingData) -> BindingId {
        let binding = binding_head.derive(data);
        let binding_head = binding.new_id(self);
        binding_head
    }

    fn get_binding_type(&mut self, binding: BindingId) -> Option<t::TypeId> {
        match binding.de(self).data {
            BindingData::Variable { ty, .. } => Some(ty),
            BindingData::FunctionDecl {
                param_type,
                return_type,
                ..
            } => Some(
                t::FunctionType {
                    lhs: param_type,
                    rhs: return_type,
                }
                .to_type()
                .en(self),
            ),
            BindingData::Empty => None,
        }
    }

    fn get_pattern_type(&mut self, pattern: t::PatternId) -> t::TypeId {
        match pattern {
            t::PatternId::Variable(variable_pattern_id) => {
                let variable_pattern = variable_pattern_id.de(self);
                variable_pattern.ty
            }
            t::PatternId::Tuple(tuple_pattern_id) => {
                let tuple_pattern = tuple_pattern_id.de(self);
                tuple_pattern.ty
            }
        }
    }

    fn get_expr_type(&mut self, expr: t::ExprId) -> t::TypeId {
        match expr {
            t::ExprId::Literal(literal_expr_id) => {
                let literal_expr = literal_expr_id.de(self);
                literal_expr.ty
            }
            t::ExprId::Application(app_expr_id) => {
                let app_expr = app_expr_id.de(self);
                app_expr.ty
            }
            t::ExprId::Tuple(tuple_expr_id) => {
                let tuple_expr = tuple_expr_id.de(self);
                tuple_expr.ty
            }
            t::ExprId::Variable(id) => {
                let variable_expr = id.de(self);
                variable_expr.ty
            }
            t::ExprId::Block(id) => {
                let block_expr = id.de(self);
                block_expr.block.de(self).ty
            }
            t::ExprId::Projection(id)   => {
                let projection_expr = id.de(self);
                projection_expr.ty
            }
        }
    }

    pub fn type_of_type_expr(&mut self, type_expr: &u::TypeExpr) -> t::TypeId {
        let ty = match type_expr {
            u::TypeExpr::Unit { span: _ } => t::TupleType { elems: vec![] }.to_type(),
            u::TypeExpr::Int { span: _ } => t::PrimitiveType::Int.to_type(),
            u::TypeExpr::Bool { span: _ } => t::PrimitiveType::Bool.to_type(),
            u::TypeExpr::Arrow { lhs, rhs, span: _ } => {
                let lhs = self.type_of_type_expr(lhs);
                let rhs = self.type_of_type_expr(rhs);
                t::FunctionType { lhs, rhs }.to_type()
            }
            u::TypeExpr::Tuple { elems, span: _ } => {
                let mut elem_types = vec![];
                let mut ty;
                for e in elems {
                    ty = self.type_of_type_expr(e);
                    elem_types.push(ty);
                }
                t::TupleType { elems: elem_types }.to_type()
            }
        };
        ty.en(self)
    }

    /// Maps the pattern to a typed one.
    /// Also gives you a binding head derived from the `binding_head` parameter, which includes
    /// the names introduced by this pattern.
    pub fn type_pattern(
        &mut self,
        mut binding_head: BindingId,
        pattern: &u::Pattern,
    ) -> (t::PatternId, BindingId) {
        match pattern {
            u::Pattern::Var {
                name: name,
                ty,
                span: span,
            } => {
                let name = InternString::from_str(&name.as_str());
                let ty = match ty {
                    None => self.bump_fresh_type_var(),
                    Some(ty) => self.type_of_type_expr(ty),
                };
                let binding = self.derive_binding(binding_head, BindingData::Variable { name, ty });
                let variable_pattern = t::VariablePattern {
                    binding_id: binding,
                    ty,
                    span: *span,
                };
                let variable_pattern_id = self.arena.bind_new_id_to(variable_pattern);
                (t::PatternId::Variable(variable_pattern_id), binding)
            }
            u::Pattern::Tuple { elems, span } => {
                let mut patts = vec![];
                for elem in elems {
                    let (patt, new_head) = self.type_pattern(binding_head, elem);
                    binding_head = new_head;
                    patts.push(patt);
                }
                let tup_patt = t::TuplePattern {
                    elems: patts,
                    ty: t::TupleType { elems: vec![] }.to_type().en(self),
                    span: *span,
                };
                (
                    t::PatternId::Tuple(self.arena.bind_new_id_to(tup_patt)),
                    binding_head,
                )
            }
            u::Pattern::Unit { span } => {
                let tup = u::Pattern::Tuple {
                    elems: vec![],
                    span: *span,
                };
                self.type_pattern(binding_head, &tup)
            }
        }
    }

    /// Maps the block to a typed one.           
    pub fn type_block(
        &mut self,
        mut binding_head: BindingId,
        block: &u::Block,
    ) -> TypeRes<Id<t::Block>> {
        // When iterating over the statements, 2 passes are made:
        // 1. Functions are "forward declared" by adding the binding into the context.
        //    A body ID is created for the function, but the body is not yet defined.
        // 2. The statements are processed, and the functions are defined with their bodies.
        let mut second_pass_data = vec![];
        struct FnData {
            fn_binding_id: BindingId,
            param_binding_begin: BindingId,
            param_binding_last: BindingId,
            name: InternString,
            typed_param: t::PatternId,
            return_type: t::TypeId,
            body_id: t::BodyId,
        }
        // first pass
        for stmt in &block.stmts {
            let data = match stmt {
                u::Stmt::Func(func) => {
                    // forward declare (make sure ctx is mutated!)
                    let name = InternString::from_str(&func.name.as_str());
                    // create an empty binding head,
                    // let the [type_pattern] function dump the names into it.
                    let param_binding_begin = self.arena.bind_new_id_to(t::Binding {
                        parent: None,
                        data: BindingData::Empty,
                    });
                    let (typed_param, param_binding_last) =
                        self.type_pattern(param_binding_begin, &func.param);
                    let param_type = self.get_pattern_type(typed_param);
                    let return_type = self.type_of_type_expr(&func.ret_ty);
                    let body_id = self.arena.unbound_body_id();
                    binding_head = self.derive_binding(
                        binding_head,
                        BindingData::FunctionDecl {
                            name: name,
                            param_type: param_type,
                            return_type: return_type,
                            body_id,
                        },
                    );
                    let fn_binding_id = binding_head;
                    (
                        stmt,
                        Some(FnData {
                            fn_binding_id,
                            param_binding_begin,
                            param_binding_last,
                            name,
                            typed_param,
                            return_type,
                            body_id,
                        }),
                    )
                }
                _ => (stmt, None),
            };
            second_pass_data.push(data);
        }
        // Now binding_head contains all functions in this block.
        // We now branch off the binding_head into let_head and binding_head.
        // 1. let_head: this contains block functions and will
        //     include the let bindings when we add them in the second pass.
        //     This is used to type the statements in the block.
        // 2. binding_head: this contains block functions only and is used
        //     to type the function bodies.
        let mut let_head = binding_head;
        // second pass (beware if the global ctx is modified)
        let mut typed_stmts: Vec<t::StmtId> = vec![];
        for (stmt, fn_data) in second_pass_data {
            let typed_stmt = match stmt {
                u::Stmt::Let { lhs, rhs, span } => {
                    let lhs_typed;
                    // rhs cannot use names from lhs
                    let rhs_typed = self.type_expr(let_head, rhs)?;
                    // adds the bindings to the context
                    (lhs_typed, let_head) = self.type_pattern(let_head, lhs);
                    // create constraint
                    let clhs = self.get_pattern_type(lhs_typed);
                    let crhs = self.get_expr_type(rhs_typed);
                    let c = self.arena.bind_new_id_to(Constraint {
                        lhs: clhs,
                        rhs: crhs,
                    });
                    let id = self.arena.bind_new_id_to(t::LetStmt {
                        lhs: lhs_typed,
                        rhs: rhs_typed,
                        span: *span,
                        constraint: c,
                    });
                    t::StmtId::Let(id)
                }
                u::Stmt::Func(func) => {
                    // add param to ctx
                    let data = fn_data.unwrap();
                    // param_binding_begin used to have a None parent
                    // now make it point to the binding_head to make
                    // block functions available in the body (which allows
                    // recursions)
                    self.arena.deref_mut(data.param_binding_begin).parent = Some(binding_head);
                    let body = self.type_block(data.param_binding_last, &func.body)?;
                    let body = body.de(self).clone();
                    self.arena.bind_id_to(data.body_id, body);
                    let fn_stmt = t::FunctionStmt {
                        new_fn_id: data.fn_binding_id,
                        param: data.typed_param,
                        ret_ty: data.return_type,
                        body: data.body_id,
                        span: func.span,
                    };
                    t::StmtId::Function(self.arena.bind_new_id_to(fn_stmt))
                }
                u::Stmt::Block(block) => {
                    let block_id = self.type_block(let_head, block)?;
                    t::StmtId::Block(block_id)
                }
                u::Stmt::Expr { expr, span: _ } => {
                    let expr_typed = self.type_expr(let_head, expr)?;
                    t::StmtId::Expr(expr_typed)
                }
                u::Stmt::Empty { span } => {
                    t::StmtId::Empty(self.arena.bind_new_id_to(t::EmptyStmt { span: *span }))
                }
            };
            typed_stmts.push(typed_stmt);
        }
        let last_stmt = typed_stmts.last().unwrap();
        let ty = match last_stmt.de(&self.arena) {
            t::Stmt::Expr(e) => e.de(&self.arena).ty(&self.arena),
            _ => t::Type::unit().en(self),
        };
        Ok(self.arena.bind_new_id_to(t::Block {
            stmts: typed_stmts,
            span: block.span,
            ty: ty,
        }))
    }

    /// Type THE expression, not type expression.
    /// Does not do the HM inference
    pub fn type_expr(&mut self, binding_head: BindingId, expr: &u::Expr) -> TypeRes<t::ExprId> {
        match expr {
            u::Expr::Unit { span } => {
                let e = t::TupleExpr {
                    elems: vec![],
                    span: *span,
                    ty: t::TupleType { elems: vec![] }.to_type().en(self),
                };
                let id = self.arena.bind_new_id_to(e);
                Ok(t::ExprId::Tuple(id))
            }
            u::Expr::Int { value, span } => {
                let e = t::LiteralExpr {
                    value: t::Literal::Int(*value),
                    span: *span,
                    ty: t::PrimitiveType::Int.to_type().en(self),
                };
                let id = self.arena.bind_new_id_to(e);
                Ok(t::ExprId::Literal(id))
            }
            u::Expr::Bool { value, span } => {
                let e = t::LiteralExpr {
                    value: t::Literal::Bool(*value),
                    span: *span,
                    ty: t::PrimitiveType::Int.to_type().en(self),
                };
                let id = self.arena.bind_new_id_to(e);
                Ok(t::ExprId::Literal(id))
            }
            u::Expr::Var { name, span } => {
                let name = name.clone().intern();
                if let Some(binding) = self.arena.lookup_name(name, binding_head) {
                    if let Some(ty) = self.get_binding_type(binding) {
                        let v = t::VariableExpr {
                            target: binding,
                            name: name,
                            ty,
                            span: *span,
                        };
                        return Ok(t::ExprId::Variable(self.arena.bind_new_id_to(v)));
                    }
                }
                Err(TypeError::UnboundVar {
                    name: name,
                    span: (*span).into(),
                })
            }
            u::Expr::UnaryOp {
                op,
                arg,
                span,
                op_span,
            } => self.type_expr(
                binding_head,
                &u::Expr::App {
                    func: Box::new(u::Expr::Var {
                        name: op.name(),
                        span: *op_span,
                    }),
                    arg: arg.clone(),
                    span: *span,
                },
            ),
            u::Expr::BinaryOp {
                op,
                lhs,
                rhs,
                span,
                op_span,
            } => self.type_expr(
                binding_head,
                &u::Expr::App {
                    func: Box::new(u::Expr::Var {
                        name: op.name(),
                        span: *op_span,
                    }),
                    arg: Box::new(u::Expr::Tuple {
                        elems: vec![*lhs.clone(), *rhs.clone()],
                        span: *span,
                    }),
                    span: *span,
                },
            ),
            u::Expr::App { func, arg, span } => {
                let func_typed = self.type_expr(binding_head, func)?;
                let arg_typed = self.type_expr(binding_head, arg)?;
                let rhs_type = self.bump_fresh_type_var();
                let func_should_be_ty = t::FunctionType {
                    lhs: self.get_expr_type(arg_typed),
                    rhs: rhs_type,
                };
                let c = Constraint {
                    lhs: self.get_expr_type(func_typed),
                    rhs: func_should_be_ty.to_type().en(self),
                };
                let cid = self.arena.bind_new_id_to(c);
                let e = t::ApplicationExpr {
                    callee: func_typed,
                    arg: arg_typed,
                    ty: rhs_type,
                    constraint: cid,
                    span: *span,
                };
                let id = self.arena.bind_new_id_to(e);
                Ok(t::ExprId::Application(id))
            }
            u::Expr::Tuple { elems, span } => {
                let mut typed_elems = Vec::new();
                let mut elem_types = Vec::new();
                for elem in elems {
                    let elem_typed = self.type_expr(binding_head, elem)?;
                    typed_elems.push(elem_typed);
                    elem_types.push(self.get_expr_type(elem_typed));
                }
                let e = t::TupleExpr {
                    elems: typed_elems,
                    span: *span,
                    ty: t::TupleType { elems: elem_types }.to_type().en(self),
                };
                let id = self.arena.bind_new_id_to(e);
                Ok(t::ExprId::Tuple(id))
            }
            u::Expr::Block(block) => {
                let block_id = self.type_block(binding_head, block)?;
                let block_expr = t::BlockExpr { block: block_id };
                let id = self.arena.bind_new_id_to(block_expr);
                Ok(t::ExprId::Block(id))
            }
        }
    }

    pub fn solve_constraints(&mut self) -> TypeRes<Solution> {
        let mut constraints: VecDeque<Constraint> = VecDeque::new();
        for (_, c) in self.arena.idmap_constraint.iter() {
            constraints.push_back(c.clone());
        }
        unify(&mut constraints, self)
    }

    pub fn to_solved_type(&self, ty: t::TypeId, sln: &Solution) -> t::Type {
        let ty = ty.de(self);
        match ty {
            t::Type::Variable(t::VariableType { index: var }) => match sln.slns.get(&var) {
                Some(tid) => tid.de(self),
                None => panic!("Type variable {} not found in solution", var),
            },
            a => a,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, )]
enum Tag {
    PrimitiveInt,
    PrimitiveBool,
    Function,
    Tuple,
}

impl Into<usize> for Tag {
    fn into(self) -> usize {
        match self {
            Tag::PrimitiveInt => 0,
            Tag::PrimitiveBool => 1,
            Tag::Function => 2,
            Tag::Tuple => 3,
        }
    }
}

impl TryFrom<usize> for Tag {
    type Error = ();
    fn try_from(value: usize) -> Result<Self, ()> {
        match value {
            0 => Ok(Tag::PrimitiveInt),
            1 => Ok(Tag::PrimitiveBool),
            2 => Ok(Tag::Function),
            3 => Ok(Tag::Tuple),
            _ => Err(()),
        }
    }
}

impl TypeIdMapper for Context {
    fn id_to_ctype(&self, id: t::TypeId) -> ConType {
        let ty = id.de(self);
        match ty {
            t::Type::Primitive(primitive_type) => {
                let leaf_kind = match primitive_type {
                    t::PrimitiveType::Int => Tag::PrimitiveInt,
                    t::PrimitiveType::Bool => Tag::PrimitiveBool,
                };
                let tag = leaf_kind.into();
                ConType::Leaf { tag: tag }
            }
            t::Type::Function(FunctionType { lhs, rhs }) => ConType::NonLeaf {
                tag: Tag::Function.into(),
                children: vec![lhs, rhs],
            },
            t::Type::Tuple(tuple_type) => {
                let elems = tuple_type.elems.into_iter().map(|e| e).collect();
                ConType::NonLeaf {
                    tag: Tag::Tuple.into(),
                    children: elems,
                }
            }
            t::Type::Variable(t::VariableType { index }) => ConType::TypeVar(index),
        }
    }

    fn ctype_to_id(&mut self, ty: ConType) -> t::TypeId {
        match ty {
            ConType::Leaf { tag } => {
                let tag_kind = Tag::try_from(tag).unwrap();
                match tag_kind {
                    Tag::PrimitiveInt => t::PrimitiveType::Int.to_type().en(self),
                    Tag::PrimitiveBool => t::PrimitiveType::Bool.to_type().en(self),
                    _ => panic!("Tag kind should be leaf, got {:?}", tag_kind),
                }
            }
            ConType::NonLeaf { tag, children } => {
                let tag_kind = Tag::try_from(tag).unwrap();
                match tag_kind {
                    Tag::Function => {
                        if children.len() != 2 {
                            panic!("Function must have 2 children types, got {}.", children.len());
                        }
                        let lhs = children[0];
                        let rhs = children[1];
                        t::FunctionType { lhs, rhs }.to_type().en(self)
                    }
                    Tag::Tuple => t::TupleType { elems: children }.to_type().en(self),
                    _ => panic!("Tag kind should be non-leaf, got {:?}", tag_kind),
                }
            }
            ConType::TypeVar(index) => t::VariableType { index }.to_type().en(self),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::typed::ExprId;
    use crate::diag::print_error;
    use crate::{ast::typed::Binding, parser, sema::typing::Context};
    #[test]
    fn simple_test() {
        let code = "{ let x = 42; fn foo(x: int) -> int { x }; foo(x) }";
        let e = parser::Parser::new(code).expr().unwrap();
        let mut c = Context::new();
        let h = c.arena.bind_new_id_to(Binding {
            parent: None,
            data: crate::ast::typed::BindingData::Empty,
        });
        let te = c
            .type_expr(h, &e)
            .map_err(|e| print_error(e, code, &mut c.type_interner));
        println!("Typed expr: {:?}", te);
        let te = te.unwrap();
        let ty = te.de(&c.arena).ty(&c.arena);
        let sln = c
            .solve_constraints()
            .map_err(|e| print_error(e, code, &mut c.type_interner));
        println!("Sln: {:?}", sln);
        let sln = sln.unwrap();
        let actual_ty = c.to_solved_type(ty, &sln);
        println!("Type: {:?}", actual_ty);
    }
}
