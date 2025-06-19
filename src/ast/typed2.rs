use std::collections::HashMap;

use crate::{ast::id_map::IdMap, utils::interning::InternString, LiteralValue};
use sight_macros::LiteralValue;

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
struct TreeId(usize);

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
struct DefId(usize);

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
struct TypeId(usize);

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
struct BodyId(usize);

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
struct ConstraintId(usize);

//
// EXPR
//

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Lit {
    Int(i32),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct LitExpr {
    pub value: Lit,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct VarExpr {
    pub target: DefId,
    pub name: InternString,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct AppExpr {
    pub callee: Box<Expr>,
    pub arg: Box<Expr>,
    pub ty: TypeId,
    pub constraint: ConstraintId,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Expr {
    Lit(LitExpr),
    Var(VarExpr),
    App(AppExpr),
    Block(Block),
}

//
// PATTERN
//
#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
enum PatternOwner {
    Let(TreeId),
    Func(TreeId),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct VarPattern {
    pub new_var_id: DefId,
    pub span: (usize, usize),
    pub owner: PatternOwner,
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct TuplePattern {
    pub id: TreeId,
    pub name: InternString,
    pub ty: TypeId,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Pattern {
    Var(VarPattern),
    Tuple(TuplePattern),
}

// 
// STMT
//

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct LetStmt {
    pub lhs: Pattern,
    pub rhs: Box<Expr>,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub struct FuncStmt {
    pub new_fn_id: DefId,
    pub param: Pattern,
    pub ret_ty: TypeId,
    pub body: Block,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, LiteralValue)]
pub enum Stmt {
    Let(LetStmt),
    Func(FuncStmt),
    Block(Block),
    Expr(Expr),
}

//
// Tree and Def
//
pub enum Def {
    FuncStmt { func: TreeId,    },
    VarPattern { var: TreeId },
}

pub enum Tree {
    Expr(Expr),
    Pattern(Pattern),
    Stmt(Stmt),
}

//
// Context
//
pub struct Context {
    def_id_to_def: IdMap<Def>,
    tree_id_to_tree: IdMap<Tree>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            def_id_to_def: IdMap::new(),
            tree_id_to_tree: IdMap::new(),
        }
    }

    pub fn insert_def(&mut self, def: Def) -> DefId {
        DefId(self.def_id_to_def.insert(def))
    }

    pub fn insert_tree(&mut self, tree: Tree) -> TreeId {
        TreeId(self.tree_id_to_tree.insert(tree))
    }

    pub fn get_def(&self, id: DefId) -> Option<&Def> {
        self.def_id_to_def.get(id.0)
    }

    pub fn get_tree(&self, id: TreeId) -> Option<&Tree> {
        self.tree_id_to_tree.get(id.0)
    }
}
