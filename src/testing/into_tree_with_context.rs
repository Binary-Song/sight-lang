use crate::span::Span;
use crate::testing::tree::*;
use crate::ast::*;
use crate::typing::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TreeContext {
    /// the source code
    pub src: String,
    /// the version number, used to make expected values of unit tests backward compatible
    pub version: i32,
}

pub trait IntoTreeWithContext {
    fn into_tree(self: &Self, ctx: &TreeContext) -> Tree;
}

impl<T: IntoTreeWithContext> IntoTreeWithContext for Box<T> {
    fn into_tree(self: &Box<T>, ctx: &TreeContext) -> Tree {
        self.as_ref().into_tree(ctx)
    }
}

impl<T: IntoTreeWithContext> IntoTreeWithContext for Vec<T> {
    fn into_tree(self: &Self, ctx: &TreeContext) -> Tree {
        let mut array = TreeArray::new();
        for item in self.iter().map(|item| item.into_tree(ctx)) {
            array = array.add_item(item);
        }
        array.into()
    }
}
impl<T: IntoTreeWithContext> IntoTreeWithContext for Option<T> {
    fn into_tree(self: &Self, ctx: &TreeContext) -> Tree {
        match self {
            Some(item) => item.into_tree(ctx),
            None => Tree::None,
        }
    }
}

impl IntoTreeWithContext for Binding {
    fn into_tree(self: &Self, ctx: &TreeContext) -> Tree {
        let span = self.span.clone();
        TreeObject::new()
            .add_field("name", self.name.to_string())
            .add_field("type", self.ty.into_tree(ctx))
            .add_field("source", ctx.src[span.0..span.1].to_string())
            .into()
    }
}

impl IntoTreeWithContext for Lit {
    fn into_tree(self: &Self, ctx: &TreeContext) -> Tree {
        match self {
            Lit::Unit => "LUnit".into(),
            Lit::Int(value) => value.clone().into(),
            Lit::Bool(value) => value.clone().into(),
        }
    }
}

impl IntoTreeWithContext for UnaryOp {
    fn into_tree(self: &Self, ctx: &TreeContext) -> Tree {
        match self {
            UnaryOp::Pos => "Pos".into(),
            UnaryOp::Neg => "Neg".into(),
        }
    }
}

impl IntoTreeWithContext for BinaryOp {
    fn into_tree(self: &Self, ctx: &TreeContext) -> Tree {
        match self {
            BinaryOp::Add => "Add".into(),
            BinaryOp::Mul => "Mul".into(),
            BinaryOp::Seq => "Seq".into(),
        }
    }
}

impl IntoTreeWithContext for Op {
    fn into_tree(self: &Self, ctx: &TreeContext) -> Tree {
        match self {
            Op::UnaryOp(unary_op) => unary_op.into_tree(ctx),
            Op::BinaryOp(binary_op) => binary_op.into_tree(ctx),
        }
    }
}

impl IntoTreeWithContext for PrimitiveType {
    fn into_tree(self: &Self, ctx: &TreeContext) -> Tree {
        match self {
            PrimitiveType::Unit => "PTUnit".into(),
            PrimitiveType::Int => "PTInt".into(),
            PrimitiveType::Bool => "PTBool".into(),
        }
    }
}


trait BasicInfoTree {
    fn basic_info_tree(self: &Self, type_name: &str, ctx: &TreeContext) -> TreeObject;
}

impl<T: Span> BasicInfoTree for T {
    fn basic_info_tree(self: &Self, type_name: &str, ctx: &TreeContext) -> TreeObject {
        let span = self.span();
        TreeObject::new()
            .add_field("type_name", type_name)
            .add_field("source", ctx.src[span.0..span.1].to_string())
            .into()
    }
}

impl IntoTreeWithContext for TypeExpr {
    fn into_tree(self: &Self, ctx: &TreeContext) -> Tree {
        match self {
            TypeExpr::Bool { .. } => self.basic_info_tree("TyBool", ctx).into(),
            TypeExpr::Int { .. } => self.basic_info_tree("TyInt", ctx).into(),
            TypeExpr::Arrow { lhs: l, rhs: r, .. } => self
                .basic_info_tree("TyArrow", ctx)
                .add_field("lhs", l.into_tree(ctx))
                .add_field("rhs", r.into_tree(ctx))
                .into(),
            TypeExpr::Tuple { elems, .. } => self
                .basic_info_tree("TyTuple", ctx)
                .add_field("elems", elems.into_tree(ctx))
                .into(),
        }
    }
}

impl IntoTreeWithContext for Expr {
    fn into_tree(self: &Expr, ctx: &TreeContext) -> Tree {
        match self {
            Expr::Unit { .. } => self.basic_info_tree("EUnitLit", ctx).into(),
            Expr::Int { value, .. } => self
                .basic_info_tree("EIntLit", ctx)
                .add_field("value", *value)
                .into(),
            Expr::Bool { value, .. } => self
                .basic_info_tree("EBoolLit", ctx)
                .add_field("value", *value)
                .into(),
            Expr::Var { name, .. } => self
                .basic_info_tree("EVar", ctx)
                .add_field("name", name.to_string())
                .into(),
            Expr::UnaryOp { op, arg, .. } => self
                .basic_info_tree("EUnaryOp", ctx)
                .add_field("op", op.into_tree(ctx))
                .add_field("arg", arg.into_tree(ctx))
                .into(),
            Expr::BinaryOp { op, lhs, rhs, .. } => self
                .basic_info_tree("EBinaryOp", ctx)
                .add_field("op", op.into_tree(ctx))
                .add_field("lhs", lhs.into_tree(ctx))
                .add_field("rhs", rhs.into_tree(ctx))
                .into(),
            Expr::App { func, arg, .. } => self
                .basic_info_tree("EApp", ctx)
                .add_field("func", func.into_tree(ctx))
                .add_field(
                    if ctx.version == 1 { "args" } else { "arg" },
                    if ctx.version == 1 {
                        vec![arg.clone()].into_tree(ctx)
                    } else {
                        arg.into_tree(ctx)
                    },
                )
                .into(),
            Expr::Func {
                param_pattern,
                ret_ty,
                body,
                ..
            } => todo!(),
            // self
            //     .basic_info_tree("EFunc", ctx)
            //     .add_field("params", param_pattern.into_tree(ctx))
            //     .add_field("ret_ty", ret_ty.into_tree(ctx))
            //     .add_field("body", body.into_tree(ctx))
            //     .into(),
            Expr::Let {
                name,
                ty,
                rhs,
                span,
                ..
            } => self
                .basic_info_tree("ELet", ctx)
                .add_field("name", name.to_string())
                .add_field("ty", ty.into_tree(ctx))
                .add_field("rhs", rhs.into_tree(ctx))
                .into(),
            Expr::Seq { seq, span } => self
                .basic_info_tree("ESeq", ctx)
                .add_field("seq", seq.into_tree(ctx))
                .into(),
            Expr::Tuple { elems, span } => self
                .basic_info_tree("ETuple", ctx)
                .add_field("elems", elems.into_tree(ctx))
                .into(),
        }
    }
}


impl IntoTreeWithContext for Term {
    fn into_tree(self: &Self, ctx: &TreeContext) -> Tree {
        match self {
            Term::Lit { value, .. } => self
                .basic_info_tree("TmLit", ctx)
                .add_field("value", value.into_tree(ctx))
                .into(),
            Term::Var { name, .. } => self
                .basic_info_tree("TmVar", ctx)
                .add_field("name", name.to_string())
                .into(),
            Term::App { callee, arg, .. } => self
                .basic_info_tree("TmApp", ctx)
                .add_field("callee", callee.into_tree(ctx))
                .add_field(
                    if ctx.version == 1 { "args" } else { "arg" },
                    if ctx.version == 1 {
                        vec![arg.clone()].into_tree(ctx)
                    } else {
                        arg.into_tree(ctx)
                    },
                )
                .into(),
            Term::Func {
                param,
                ret_ty,
                body,
                ..
            } => self
                .basic_info_tree("TmFunc", ctx)
                .add_field("params", param.into_tree(ctx))
                .add_field("ret_ty", ret_ty.into_tree(ctx))
                .add_field("body", body.into_tree(ctx))
                .into(),
            Term::Let {
                name,
                ty,
                rhs,
                body,
                ..
            } => self
                .basic_info_tree("TmLet", ctx)
                .add_field("name", name.to_string())
                .add_field("ty", ty.into_tree(ctx))
                .add_field("rhs", rhs.into_tree(ctx))
                .add_field("body", body.into_tree(ctx))
                .into(),
            Term::Seq { seq, .. } => self
                .basic_info_tree("TmSeq", ctx)
                .add_field("seq", seq.into_tree(ctx))
                .into(),
            Term::Op { op, .. } => self
                .basic_info_tree("TmOp", ctx)
                .add_field("op", op.into_tree(ctx))
                .into(),
            Term::Tuple { elems, .. } => self
                .basic_info_tree("TmTuple", ctx)
                .add_field("elems", elems.into_tree(ctx))
                .into(),
        }
    }
}

impl IntoTreeWithContext for Type {
    fn into_tree(self: &Self, ctx: &TreeContext) -> Tree {
        match self {
            Type::Primitive(primitive_type) => TreeObject::new_basic("TPrimitive", ctx)
                .add_field("inner", primitive_type.into_tree(ctx))
                .into(),
            Type::Func { lhs, rhs } => TreeObject::new_basic("TFunc", ctx)
                .add_field("lhs", lhs.into_tree(ctx))
                .add_field("rhs", rhs.into_tree(ctx))
                .into(),
            Type::Var { name } => TreeObject::new_basic("TVar", ctx)
                .add_field("name", name.to_string())
                .into(),
            Type::Tuple { elems } => TreeObject::new_basic("TTuple", ctx)
                .add_field("elems", elems.into_tree(ctx))
                .into(),
        }
    }
}

impl Tree {
    fn get_tag_attr_and_content(obj: &TreeObject, ctx: &TreeContext) -> (String, String, String) {
        let mut tag = None;
        let mut attr = None;
        let ctt = obj
            .iter()
            .filter(|(k, v)| {
                if *k == "type_name" {
                    match v.clone() {
                        Tree::String(type_name) => tag = Some(type_name.clone()),
                        _ => (),
                    }
                    false
                } else if *k == "source" {
                    match v.clone() {
                        Tree::String(source) => {
                            attr = Some(format!(" source=\"{}\"", source));
                            false
                        }
                        _ => false,
                    }
                } else {
                    true
                }
            })
            .fold("".to_string(), |sum, (k, v)| {
                format!("{}<{}>{}</{}>", sum, k, v.to_xml(ctx), k)
            });
        let tag = tag.unwrap_or("Object".to_string());
        let attr = attr.unwrap_or("".to_string());
        (tag, attr, ctt)
    }

    pub fn to_xml(self: &Self, ctx: &TreeContext) -> String {
        match self {
            Tree::None => "<None/>".to_string(),
            Tree::Bool(value) => format!("{}", value),
            Tree::Int(value) => format!("{}", value),
            Tree::String(value) => format!("{}", value),
            Tree::Object(object) => {
                let (tag, attr, con) = Tree::get_tag_attr_and_content(object, ctx);
                format!("<{tag}{attr}>{con}</{tag}>",)
            }
            Tree::Array(array) => {
                let mut con = String::new();
                for (idx, item) in array.iter().enumerate() {
                    con += format!("<Item index=\"{}\">{}</Item>", idx, item.to_xml(ctx)).as_str();
                }
                format!("{con}")
            }
        }
    }
}