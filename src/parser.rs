lalrpop_mod!(pub syntax, "/syntax.rs");
use crate::ast::*;
use crate::compat_serialize::*;
pub use lalrpop_util::lalrpop_mod;
use std::collections::VecDeque;
use std::vec;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Context {
    pub bindings: Vec<Binding>,
}

fn to_term(e: Expr, mut ctx: Context) -> Term {
    match e {
        Expr::UnitLit { span } => Term::Lit { value: Lit::Unit, span },
        Expr::IntLit { value, span } => Term::Lit { value: Lit::Int(value), span },
        Expr::BoolLit { value, span } => Term::Lit { value: Lit::Bool(value), span },
        Expr::Var { name, span } =>  {
            Term::Var { name, span }
        }
        Expr::UnaryOp { op, arg, span, op_span } => Term::App {
            callee: Box::new(Term::Op { op: Op::UnaryOp(op), span: op_span }), args: vec![to_term(*arg, ctx)], span: span
        },
        Expr::BinaryOp {
            op,
            lhs: arg1,
            rhs: arg2,
            span,
            op_span,
        } => Term::App {
            callee:Box::new(Term::Op { op: Op::BinaryOp(op), span: op_span }), args: vec![to_term(*arg1, ctx.clone()), to_term(*arg2, ctx)], span: span
        },
        Expr::Seq{seq, span: span_seq} => {
            let mut res_seq: Vec<Term> = vec![];
            // copy the seq to a deque
            let mut seq = VecDeque::from(seq);
            while let Some(expr) = seq.pop_front() {
                if let Expr::Let { name, ty, rhs: init, span: span_let } = expr {
                    // init-expr CANNOT use the 'let' variable
                    let init = to_term(*init, ctx.clone());
                    // body CAN use the 'let' variable
                    ctx.bindings.push(Binding{name: name.clone(), ty: ty.clone(), span: span_let});
                    // body is what is left of the sequence
                    let body = to_term(Expr::Seq { seq: Vec::from(seq), span: (span_let.1, span_seq.1) }, ctx);
                    res_seq.push(Term::Let { name: name, ty: ty, rhs: Box::new(init), body: Box::new(body), span: span_let.clone() });
                    // no need to continue, we already desugared the rest of the sequence as the body
                    return Term::Seq { seq: Vec::from(res_seq) , span: span_seq} ;
                } else {
                    res_seq.push(to_term(expr, ctx.clone()));
                }
            };    
            return Term::Seq { seq: Vec::from(res_seq) , span: span_seq} ;
        },
        Expr::App { func, args, span } => Term::App {
            callee:  (Box::new(to_term(*func, ctx.clone()))),
            args: {
                let mut result = Vec::new();
                for arg in args {
                    let desugd = to_term(arg, ctx.clone());
                    result.push(desugd);
                }
                result
            },
            span,
        } ,
        Expr::Func {
            params,
            ret_ty,
            body,
            span,
        } => {
            let mut ctx = ctx.clone();
            // add param bindings to context
            for p in params.iter() {
                ctx.bindings.push(Binding {
                        name: p.name.clone(),
                        ty: p.ty.clone(),
                        span: p.span,
                });
            }
            Term::Func {
            params,
            ret_ty,
            body: Box::new(to_term(*body, ctx)),
            span,
        }},
        Expr::Let {..} => panic!("Let Expr can never be here. All Lets' should have been desugared by the let-lifting rule.
        If you see this, that probably means this Let did not show up as a left child of a Binary-Seq.
        A parser bug maybe?
        "),
    }
}

type ParserError<'a> =
    lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'a>, &'static str>;

pub fn parse(input: &str) -> Result<Term, ParserError> {
    let e = syntax::ExprParser::new().parse(input)?;
    let r = to_term(e, Context { bindings: vec![] });
    Ok(r)
}

#[cfg(test)]
fn format_xml(src: &str) -> Result<String, xml::reader::Error> {
    use xml::{reader::ParserConfig, writer::EmitterConfig};
    let mut dest = Vec::new();
    let reader = ParserConfig::new()
        .trim_whitespace(true)
        .ignore_comments(false)
        .create_reader(src.as_bytes());
    let mut writer = EmitterConfig::new()
        .perform_indent(true)
        .normalize_empty_elements(false)
        .autopad_comments(false)
        .create_writer(&mut dest);
    for event in reader {
        if let Some(event) = event?.as_writer_event() {
            writer.write(event).unwrap();
        }
    }
    Ok(String::from_utf8(dest).unwrap())
}

#[cfg(test)]
fn test_parse(src: &str, expected: &str) {
    use pretty_assertions::{assert_eq, assert_ne};
    let expected = format_xml(expected).unwrap();
    let r = parse(src).unwrap();
    let ctx = TreeContext {
        src: src.to_string(),
        version: 1,
    };
    let actual = r.into_tree(&ctx).to_xml(&ctx);
    let actual = format_xml(actual.as_str()).unwrap();
    assert_eq!(actual, expected);
}

#[test]
fn source_to_term_test() {
    test_parse(
        "{let a = { 1 + 2; let b = a + 3; };4}",
        r##"<TmSeq source="let a = { 1 + 2; let b = a + 3; };4">
  <seq>
    <Item index="0">
      <TmLet source="let a = { 1 + 2; let b = a + 3; };">
        <name>a</name>
        <ty>
          <None />
        </ty>
        <rhs>
          <TmSeq source="1 + 2; let b = a + 3;">
            <seq>
              <Item index="0">
                <TmApp source="1 + 2">
                  <callee>
                    <TmOp source="+">
                      <op>Add</op>
                    </TmOp>
                  </callee>
                  <args>
                    <Item index="0">
                      <TmLit source="1">
                        <value>1</value>
                      </TmLit>
                    </Item>
                    <Item index="1">
                      <TmLit source="2">
                        <value>2</value>
                      </TmLit>
                    </Item>
                  </args>
                </TmApp>
              </Item>
              <Item index="1">
                <TmLet source="let b = a + 3;">
                  <name>b</name>
                  <ty>
                    <None />
                  </ty>
                  <rhs>
                    <TmApp source="a + 3">
                      <callee>
                        <TmOp source="+">
                          <op>Add</op>
                        </TmOp>
                      </callee>
                      <args>
                        <Item index="0">
                          <TmVar source="a">
                            <name>a</name>
                          </TmVar>
                        </Item>
                        <Item index="1">
                          <TmLit source="3">
                            <value>3</value>
                          </TmLit>
                        </Item>
                      </args>
                    </TmApp>
                  </rhs>
                  <body>
                    <TmSeq source="">
                      <seq>
                        <Item index="0">
                          <TmLit source="">
                            <value>LUnit</value>
                          </TmLit>
                        </Item>
                      </seq>
                    </TmSeq>
                  </body>
                </TmLet>
              </Item>
            </seq>
          </TmSeq>
        </rhs>
        <body>
          <TmSeq source="4">
            <seq>
              <Item index="0">
                <TmLit source="4">
                  <value>4</value>
                </TmLit>
              </Item>
            </seq>
          </TmSeq>
        </body>
      </TmLet>
    </Item>
  </seq>
</TmSeq>"##,
    )
}
