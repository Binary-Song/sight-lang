use crate::ast::*;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::parser::Context;
use crate::parser::Parser;
use crate::span::Span;
use function_name::named;
use std::collections::VecDeque;
use std::vec;

pub fn to_term(e: Expr, mut ctx: Context) -> Term {
    match e {
        Expr::Unit { span } => Term::Lit {
            value: Lit::Unit,
            span,
        },
        Expr::Int { value, span } => Term::Lit {
            value: Lit::Int(value),
            span,
        },
        Expr::Bool { value, span } => Term::Lit {
            value: Lit::Bool(value),
            span,
        },
        Expr::Var { name, span } => Term::Var { name, span },
        Expr::UnaryOp {
            op,
            arg,
            span,
            op_span,
        } => Term::App {
            callee: Box::new(Term::Op {
                op: Op::UnaryOp(op),
                span: op_span,
            }),
            arg: Box::new(to_term(*arg, ctx)),
            span: span,
        },
        Expr::BinaryOp {
            op,
            lhs: arg1,
            rhs: arg2,
            span,
            op_span,
        } => Term::App {
            callee: Box::new(Term::Op {
                op: Op::BinaryOp(op),
                span: op_span,
            }),
            arg: Box::new(Term::Tuple {
                elems: vec![to_term(*arg1, ctx.clone()), to_term(*arg2, ctx)],
                span: span.clone(),
            }),
            span: span.clone(),
        },
        Expr::Seq {
            seq,
            span: span_seq,
        } => {
            let mut res_seq: Vec<Term> = vec![];
            // copy the seq to a deque
            let mut seq = VecDeque::from(seq);
            while let Some(expr) = seq.pop_front() {
                if let Expr::Let {
                    name,
                    ty,
                    rhs: init,
                    span: span_let,
                } = expr
                {
                    // init-expr CANNOT use the 'let' variable
                    let init = to_term(*init, ctx.clone());
                    // body CAN use the 'let' variable
                    ctx.bindings.push(Binding {
                        name: name.clone(),
                        ty: ty.clone(),
                        span: span_let,
                    });
                    // body is what is left of the sequence
                    let body = to_term(
                        Expr::Seq {
                            seq: Vec::from(seq),
                            span: (span_let.1, span_seq.1),
                        },
                        ctx,
                    );
                    res_seq.push(Term::Let {
                        name: name,
                        ty: ty,
                        rhs: Box::new(init),
                        body: Box::new(body),
                        span: span_let.clone(),
                    });
                    // no need to continue, we already desugared the rest of the sequence as the body
                    return Term::Seq {
                        seq: Vec::from(res_seq),
                        span: span_seq,
                    };
                } else {
                    res_seq.push(to_term(expr, ctx.clone()));
                }
            }
            return Term::Seq {
                seq: Vec::from(res_seq),
                span: span_seq,
            };
        }
        Expr::App { func, arg, span } => Term::App {
            callee: Box::new(to_term(*func, ctx.clone())),
            arg: Box::new(to_term(*arg, ctx.clone())),
            span,
        },
        Expr::Func {
            param_pattern,
            ret_ty,
            body,
            span,
        } => {
            todo!()
            // let mut ctx = ctx.clone();
            // // add param bindings to context
            // ctx.bindings.push(Binding {
            //     name: param.name.clone(),
            //     ty: param.ty.clone(),
            //     span: param.span,
            // });
            // Term::Func {
            //     param: Box::new(param),
            //     ret_ty,
            //     body: Box::new(to_term(*body, ctx)),
            //     span,
            // }
        }
        // Let Expr can never be here. All Lets' should have been desugared by the let-lifting rule. If you see this, that probably means this Let did not show up as a left child of a Binary-Seq. A parser bug maybe?
        Expr::Let { .. } => panic!("Let Expr can never be here"),
        Expr::Tuple { elems, span } => {
            let elems = elems.into_iter().map(|e| to_term(e, ctx.clone())).collect();
            Term::Tuple { elems, span }
        }
    }
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
    // use pretty_assertions::{assert_eq, assert_ne};
    // let expected = format_xml(expected).unwrap();
    // let parser = Parser::new(src);
    // let r = parser.parse(src).unwrap();
    // let ctx = TreeContext {
    //     src: src.to_string(),
    //     version: 1,
    // };
    // let actual = r.into_tree(&ctx).to_xml(&ctx);
    // let actual = format_xml(actual.as_str()).unwrap();
    // assert_eq!(actual, expected);
    todo!()
}

#[test]
fn app_test1() {
    test_parse(
        "{foo bar baz}",
        r###"
  <TmSeq source="foo bar baz">
  <seq>
    <Item index="0">
      <TmApp source="foo bar baz">
        <callee>
          <TmVar source="foo">
            <name>foo</name>
          </TmVar>
        </callee>
        <args>
          <Item index="0">
            <TmApp source="bar baz">
              <callee>
                <TmVar source="bar">
                  <name>bar</name>
                </TmVar>
              </callee>
              <args>
                <Item index="0">
                  <TmVar source="baz">
                    <name>baz</name>
                  </TmVar>
                </Item>
              </args>
            </TmApp>
          </Item>
        </args>
      </TmApp>
    </Item>
  </seq>
</TmSeq>"###,
    );
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
