use crate::{
    container::{Container, Interner},
    sum_container, parser,
};
use std::{cell::RefCell, rc::Rc};

#[test]
fn parse_simple_expr() {
    let container = Interner::<String>::default();
    let container = Rc::new(RefCell::new(container));
    let e = parser::sight::expr("{ let a  = b + 1 * 2; a 1 2; 3 }",container.clone()).unwrap();
    println!("expr = {:#?}", e);
    println!("container = {:#?}", container);
}
