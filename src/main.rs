mod ast;
mod vocab;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub syntax);

fn main() {
    println!("Hello, world!");
}

#[test]
fn calculator1() {
    print!("{:?}",syntax::ExprParser::new().parse("1+2*3"));
}

