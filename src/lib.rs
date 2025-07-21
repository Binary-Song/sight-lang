pub mod ast;
// pub mod parser;
pub mod literal_value;
pub use literal_value::LiteralValue;
// pub mod lexer;
// pub mod span;
// pub mod sema;
// pub mod backend;
// pub mod diag;
// pub mod context;
pub mod container;
// mod test;

peg::parser!{
grammar list_parser() for str {
    rule number() -> i32
        = n:$(['0'..='9']+) { n.parse().unwrap() }

    pub rule expression(ctx: String) -> i32 = precedence!{
        x:(@) "+" y:@ { x + y }
        --
        "(" e:expression (ctx.clone()) ")" { e }
        n:number() { n }
    }
}
}

pub fn main() {
    let input = "2 + 3 + (4 + 5)";
    match list_parser::expression(input, "".to_string()) {
        Ok(result) => println!("Result: {}", result),
        Err(e) => println!("Parse error: {}", e),
    }
}
