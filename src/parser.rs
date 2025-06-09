
lalrpop_mod!(pub syntax);
use lalrpop_util::lalrpop_mod;

pub fn parse(input: &str)  {
    match syntax::ExprParser::new().parse(input) {
        Ok(expr) => {
            println!("Parsed successfully: {:?}", expr);
        }
        Err(err) => {
            println!("Error parsing: {:?}", err);
        }
    }
}


#[test]
fn calculator1() {
    parse("let a = 1; 1; a + 2; 3");
    // syntax::ExprParser::new().parse("fn main(arg: bool) { 1 } (2, 3); { 1 }");
    // print!("{:?}",a.unwrap());
}
