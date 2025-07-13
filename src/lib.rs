pub mod ast;
pub mod parser;
pub mod literal_value;
pub use literal_value::LiteralValue;
pub mod lexer;
pub mod span;
pub mod sema;
pub mod frontend;
pub mod backend;
pub mod utils;
pub mod diag;
pub mod arena;
enum F
{
    A(i32)
}

fn f() -> F
{
    use F::A;
    A(1)
}