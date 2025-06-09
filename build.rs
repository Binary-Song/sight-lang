use dough_utils::syntax::syntax_to_file;
use std::path::Path;
fn main() {
    let out_dir = std::env::var("OUT_DIR").expect("OUT_DIR not set");
    let rule_path = format!("{}/syntax.lalrpop", out_dir);
    syntax_to_file(&rule_path);
    lalrpop::Configuration::new()
        .process_file(rule_path)
        .unwrap();
}
