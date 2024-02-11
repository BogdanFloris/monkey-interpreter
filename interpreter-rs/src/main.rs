use repl::start;

pub mod evaluator;
pub mod lexer;
pub mod parser;
pub mod repl;

fn main() {
    let mut reader = std::io::stdin().lock();
    let mut writer = std::io::stdout();
    start(&mut reader, &mut writer);
}
