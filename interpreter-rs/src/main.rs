use repl::start;

mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

fn main() {
    let mut reader = std::io::stdin().lock();
    let mut writer = std::io::stdout();
    start(&mut reader, &mut writer);
}
