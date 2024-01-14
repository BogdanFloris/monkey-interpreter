use repl::start;

mod lexer;
mod repl;
mod token;

fn main() {
    let mut reader = std::io::stdin().lock();
    let mut writer = std::io::stdout();
    start(&mut reader, &mut writer);
}
