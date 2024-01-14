use repl::start_repl;

mod lexer;
mod repl;
mod token;

fn main() {
    let mut reader = std::io::stdin().lock();
    let mut writer = std::io::stdout();
    start_repl(&mut reader, &mut writer);
}
