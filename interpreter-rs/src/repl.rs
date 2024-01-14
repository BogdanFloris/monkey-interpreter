use std::io::{BufRead, Write};

use crate::{lexer::Lexer, token::Token};

const PROMPT: &str = ">> ";

pub fn start_repl<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    loop {
        write!(writer, "{PROMPT}").unwrap();
        writer.flush().unwrap();
        let mut line = String::new();
        let _ = reader.read_line(&mut line);
        if line.is_empty() {
            break;
        }
        let mut lexer = Lexer::new(line);
        loop {
            let token = lexer.next_token();
            if token == Token::Eof {
                break;
            }
            writeln!(writer, "{token:?}").unwrap();
        }
    }
}
