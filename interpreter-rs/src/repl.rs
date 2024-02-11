use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{BufRead, Write};

const PROMPT: &str = ">> ";

pub fn start<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    loop {
        write!(writer, "{PROMPT}").unwrap();
        writer.flush().unwrap();
        let mut line = String::new();
        let _ = reader.read_line(&mut line);
        if line.is_empty() {
            break;
        }
        let lexer = Lexer::new(line);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        if !parser.errors.is_empty() {
            writeln!(writer, "Woops! We ran into some monkey business here!").unwrap();
            writeln!(writer, " parser has {} errors", parser.errors.len()).unwrap();
            for error in parser.errors {
                writeln!(writer, "\t{error}").unwrap();
            }
            continue;
        }
        for stmt in program {
            writeln!(writer, "{stmt}").unwrap();
        }
    }
}
