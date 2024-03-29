use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{BufRead, Write};

const PROMPT: &str = ">> ";

/// Start the REPL
///
/// # Panics
///
/// Panics if the writer is unable to write to the output
pub fn start<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    let mut evaluator = Evaluator::new();
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
        let evaluated = evaluator.eval_program(program);
        writeln!(writer, "{evaluated}").unwrap();
    }
}
