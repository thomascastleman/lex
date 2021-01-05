use regex::Regex;
mod lex;
use crate::lex::{Lexer, Token};

fn main() {
    let num_re = Regex::new(r#"\d+"#).unwrap();
    let id_re = Regex::new(r#"[a-zA-Z][a-zA-Z0-9_]+"#).unwrap();
    let ws_re = Regex::new(r#"\s+"#).unwrap();

    let mut l = Lexer::new();

    l.add_pattern(&num_re, |s| Token::new("number", s));
    l.add_pattern(&id_re, |s| Token::new("name", s));
    l.add_pattern(&ws_re, |s| Token::new("whitespace", s));

    let lex_result = l.lex("hello ok 10");

    match lex_result {
        Ok(tokens) => {
            println!("{} tokens.", tokens.len());
            for tok in tokens.iter() {
                println!("{:?}", tok);
            }
        }
        Err(e) => {
            println!("{}", e);
        }
    }
}
