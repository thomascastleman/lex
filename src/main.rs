use regex::Regex;
mod lex;
use crate::lex::Lexer;

// a possible implementation of a token type
#[derive(Debug)]
pub struct Tok<'a> {
    name: &'a str,
    lexeme: &'a str,
}

impl<'a> Tok<'a> {
    pub fn new(name: &'a str, lexeme: &'a str) -> Tok<'a> {
        Tok {
            name: name,
            lexeme: lexeme,
        }
    }
}

fn main() {
    let num_re = Regex::new(r#"\d+"#).unwrap();
    let id_re = Regex::new(r#"[a-zA-Z][a-zA-Z0-9_]+"#).unwrap();
    let ws_re = Regex::new(r#"\s+"#).unwrap();

    let mut l = Lexer::new();

    l.add_pattern(&num_re, |s| ("num", s)); //Tok::new("number", s));
    l.add_pattern(&id_re, |s| ("name", s)); //Tok::new("name", s));
    l.add_pattern(&ws_re, |s| ("ws", s)); //Tok::new("whitespace", s));

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
