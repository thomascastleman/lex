use regex::Regex;
mod lex;
use crate::lex::{Lexer, Token};

fn main() {

    let num_re = Regex::new(r#"\d+"#).unwrap();
    let id_re = Regex::new(r#"[a-zA-Z][a-zA-Z0-9_]+"#).unwrap();
    let ws_re = Regex::new(r#"\s+"#).unwrap();

    let mut l = Lexer::new();

    l.add_rule(&num_re, |s| Token::new("number", s));
    l.add_rule(&id_re, |s| Token::new("name", s));
    l.add_rule(&ws_re, |s| Token::new("whitespace", s));

    let tokens = l.lex("hello 10");

    // for tok in tokens {
    //     println!("{:?}", tok);
    // }
}
