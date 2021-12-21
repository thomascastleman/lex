# lex
A [lexer](https://en.wikipedia.org/wiki/Lexical_analysis).

### Example
```rust
let num_re    = Regex::new(r#"\d+"#).unwrap();
let plus_re   = Regex::new(r#"\+"#).unwrap();
let str_re    = Regex::new(r#"".*?""#).unwrap();
let id_re     = Regex::new(r#"[a-zA-Z][a-zA-Z0-9_]*"#).unwrap();
let ws_re     = Regex::new(r#"\s+"#).unwrap();
let lparen_re = Regex::new(r#"\("#).unwrap();
let rparen_re = Regex::new(r#"\)"#).unwrap();

// A type representing all possible tokens we expect in the input stream.
#[derive(Debug, PartialEq, Eq)]
enum Token {
    Num(i32),
    Plus,
    Str(String),
    Id(String),
    Whitespace,
    LParen,
    RParen,
}
use Token::*;

// Construct a `Lexer` by supplying rules which determine how to convert
// text that matches each pattern into the token for that pattern.
let lexer = Lexer::new(vec![
    Rule::new(&num_re,    |s| Num(s.parse::<i32>().unwrap())),
    Rule::new(&plus_re,   |_| Plus),
    Rule::new(&str_re,    |s| Str(s.to_string())),
    Rule::new(&id_re,     |s| Id(s.to_string())),
    Rule::new(&ws_re,     |_| Whitespace),
    Rule::new(&lparen_re, |_| LParen),
    Rule::new(&rparen_re, |_| RParen),
]);

// Lex an input string and collect tokens into a `Vec` (ignoring whitespace).
// Note that in this example, we don't handle possible errors.
let tokens: Vec<_> = lexer
    .lex("(define (f x) (+ x 1))")
    .map(Result::unwrap)
    .filter(|t| *t != Whitespace)
    .collect();

assert_eq!(
    tokens,
    vec![
        LParen,
        Id("define".into()),
        LParen,
        Id("f".into()),
        Id("x".into()),
        RParen,
        LParen,
        Plus,
        Id("x".into()),
        Num(1),
        RParen,
        RParen
    ]
);
```

### Errors
There is currently only one possible `LexerError`, which indicates a token was encountered which did not match any of the rules in the lexer. 
```rust
enum LexerError {
    InvalidToken(usize),
}
```