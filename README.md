# lex
A [lexer](https://en.wikipedia.org/wiki/Lexical_analysis).

### Usage
To create a new lexer, use `Lexer::new()`, providing a vector of `Rule`s which indicate what text patterns you want to match and how you want to turn them into tokens. 

 For instance,
```rust
let num_re = Regex::new(r#"\d+"#).unwrap();
let name_re = Regex::new(r#"[a-zA-Z][a-zA-Z0-9_]+"#).unwrap();
let ws_re = Regex::new(r#"\s+"#).unwrap();

let l = Lexer::new(vec![
  Rule::new(&num_re, |s| ("number", s)),      // highest priority
  Rule::new(&name_re, |s| ("name", s)),
  Rule::new(&ws_re, |s| ("whitespace", s)),   // lowest priority
]);
```

> Note: patterns should be added in order of precedence, with higher priority patterns at the beginning of the rules list.

Now, the `lex()` method can be used to tokenize a given string:
```rust
let stream = l.lex("test 5");

assert_eq!(stream.next(), Some(Ok(("name", "test"))));
assert_eq!(stream.next(), Some(Ok(("whitespace", " "))));
assert_eq!(stream.next(), Some(Ok(("number", "5"))));
assert!(stream.next().is_none());
```

There is currently only one possible `LexerError`, which indicates a token was encountered which did not match any of the rules in the lexer. 
```rust
enum LexerError {
    InvalidToken(usize),
}
```