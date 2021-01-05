# lex
A lexer.

### Usage
To create a new lexer, use `Lexer::new()`. 

Then, you can add patterns to the lexer, and tell it how you'd like to construct 
tokens based on those patterns. For instance,
```rust
let l = Lexer::new();

let num_re = Regex::new(r#"\d+"#).unwrap();
let name_re = Regex::new(r#"[a-zA-Z][a-zA-Z0-9_]+"#).unwrap();
let ws_re = Regex::new(r#"\s+"#).unwrap();

l.add_pattern(&num_re, |s| ("number", s));      // highest priority
l.add_pattern(&name_re, |s| ("name", s));
l.add_pattern(&ws_re, |s| ("whitespace", s));   // lowest priority
```

> Note: patterns should be added in order of precedence, with higher priority patterns added first.

Finally, the `lex()` method can be used to tokenize a given string:
```rust
let result = l.lex("test 5").unwrap();

assert_eq!(result, vec![
  ("name", "test"),
  ("whitespace", " "),
  ("number", "5")
]);
```

> Note: `lex()` returns a `Result`, so remember to check for errors.

There are two possible `LexerError`s:
```rust
enum LexerError {
    NoPatterns,
    InvalidToken(usize),
}
```
The `LexerError::NoPatterns` is returned if you attempt to use `lex()` on a lexer that has no patterns added to it.

`LexerError::InvalidToken(index)` is returned if a point in the input is reached at which none of the lexer's patterns match the input. `index` is the index in the input string at which this occurred.