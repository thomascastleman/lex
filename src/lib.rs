//! This crate provides a simple implementation of a [lexer].
//!
//! # Example
//! ```
//! # use lex::{Rule, Lexer};
//! # use regex::Regex;
//! let num_re    = Regex::new(r#"\d+"#).unwrap();
//! let plus_re   = Regex::new(r#"\+"#).unwrap();
//! let str_re    = Regex::new(r#"".*?""#).unwrap();
//! let id_re     = Regex::new(r#"[a-zA-Z][a-zA-Z0-9_]*"#).unwrap();
//! let ws_re     = Regex::new(r#"\s+"#).unwrap();
//! let lparen_re = Regex::new(r#"\("#).unwrap();
//! let rparen_re = Regex::new(r#"\)"#).unwrap();
//!
//! // A type representing all possible tokens we expect in the input stream.
//! #[derive(Debug, PartialEq, Eq)]
//! enum Token {
//!     Num(i32),
//!     Plus,
//!     Str(String),
//!     Id(String),
//!     Whitespace,
//!     LParen,
//!     RParen,
//! }
//! use Token::*;
//!
//! // Construct a `Lexer` by supplying rules which determine how to convert
//! // text that matches each pattern into the token for that pattern.
//! let lexer = Lexer::new(vec![
//!     Rule::new(&num_re,    |s| Num(s.parse::<i32>().unwrap())),
//!     Rule::new(&plus_re,   |_| Plus),
//!     Rule::new(&str_re,    |s| Str(s.to_string())),
//!     Rule::new(&id_re,     |s| Id(s.to_string())),
//!     Rule::new(&ws_re,     |_| Whitespace),
//!     Rule::new(&lparen_re, |_| LParen),
//!     Rule::new(&rparen_re, |_| RParen),
//! ]);
//!
//! // Lex an input string and collect tokens into a `Vec` (ignoring whitespace).
//! // Note that in this example, we don't handle possible errors.
//! let tokens: Vec<_> = lexer
//!     .lex("(define (f x) (+ x 1))")
//!     .map(Result::unwrap)
//!     .filter(|t| *t != Whitespace)
//!     .collect();
//!
//! assert_eq!(
//!     tokens,
//!     vec![
//!         LParen,
//!         Id("define".into()),
//!         LParen,
//!         Id("f".into()),
//!         Id("x".into()),
//!         RParen,
//!         LParen,
//!         Plus,
//!         Id("x".into()),
//!         Num(1),
//!         RParen,
//!         RParen
//!     ]
//! );
//! ```
//!
//! [lexer]: https://en.wikipedia.org/wiki/Lexical_analysis

use regex::Regex;

/// Error type for lexer errors.
///
/// `InvalidToken(idx)` means a token that doesn't match any pattern
/// was found at index `idx` in the input string.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LexerError {
    InvalidToken(usize),
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexerError::InvalidToken(idx) => {
                write!(f, "invalid token at index {}", idx)
            }
        }
    }
}

/// A [`Rule`] consists of a regular expression for matching a pattern for a
/// given token and a function (action) capable of producing that token given the
/// lexeme that matched.
pub struct Rule<'a, Token> {
    pattern: &'a Regex,
    action: Box<dyn Fn(&'a str) -> Token + 'a>,
}

impl<'a, Token> Rule<'a, Token> {
    /// Construct a new [`Rule`] for use in a [`Lexer`].
    ///
    /// # Example
    /// ```
    /// # use regex::Regex;
    /// # use lex::Rule;
    /// Rule::new(&Regex::new(r#"\d+"#).unwrap(), |n| ("number", n));
    /// ```
    /// If the input was `"50"`, for instance, this rule will match
    /// and produce a token `("number", "50")`.
    pub fn new<Action>(pattern: &'a Regex, action: Action) -> Self
    where
        Action: Fn(&'a str) -> Token + 'a,
    {
        Rule {
            pattern,
            action: Box::new(action),
        }
    }
}

/// A [`Lexer`] uses rules to break up a given string into tokens. The exact
/// type used as `Token` is up to the user.
pub struct Lexer<'a, Token> {
    rules: Vec<Rule<'a, Token>>,
}

impl<'a, Token> Lexer<'a, Token> {
    /// Constructs a new [`Lexer`] instance from a given sequence of rules.
    ///
    /// # Example
    /// ```
    /// # use lex::{Lexer, Rule};
    /// # use regex::Regex;
    /// Lexer::new(vec![
    ///     Rule::new(&Regex::new(r#"\d+"#).unwrap(), |n| ("number", n)),
    ///     Rule::new(&Regex::new(r#"\s+"#).unwrap(), |ws| ("whitespace", ws)),
    /// ]);
    /// ```
    ///
    /// # Panics
    /// This function panics if an empty vector of rules is supplied, as the [`Lexer`]
    /// needs at least one rule to operate.
    pub fn new(rules: Vec<Rule<'a, Token>>) -> Lexer<'a, Token> {
        assert!(!rules.is_empty(), "lexer must have at least one rule");
        Lexer { rules }
    }

    /// Constructs a lexed token stream from a given text, using this lexer.
    ///
    /// [`TokenStream`] implements [`Iterator`] over `Token`s, so you can process
    /// the stream as an iterator to start getting tokens. The iterator yields
    /// `Result<Token, LexerError>` items, since the lexer may run into invalid
    /// text which doesn't match any rule in its list.
    ///
    /// # Example
    /// ```
    /// # use lex::{Lexer, Rule};
    /// # use regex::Regex;
    /// let num_re =  Regex::new(r#"\d+"#).unwrap();
    /// let ws_re = Regex::new(r#"\s+"#).unwrap();
    ///
    /// let lx = Lexer::new(vec![
    ///     Rule::new(&num_re, |n| ("number", n)),
    ///     Rule::new(&ws_re, |ws| ("whitespace", ws)),
    /// ]);
    ///
    /// // Note: We `unwrap` each token here because we know lexing will succeed
    /// // on the string "1 2 3". In practice, it might fail, producing `Err()`.
    /// let tokens = lx.lex("1 2 3").map(|r| r.unwrap()).collect::<Vec<_>>();
    /// assert_eq!(tokens, vec![
    ///     ("number", "1"),
    ///     ("whitespace", " "),
    ///     ("number", "2"),
    ///     ("whitespace", " "),
    ///     ("number", "3"),
    /// ]);
    /// ```
    pub fn lex<'lex>(
        &'lex self,
        text: &'a str,
    ) -> TokenStream<'lex, 'a, Token> {
        TokenStream {
            lexer: self,
            current_index: 0,
            rest: text,
        }
    }
}

/// [`TokenStream`] implements an iterator over user-defined `Token`s.
pub struct TokenStream<'lex, 'a, Token> {
    lexer: &'lex Lexer<'a, Token>,
    current_index: usize,
    rest: &'a str,
}

impl<'lex, 'a, Token> Iterator for TokenStream<'lex, 'a, Token> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        // Nothing left to tokenize, iterator is exhausted
        if self.rest.is_empty() {
            None
        } else {
            let mut longest_match: Option<&str> = None;
            let mut longest_token: Option<Token> = None;

            // For each rule in the lexical spec
            for Rule { pattern, action } in self.lexer.rules.iter() {
                // If the pattern matches a prefix of the rest of the text
                if let Some(mat) = pattern.find(self.rest) {
                    // Ignore match if it doesn't start from beginning
                    if mat.start() != 0 {
                        continue;
                    } else {
                        let lexeme = &self.rest[..mat.end()];
                        let token = action(lexeme);

                        // Update longest match if new max found
                        if longest_match.is_none()
                            || lexeme.len() > longest_match.unwrap().len()
                        {
                            longest_match = Some(lexeme);
                            longest_token = Some(token);
                        }
                    }
                }
            }

            match (longest_token, longest_match) {
                (Some(tok), Some(lexeme)) => {
                    // Move rest past lexeme
                    self.rest = &self.rest[lexeme.len()..];
                    self.current_index += lexeme.len();

                    // Produce this token
                    Some(Ok(tok))
                }
                _ => Some(Err(LexerError::InvalidToken(self.current_index))),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use regex::Regex;

    // construct regular expressions for use in tests
    fn patterns() -> (Regex, Regex, Regex, Regex) {
        (
            Regex::new(r#"\d+"#).unwrap(),   // numbers
            Regex::new(r#"".*?""#).unwrap(), // strings
            Regex::new(r#"[a-zA-Z][a-zA-Z0-9_]+"#).unwrap(), // identifiers
            Regex::new(r#"\s+"#).unwrap(),   // whitespace
        )
    }

    #[test]
    fn simple_token_repr() {
        let (num_re, str_re, id_re, ws_re) = patterns();

        // just represent tokens as tuples
        let rules = vec![
            Rule::new(&num_re, |s| ("num", s)),
            Rule::new(&str_re, |s| ("str", s)),
            Rule::new(&id_re, |s| ("name", s)),
            Rule::new(&ws_re, |s| ("ws", s)),
        ];

        let l = Lexer::new(rules);

        assert_eq!(
            l.lex("this is 10 or \"10\"")
                .map(|r| r.unwrap())
                .collect::<Vec<_>>(),
            vec![
                ("name", "this"),
                ("ws", " "),
                ("name", "is"),
                ("ws", " "),
                ("num", "10"),
                ("ws", " "),
                ("name", "or"),
                ("ws", " "),
                ("str", "\"10\"")
            ]
        );
    }

    #[test]
    fn more_complex_token_repr() {
        let (num_re, str_re, id_re, ws_re) = patterns();

        #[derive(PartialEq, Eq, Debug)]
        enum TokenType {
            Num,
            Str,
            Name,
            Whitespace,
        }

        #[derive(PartialEq, Eq, Debug)]
        struct Token<'a> {
            tok_type: TokenType,
            lexeme: &'a str,
        }

        impl<'a> Token<'a> {
            fn new(t: TokenType, l: &str) -> Token {
                Token {
                    tok_type: t,
                    lexeme: l,
                }
            }
        }

        use TokenType::*;

        // use our richer representation of tokens
        let l = Lexer::new(vec![
            Rule::new(&num_re, |s| Token::new(Num, s)),
            Rule::new(&str_re, |s| Token::new(Str, s)),
            Rule::new(&id_re, |s| Token::new(Name, s)),
            Rule::new(&ws_re, |s| Token::new(Whitespace, s)),
        ]);

        assert_eq!(
            l.lex("\"a\"	is 91 ok")
                .map(|r| r.unwrap())
                .collect::<Vec<_>>(),
            vec![
                Token::new(Str, "\"a\""),
                Token::new(Whitespace, "	"),
                Token::new(Name, "is"),
                Token::new(Whitespace, " "),
                Token::new(Num, "91"),
                Token::new(Whitespace, " "),
                Token::new(Name, "ok")
            ]
        );
    }

    #[test]
    #[should_panic]
    fn panics_on_no_patterns() {
        Lexer::<()>::new(Vec::new());
    }

    #[test]
    fn errors_on_invalid_token() {
        let (num_re, str_re, id_re, ws_re) = patterns();
        let l = Lexer::new(vec![
            Rule::new(&num_re, |s| ("num", s)),
            Rule::new(&str_re, |s| ("str", s)),
            Rule::new(&id_re, |s| ("name", s)),
            Rule::new(&ws_re, |s| ("ws", s)),
        ]);

        assert_eq!(
            l.lex("^what is this").next(),
            Some(Err(LexerError::InvalidToken(0)))
        );

        let mut tokens = l.lex("neat $!&@# not neat");
        assert_eq!(tokens.next(), Some(Ok(("name", "neat"))));
        assert_eq!(tokens.next(), Some(Ok(("ws", " "))));
        assert_eq!(tokens.next(), Some(Err(LexerError::InvalidToken(5))));

        let mut tokens = l.lex("  5 (");
        assert_eq!(tokens.next(), Some(Ok(("ws", "  "))));
        assert_eq!(tokens.next(), Some(Ok(("num", "5"))));
        assert_eq!(tokens.next(), Some(Ok(("ws", " "))));
        assert_eq!(tokens.next(), Some(Err(LexerError::InvalidToken(4))));
    }
}
