use regex::Regex;

/// Error type for lexer errors.
/// - `NoPatterns`: tried to lex with no patterns added.
/// - `InvalidToken(idx)`: token that doesn't match any pattern
///     found at index idx in the input string.
#[derive(Debug, PartialEq, Eq)]
pub enum LexerError {
    NoPatterns,
    InvalidToken(usize),
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexerError::NoPatterns => {
                write!(f, "lexer error: no patterns supplied to lexer")
            }
            LexerError::InvalidToken(idx) => {
                write!(f, "lexer error: invalid token at index {}", idx)
            }
        }
    }
}

/// The Lexer itself.
/// - spec: a vector of (pattern, make_token) tuples, where
///     pattern is a regular expr, and make_token is a function
/// 	which constructs a token from a match of the pattern.
pub struct Lexer<'a, Token> {
    spec: Vec<(&'a Regex, fn(&'a str) -> Token)>,
}

impl<'a, Token> Lexer<'a, Token> {
    /// Constructs a new Lexer instance with an empty spec
    pub fn new() -> Lexer<'a, Token> {
        Lexer { spec: Vec::new() }
    }

    /// Adds a lexical rule to the lexer, which describes a given token's
    /// structure (regular expr) and how to build a token out of it.
    pub fn add_pattern(
        &mut self,
        pattern: &'a Regex,
        make_token: fn(&'a str) -> Token,
    ) {
        self.spec.push((pattern, make_token));
    }

    /// Produces a vector of tokens lexed from the given text, or a lexer error.
    pub fn lex(&self, text: &'a str) -> Result<Vec<Token>, LexerError> {
        // check that lexical patterns exist in internal state
        if self.spec.is_empty() {
            Err(LexerError::NoPatterns)
        } else {
            let mut tokens = Vec::new();

            // rest begins as entire text, starting index 0
            let mut rest = text;
            let mut rest_start_idx = 0;

            // while there is text left to tokenize
            while !rest.is_empty() {
                let mut longest_match: Option<&str> = None;
                let mut longest_token: Option<Token> = None;

                // for each pattern in the lexical spec
                for (pat, mk_tok) in self.spec.iter() {
                    // test pattern against text
                    if let Some(mat) = pat.find(rest) {
                        // ignore match if it doesn't start from beginning
                        if mat.start() != 0 {
                            continue;
                        } else {
                            let lexeme = &rest[..mat.end()];
                            let token = mk_tok(lexeme);

                            // update longest match if new max found
                            match longest_match {
                                None => {
                                    longest_match = Some(lexeme);
                                    longest_token = Some(token);
                                }
                                Some(long_lexeme) => {
                                    if lexeme.len() > long_lexeme.len() {
                                        longest_match = Some(lexeme);
                                        longest_token = Some(token);
                                    }
                                }
                            }
                        }
                    }
                }

                match (longest_token, longest_match) {
                    (Some(tok), Some(lexeme)) => {
                        // move rest past lexeme
                        rest = &rest[lexeme.len()..];
                        rest_start_idx += lexeme.len();

                        // add token to token vector
                        tokens.push(tok);
                    }
                    _ => {
                        return Err(LexerError::InvalidToken(rest_start_idx));
                    }
                }
            }

            Ok(tokens)
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
        let mut l = Lexer::new();

        // just represent tokens as tuples
        l.add_pattern(&num_re, |s| ("num", s));
        l.add_pattern(&str_re, |s| ("str", s));
        l.add_pattern(&id_re, |s| ("name", s));
        l.add_pattern(&ws_re, |s| ("ws", s));

        assert_eq!(
            l.lex("this is 10 or \"10\"").unwrap(),
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
        let mut l = Lexer::new();

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
        l.add_pattern(&num_re, |s| Token::new(Num, s));
        l.add_pattern(&str_re, |s| Token::new(Str, s));
        l.add_pattern(&id_re, |s| Token::new(Name, s));
        l.add_pattern(&ws_re, |s| Token::new(Whitespace, s));

        assert_eq!(
            l.lex("\"a\"	is 91 ok").unwrap(),
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
    fn errors_on_no_patterns() {
        let l = Lexer::<(&str, &str)>::new();
        assert_eq!(l.lex("something"), Err(LexerError::NoPatterns));
        assert_eq!(l.lex(""), Err(LexerError::NoPatterns));
    }

    #[test]
    fn errors_on_invalid_token() {
        let (num_re, str_re, id_re, ws_re) = patterns();
        let mut l = Lexer::new();

        l.add_pattern(&num_re, |s| ("num", s));
        l.add_pattern(&str_re, |s| ("str", s));
        l.add_pattern(&id_re, |s| ("name", s));
        l.add_pattern(&ws_re, |s| ("ws", s));

        assert_eq!(l.lex("^what is this"), Err(LexerError::InvalidToken(0)));
        assert_eq!(
            l.lex("neat $!&@# not neat"),
            Err(LexerError::InvalidToken(5))
        );
        assert_eq!(l.lex("  5 ("), Err(LexerError::InvalidToken(4)));
    }
}
