use regex::Regex;

/// Error type for lexer errors.
/// - `NoPatterns` indicates a lexer was invoked with no patterns added.
/// - `InvalidToken(idx)` means a token that doesn't match any pattern
///     was found at index idx in the input string.
#[derive(Debug, PartialEq, Eq)]
pub enum LexerError {
    NoPatterns,
    InvalidToken(usize),
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexerError::NoPatterns => {
                write!(f, "no patterns supplied to lexer")
            }
            LexerError::InvalidToken(idx) => {
                write!(f, "invalid token at index {}", idx)
            }
        }
    }
}

pub struct Rule<'text, 'pattern, 'action, Token> {
    pattern: &'pattern Regex,
    action: &'action dyn Fn(&'text str) -> Token,
}

impl<'text, 'pattern, 'action, Token> Rule<'text, 'pattern, 'action, Token>
where
    Token: 'text,
{
    /// Construct a new `Rule` for use in a lexer.
    pub fn new<Action>(
        pattern: &'pattern Regex,
        action: &'action Action,
    ) -> Self
    where
        Action: Fn(&'text str) -> Token,
    {
        Rule { pattern, action }
    }
}

pub struct Lexer<'text, 'pattern, 'action, Token> {
    rules: Vec<Rule<'text, 'pattern, 'action, Token>>,
}

impl<'text, 'pattern, 'action, Token> Lexer<'text, 'pattern, 'action, Token> {
    /// Constructs a new Lexer instance from a given sequence of rules.
    pub fn new(
        rules: Vec<Rule<'text, 'pattern, 'action, Token>>,
    ) -> Lexer<'text, 'pattern, 'action, Token> {
        assert!(!rules.is_empty(), "lexer must have at least one rule");
        Lexer { rules }
    }

    /// Constructs a lexed token stream from a given text, using this lexer.
    pub fn lex<'lex>(
        &'lex self,
        text: &'text str,
    ) -> TokenStream<'text, 'lex, 'pattern, 'action, Token> {
        TokenStream {
            current_index: 0,
            rest: text,
            lexer: self,
        }
    }
}
pub struct TokenStream<'text, 'lex, 'pattern, 'action, Token>
where
    Token: 'text,
{
    current_index: usize,
    rest: &'text str,
    lexer: &'lex Lexer<'text, 'pattern, 'action, Token>,
}

impl<'text, 'lex, 'rules, 'action, Token> Iterator
    for TokenStream<'text, 'lex, 'rules, 'action, Token>
where
    Token: 'text,
{
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

                        // update longest match if new max found
                        match longest_match {
                            None => {
                                longest_match = Some(lexeme);
                                longest_token = Some(token);
                            }
                            Some(long_lexeme)
                                if lexeme.len() > long_lexeme.len() =>
                            {
                                longest_match = Some(lexeme);
                                longest_token = Some(token);
                            }
                            _ => (),
                        }
                    }
                }
            }

            match (longest_token, longest_match) {
                (Some(tok), Some(lexeme)) => {
                    // move rest past lexeme
                    self.rest = &self.rest[lexeme.len()..];
                    self.current_index += lexeme.len();

                    // add token to token vector
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
            Rule::new(&num_re, &|s| ("num", s)),
            Rule::new(&str_re, &|s| ("str", s)),
            Rule::new(&id_re, &|s| ("name", s)),
            Rule::new(&ws_re, &|s| ("ws", s)),
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
            Rule::new(&num_re, &|s| Token::new(Num, s)),
            Rule::new(&str_re, &|s| Token::new(Str, s)),
            Rule::new(&id_re, &|s| Token::new(Name, s)),
            Rule::new(&ws_re, &|s| Token::new(Whitespace, s)),
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
            Rule::new(&num_re, &|s| ("num", s)),
            Rule::new(&str_re, &|s| ("str", s)),
            Rule::new(&id_re, &|s| ("name", s)),
            Rule::new(&ws_re, &|s| ("ws", s)),
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
