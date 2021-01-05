use regex::Regex;

// ---------------------------------------------------------------------------
/// TEMP: tokens outputted by the lexer
/// ultimately this should be generic
#[derive(Debug)]
pub struct Token<'a> {
    name: &'a str,
    lexeme: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(name: &'a str, lexeme: &'a str) -> Token<'a> {
        Token {
            name: name,
            lexeme: lexeme,
        }
    }
}
// ---------------------------------------------------------------------------

/// Error type for lexer errors.
/// - NoPatterns: tried to lex with no patterns added.
/// - InvalidToken(idx): token that doesn't match any pattern
/// 	found at index idx in the input string.
#[derive(Debug)]
pub enum LexerError {
    NoPatterns,
    InvalidToken(usize),
}

// implement Display so LexerErrors can be printed
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
/// 	pattern is a regular expr, and make_token is a function
/// 	which constructs a token from a match of the pattern.
pub struct Lexer<'a> {
    spec: Vec<(&'a Regex, fn(&str) -> Token)>,
}

impl<'a> Lexer<'a> {
    /// Constructs a new Lexer instance with an empty spec
    pub fn new() -> Lexer<'a> {
        Lexer { spec: Vec::new() }
    }

    /// Adds a lexical rule to the lexer, which describes a given token's
    /// structure (regular expr) and how to build a token out of it.
    pub fn add_pattern(
        &mut self,
        pattern: &'a Regex,
        make_token: fn(&str) -> Token,
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
            let mut rest = &text[..];
            let mut rest_start_idx = 0;

            // while there is text left to tokenize
            while !rest.is_empty() {
                let mut longest_match: Option<&str> = None;
                let mut longest_token: Option<Token> = None;

                // for each pattern in the lexical spec
                for (pat, mk_tok) in self.spec.iter() {
                    // test pattern against text
                    match pat.find(rest) {
                        Some(mat) => {
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
                        None => (),
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
