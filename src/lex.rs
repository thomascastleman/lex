use std::iter::Iterator;
use regex::Regex;

/*
  The lexer generalizes over the lexical patterns, and what tokens 
  look like. 
*/

// ---------------------------------------------------------------------------
/// TEMP: tokens outputted by the lexer
/// ultimately this should be generic
#[derive(Debug)]
pub struct Token<'a> {
	name: &'a str,
	lexeme: &'a str
}

impl<'a> Token<'a> {
	pub fn new(name: &'a str, lexeme: &'a str) -> Token<'a> {
		Token { name: name, lexeme: lexeme }
	}
}
// ---------------------------------------------------------------------------

/// Error type for lexer errors.
/// failing_text is the rest of the text at the point
/// at which the invalid token was encountered
#[derive(Debug)]
pub struct LexError<'a> {
	failing_text: &'a str
}

impl<'a> LexError<'a> {
	fn new(text: &str) -> LexError {
		LexError { failing_text: text }
	}
}

use std::fmt;
impl<'a> fmt::Display for LexError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid token encountered at `{}`", self.failing_text)
    }
}

/// The Lexer itself
pub struct Lexer<'a> {
	// where F: Fn(&str) -> Token {
	spec: Vec<(&'a Regex, fn(&str) -> Token)>
}

impl<'a> Lexer<'a> {
	/// Constructs a new Lexer instance with an empty spec
	pub fn new() -> Lexer<'a> {
		Lexer { spec: Vec::new() }
	}

	/// Adds a lexical rule to the lexer, which describes a given token's 
	/// structure (regular expr) and how to build a token out of it.
	pub fn add_rule(&mut self, pattern: &'a Regex, make_token: fn(&str) -> Token) {
		self.spec.push((pattern, make_token));
	}

	/// Produces a stream of tokens by runnig this lexer on given text
	pub fn lex(&self, text: &'a str) -> TokenStream {
		TokenStream::new(&self, text)
	}

	/// Parses a single token off the beginning of the given string,
	/// returning the token and the rest of the string, or None
	/// if there is no more input.
	pub fn get_next_token(&self, text: &'a str) 
		-> Option<Result<(Token, &'a str), LexError>> {
		// indicate end of input if encountered
		if text.is_empty() {
			None
		} else {
			let mut longest_match: Option<&str> = None;
			let mut longest_token: Option<Token> = None;

			// for each pattern in the lexical spec
			for (pat, mk_tok) in self.spec.iter() {
				// test pattern against text
				match pat.find(text) {
					Some(mat) => {
						// ignore match if it doesn't cover the text beginning
						if mat.start() != 0 {
							continue;
						} else {
							let lexeme = &text[..mat.end()];
							let token = mk_tok(lexeme);

							// update longest match if new max found
							match longest_match {
								None => {
									longest_match = Some(lexeme);
									longest_token = Some(token);
								},
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
					let rest_of_text = &text[lexeme.len()..];
					Some(Ok((tok, rest_of_text)))
				},
				_ => Some(Err(LexError::new(text))),
			}
		}
	}
}

/// A stream of tokens
pub struct TokenStream<'a> {
	lexer: &'a Lexer<'a>,
	text_rest: &'a str
}

impl<'a> TokenStream<'a> {
	/// Constructs a new stream of tokens, given a lexer and text to lex
	fn new(lexer: &'a Lexer, text: &'a str) -> TokenStream<'a>  {
		TokenStream { lexer: lexer, text_rest: text }
	}
}

impl<'a> Iterator for TokenStream<'a> {
	type Item = Result<Token<'a>, LexError<'a>>;

	/// Produces the next token in the stream of tokens generated
	/// by lexing the given 
	fn next(&mut self) -> Option<Self::Item> {
		// get the next token from our internal lexer on its text
		match self.lexer.get_next_token(self.text_rest) {
			// end of file
			None => None,
			// a token was parsed or error
			Some(res) => match res {
				| Ok((tok, rest)) => {
					self.text_rest = rest;
					Some(Ok(tok))
				},
				| Err(e) => Some(Err(e))
			}
		}
	}
}