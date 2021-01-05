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
	/// returning the token and the rest of the string
	pub fn get_next_token(&self, text: &str) -> (Token, &str) {
		// this is where the main lexing logic goes
		unimplemented!();
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
	type Item = Token<'a>;

	/// Produces the next token in the stream of tokens generated
	/// by lexing the given 
	fn next(&mut self) -> Option<Self::Item> {
		unimplemented!();
	}
}