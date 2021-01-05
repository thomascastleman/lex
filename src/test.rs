#[cfg(test)]
mod tests {
    use crate::lex::*;
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
