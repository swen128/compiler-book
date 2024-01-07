//! The lexer is responsible for splitting the source code into a sequence of 'token's,
//! which are smallest meaningful units of the language.
//! 
//! The grammer of token is described by regular expressions.
//! More complex syntaxes are handled by the later parsing 

use logos::{Lexer, Logos};

use crate::mylang::document::Span;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"\s+")]
pub enum TokenKind<'src> {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("=")]
    Eq,
    #[token(":=")]
    ColonEq,

    #[token("+")]
    Plus,
    #[token("*")]
    Times,
    #[token("-")]
    Minus,

    // Reserved keywords
    #[token("type")]
    Type,
    #[token("function")]
    Fn,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("let")]
    Let,
    #[token("var")]
    Var,
    #[token("in")]
    In,
    #[token("begin")]
    Begin,
    #[token("end")]
    End,
    #[token("print")]
    Print,

    #[regex(r"true|false", parse_bool)]
    Bool(bool),

    #[regex(r"-?[0-9]+", parse_num, priority = 2)]
    Num(i64),

    // An identifier.
    //
    // The first character must not be numeric or a special character.
    #[regex(r"[^\s\(\){}\[\],:;=+*=\-0-9][^\s\(\){}\[\],:;=+*\-]*", priority = 2)]
    Id(&'src str),

    // A string literal surrounded by double quotes.
    #[regex(r#""(?:[^"]|\\")*""#, parse_str_literal)]
    String(&'src str),

    // An invalid token.
    //
    // When the lexer encouters an invalid token, it skips until next special character,
    // and then continues tokenizing the rest of text.
    #[regex(r"[^\s\(\){}\[\],:;=+*\-]+", priority = 1)]
    Invalid(&'src str),
}

#[derive(PartialEq)]
pub struct Token<'src> {
    pub token: TokenKind<'src>,
    /// The position of the token in the source code.
    pub span: Span,
}

impl Token<'_> {
    fn new(token: TokenKind, start: usize, end: usize) -> Token {
        Token {
            token,
            span: Span::new(start, end),
        }
    }

    pub fn is_invalid(&self) -> bool {
        if let TokenKind::Invalid(_) = self.token {
            true
        } else {
            false
        }
    }
}

/// Takes source code of the program and split it into sequence of tokens.
pub fn tokenize(src: &str) -> Vec<Token> {
    let results = TokenKind::lexer(src).spanned();

    results
        .map(|(result, span)| match result {
            Ok(token) => Token {
                token,
                span: Span::from(span),
            },

            Err(_) => panic!("Tokenization error"),
        })
        .collect()
}

fn parse_num<'src>(lex: &Lexer<'src, TokenKind<'src>>) -> i64 {
    lex.slice().parse().unwrap()
}

fn parse_bool<'src>(lex: &Lexer<'src, TokenKind<'src>>) -> bool {
    match lex.slice() {
        "true" => true,
        "false" => false,
        _ => panic!("Invalid boolean literal"),
    }
}

fn parse_str_literal<'src>(lex: &Lexer<'src, TokenKind<'src>>) -> &'src str {
    // Strip the surrounding quotes.
    &lex.slice()[1..lex.slice().len() - 1]
}

impl std::fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok({
            f.write_fmt(format_args!(
                "Token {{{:?} at {}..{}}}",
                self.token, self.span.start, self.span.end
            ))?;
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn empty() {
        let src = "";
        let tokens = tokenize(src);
        let expected = vec![];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn whitespace_only() {
        let src = " \t\n";
        let tokens = tokenize(src);
        let expected = vec![];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn left_paren() {
        let src = "(";
        let tokens = tokenize(src);
        let expected = vec![Token::new(TokenKind::LParen, 0, 1)];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn single_char_with_whitespace() {
        let src = " ( \n\t";
        let tokens = tokenize(src);
        let expected = vec![Token::new(TokenKind::LParen, 1, 2)];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn simple_arithmetic() {
        let src = "(8 + 2)";
        let tokens = tokenize(src);
        let expected = vec![
            Token::new(TokenKind::LParen, 0, 1),
            Token::new(TokenKind::Num(8), 1, 2),
            Token::new(TokenKind::Plus, 3, 4),
            Token::new(TokenKind::Num(2), 5, 6),
            Token::new(TokenKind::RParen, 6, 7),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn negative_numbers() {
        let src = "-8 - -2";
        let tokens = tokenize(src);
        let expected = vec![
            Token::new(TokenKind::Num(-8), 0, 2),
            Token::new(TokenKind::Minus, 3, 4),
            Token::new(TokenKind::Num(-2), 5, 7),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn valid_tokens() {
        let src = indoc! {r#"
            let
                var N := 8
                in N
            end
        "#};
        let tokens = tokenize(src);
        let expected = vec![
            Token::new(TokenKind::Let, 0, 3),
            Token::new(TokenKind::Var, 8, 11),
            Token::new(TokenKind::Id("N"), 12, 13),
            Token::new(TokenKind::ColonEq, 14, 16),
            Token::new(TokenKind::Num(8), 17, 18),
            Token::new(TokenKind::In, 23, 25),
            Token::new(TokenKind::Id("N"), 26, 27),
            Token::new(TokenKind::End, 28, 31),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn invalid_tokens() {
        let src = "val N1 :a= 829zd9d";
        let tokens = tokenize(src);
        let expected = vec![
            Token::new(TokenKind::Id("val"), 0, 3),
            Token::new(TokenKind::Id("N1"), 4, 6),
            Token::new(TokenKind::Colon, 7, 8),
            Token::new(TokenKind::Id("a"), 8, 9),
            Token::new(TokenKind::Eq, 9, 10),
            Token::new(TokenKind::Invalid("829zd9d"), 11, 18),
        ];
        assert_eq!(tokens, expected);
    }
}
