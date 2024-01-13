//! The lexer is responsible for splitting the source code into a sequence of 'token's,
//! which are smallest meaningful units of the language.
//!
//! The grammer of token is described by regular expressions.
//! More complex syntaxes are handled by the later parsing

use logos::{Lexer, Logos};

use crate::mylang::document::Span;

use super::document::Spanned;

#[derive(Logos, Debug, PartialEq, Eq, Clone, Hash)]
#[logos(skip r"\s+")]
pub enum Token<'src> {
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
    #[token("end")]
    End,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("to")]
    To,
    #[token("do")]
    Do,
    #[token("break")]
    Break,
    #[token("array")]
    Array,
    #[token("of")]
    Of,
    #[token("print")]
    Print,

    #[regex(r"true|false", parse_bool)]
    Bool(bool),

    #[regex(r"[0-9]+", parse_num, priority = 2)]
    Num(u64),

    // An identifier.
    //
    // The first character must not be numeric or a special character.
    #[regex(r"[^\s\(\){}\[\],:;=+*=\-0-9][^\s\(\){}\[\],:;=+*\-]*", priority = 2)]
    Id(&'src str),

    // A string literal surrounded by double quotes.
    #[regex(r#""(?:[^"]|\\")*""#, parse_str_literal)]
    String(&'src str),

    Invalid,
}

/// Takes source code of the program and split it into sequence of tokens.
pub fn tokenize(src: &str) -> Vec<Spanned<Token>> {
    let results = Token::lexer(src).spanned();

    results
        .map(|(result, span)| match result {
            Ok(token) => Spanned::new(token, Span::from(span)),
            Err(_) => Spanned::new(Token::Invalid, Span::from(span)),
        })
        .collect()
}

fn parse_num<'src>(lex: &Lexer<'src, Token<'src>>) -> u64 {
    lex.slice().parse().unwrap()
}

fn parse_bool<'src>(lex: &Lexer<'src, Token<'src>>) -> bool {
    match lex.slice() {
        "true" => true,
        "false" => false,
        _ => panic!("Invalid boolean literal"),
    }
}

fn parse_str_literal<'src>(lex: &Lexer<'src, Token<'src>>) -> &'src str {
    // Strip the surrounding quotes.
    &lex.slice()[1..lex.slice().len() - 1]
}

impl From<logos::Span> for Span {
    fn from(span: logos::Span) -> Span {
        Span::new(span.start, span.end)
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
        let expected = vec![token(Token::LParen, 0, 1)];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn single_char_with_whitespace() {
        let src = " ( \n\t";
        let tokens = tokenize(src);
        let expected = vec![token(Token::LParen, 1, 2)];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn simple_arithmetic() {
        let src = "(8 + 2)";
        let tokens = tokenize(src);
        let expected = vec![
            token(Token::LParen, 0, 1),
            token(Token::Num(8), 1, 2),
            token(Token::Plus, 3, 4),
            token(Token::Num(2), 5, 6),
            token(Token::RParen, 6, 7),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn negative_numbers() {
        let src = "-8 - -2";
        let tokens = tokenize(src);
        let expected = vec![
            token(Token::Minus, 0, 1),
            token(Token::Num(8), 1, 2),
            token(Token::Minus, 3, 4),
            token(Token::Minus, 5, 6),
            token(Token::Num(2), 6, 7),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn for_loop() {
        let src = indoc! {r#"
            for i := 0 to 10 do
                x
            end
        "#};
        let tokens = tokenize(src);
        let expected = vec![
            token(Token::For, 0, 3),
            token(Token::Id("i"), 4, 5),
            token(Token::ColonEq, 6, 8),
            token(Token::Num(0), 9, 10),
            token(Token::To, 11, 13),
            token(Token::Num(10), 14, 16),
            token(Token::Do, 17, 19),
            token(Token::Id("x"), 24, 25),
            token(Token::End, 26, 29),
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
            token(Token::Let, 0, 3),
            token(Token::Var, 8, 11),
            token(Token::Id("N"), 12, 13),
            token(Token::ColonEq, 14, 16),
            token(Token::Num(8), 17, 18),
            token(Token::In, 23, 25),
            token(Token::Id("N"), 26, 27),
            token(Token::End, 28, 31),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn invalid_tokens() {
        let src = "val N1 :a= 829zd9d";
        let tokens = tokenize(src);
        let expected = vec![
            token(Token::Id("val"), 0, 3),
            token(Token::Id("N1"), 4, 6),
            token(Token::Colon, 7, 8),
            token(Token::Id("a"), 8, 9),
            token(Token::Eq, 9, 10),
            token(Token::Num(829), 11, 14),
            token(Token::Id("zd9d"), 14, 18),
        ];
        assert_eq!(tokens, expected);
    }

    /// A helper function to create a [Spanned<Token>] instance.
    fn token(kind: Token, start: usize, end: usize) -> Spanned<Token> {
        Spanned::new(kind, Span::new(start, end))
    }
}
