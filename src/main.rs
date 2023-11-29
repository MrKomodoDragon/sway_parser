use chumsky::Stream;
use chumsky::{error::Simple, prelude::*};
use logos::Logos;
use std::str::FromStr;

#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
enum Token<'a> {
    #[token(",")]
    Comma,

    #[regex(r#"(?x)" (?: \\. | [^\\"] )* ""#)]
    Str(&'a str),

    #[regex("([1-9][0-9]*)|0")]
    Integer(&'a str),

    #[regex("[0-9]+\\.[0-9]+")]
    Float(&'a str),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_-]*", priority = 2)]
    Ident(&'a str),

    #[token("true")]
    #[token("false")]
    Boolean(&'a str),
    #[error]
    #[regex(r"//.*", logos::skip)]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

#[derive(Debug, Clone)]
struct Output {
    monitor_name: IdentAst,
    monitor_x: Literal,
    monitor_y: Literal,
}

#[derive(Debug, Clone)]
pub struct IdentAst {
    pub name: String,
}

#[derive(Debug, Clone)]
enum Literal {
    Float(f64),
    Integer(i128),
    Str(String),
}

pub fn parse<'a>(
    tokens: Vec<(Token<'a>, std::ops::Range<usize>)>,
) -> Result<Root, Vec<Simple<Token<'a>>>> {
    let span = (&(tokens.last().unwrap()).1).clone();
    let stream = Stream::from_iter(span, tokens.iter().cloned());
    let parser = Root::parser();
    parser.parse(stream)
}

impl Literal {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        filter_map(|span, token| match token {
            Token::Float(float) => Ok(Literal::Float(f64::from_str(float).unwrap())),
            Token::Integer(integer) => Ok(Literal::Integer(i128::from_str(integer).unwrap())),
            Token::Str(string) => Ok(Literal::Str(String::from(string))),
            _ => Err(Simple::expected_input_found(
                span,
                [
                    Some(Token::Integer("...")),
                    Some(Token::Float("...")),
                    Some(Token::Str("...")),
                    Some(Token::Boolean("...")),
                ],
                Some(token),
            )),
        })
    }
}

#[derive(Debug, Clone)]
enum Statement {
    Output(Output),
}

#[derive(Debug, Clone)]
struct Root {
    statements: Vec<Statement>,
}

impl Root {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        Statement::parser().repeated().map(|statements| Root {statements: statements})
    }
}

impl Statement {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        recursive(|stmt| Output::parser().map(Statement::Output))
    }
}

impl IdentAst {
    pub fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        select! {
         Token::Ident(i) => IdentAst {name: i.to_string()}
        }
        .labelled("IdentAst")
    }
}

impl Output {
    fn parser<'a>() -> impl chumsky::Parser<Token<'a>, Self, Error = Simple<Token<'a>>> {
        just(Token::Ident("output"))
            .ignore_then(IdentAst::parser())
            .then_ignore(just(Token::Ident("position")))
            .then(Literal::parser())
            .then_ignore(just(Token::Comma))
            .then(Literal::parser())
            .map(|((name, x), y)| Output {
                monitor_name: name,
                monitor_x: x,
                monitor_y: y,
            })
    }
}

fn main() {
    let tokens: Vec<_> = Token::lexer("output HDMI-A-1 position 0,0\noutput eDP-1 position 0,1080")
        .spanned()
        .collect();
    let tree = parse(tokens);
    println!("{:#?}", tree)
}
