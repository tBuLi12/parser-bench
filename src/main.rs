mod pull_parser;
mod push_parser;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Keyword {
    Fun,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Punctuation {
    Dot,
    Comma,
}

#[derive(Clone, Copy)]
enum Token {
    Ident(u32),
    Keyword(Keyword),
    Punctuation(Punctuation),
    Eof,
}

pub trait Source {
    type Token;

    fn get(&mut self) -> Self::Token;
    fn peek(&self) -> &Self::Token;
}

struct CycledInput {
    i: usize,
    items: Vec<Token>,
}

impl Source for CycledInput {
    type Token = Token;

    fn get(&mut self) -> Token {
        let token = self.items[self.i];
        self.i += 1;
        if self.i == self.items.len() {
            self.i = 0;
        }
        token
    }

    fn peek(&self) -> &Token {
        &self.items[self.i]
    }
}

mod pull_grammar {
    use crate::{pull_parser::*, Keyword, Punctuation, Token};

    fn keyword(keyword: Keyword) -> impl Rule<Token = Token, Output = Token> {
        single(move |token: &Token| match token {
            Token::Keyword(kw) if *kw == keyword => true,
            _ => false,
        })
    }

    fn punctuation(punct: Punctuation) -> impl Rule<Token = Token, Output = Token> {
        single(move |token: &Token| match token {
            Token::Punctuation(p) if *p == punct => true,
            _ => false,
        })
    }

    fn ident() -> impl Rule<Token = Token, Output = Token> {
        single(move |token: &Token| match token {
            Token::Ident(_) => true,
            _ => false,
        })
    }

    pub fn grammar() -> impl Rule<Token = Token, Output = Token> {
        keyword(Keyword::Fun)
            .and(punctuation(Punctuation::Dot))
            .and(ident().list())
            .and(punctuation(Punctuation::Dot))
    }
}

fn main() {
    let mut input = CycledInput {
        i: 0,
        items: vec![
            Token::Keyword(Keyword::Fun),
            Token::Punctuation(Punctuation::Dot),
            Token::Ident(30),
            Token::Ident(30),
            Token::Ident(30),
            Token::Punctuation(Punctuation::Dot),
        ],
    };

    let grammar = pull_grammar::grammar();

    for _ in 0..1_000_000 {
        grammar.parse(input);
    }

    // 'outer: for _ in 0..1_000_000 {
    //     let mut builder = push_fun.builder();
    //     for _ in 0..6 {
    //         match builder.feed(input.get()) {
    //             Fed::Ok(next) => builder = next,
    //             _ => continue 'outer,
    //         }
    //     }
    // }
}
