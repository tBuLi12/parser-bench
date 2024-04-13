use std::time::Instant;

mod pull_fix_parser;
mod pull_parser;
mod push_parser;
mod stack_parser;

// #[derive(Clone, Copy, PartialEq, Eq, Debug)]
// enum Keyword {
//     Fun,
// }

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Punctuation {
    // Dot,
    Comma,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,
}

#[derive(Clone, Debug)]
enum Token {
    String(String),
    Number(usize),
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
        let token = self.items[self.i].clone();
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

#[derive(Debug)]
enum Json {
    String(String),
    Number(usize),
    Object(Vec<(String, Json)>),
    Array(Vec<Json>),
}

mod pull_grammar {
    use crate::{pull_parser::*, Json, Punctuation, Token};

    fn punctuation(punct: Punctuation) -> impl Rule<Token = Token, Output = ()> {
        single(move |token: &Token| match token {
            Token::Punctuation(p) if *p == punct => true,
            _ => false,
        })
        .map(|_| ())
    }

    fn string() -> impl Rule<Token = Token, Output = String> {
        single(move |token: &Token| match token {
            Token::String(_) => true,
            _ => false,
        })
        .map(|token| {
            if let Token::String(value) = token {
                value
            } else {
                unreachable!()
            }
        })
    }

    fn number() -> impl Rule<Token = Token, Output = usize> {
        single(move |token: &Token| match token {
            Token::Number(_) => true,
            _ => false,
        })
        .map(|token| {
            if let Token::Number(value) = token {
                value
            } else {
                unreachable!()
            }
        })
    }

    fn prop() -> impl Rule<Token = Token, Output = (String, Json)> {
        string()
            .and(punctuation(Punctuation::Colon))
            .map(|(s, _)| s)
            .and(JsonRule)
    }

    fn object() -> impl Rule<Token = Token, Output = Vec<(String, Json)>> {
        punctuation(Punctuation::LBrace)
            .and(prop())
            .and(
                punctuation(Punctuation::Comma)
                    .and(prop())
                    .map(|(_, json)| json)
                    .list(),
            )
            .and(punctuation(Punctuation::RBrace))
            .map(|(((_, first), mut props), _)| {
                props.insert(0, first);
                props
            })
    }

    fn array() -> impl Rule<Token = Token, Output = Vec<Json>> {
        punctuation(Punctuation::LBracket)
            .and(JsonRule)
            .and(
                punctuation(Punctuation::Comma)
                    .and(JsonRule)
                    .map(|(_, json)| json)
                    .list(),
            )
            .and(punctuation(Punctuation::RBracket))
            .map(|(((_, first), mut props), _)| {
                props.insert(0, first);
                props
            })
    }

    #[derive(Clone, Copy)]
    struct JsonRule;

    impl NamedRule for JsonRule {
        type Token = Token;
        type Output = Json;

        fn get(self) -> impl Rule<Token = Self::Token, Output = Self::Output> {
            string()
                .map(Json::String)
                .or(number().map(Json::Number))
                .or(array().map(Json::Array))
                .or(object().map(Json::Object))
        }
    }

    pub fn grammar() -> impl Rule<Token = Token, Output = Json> {
        JsonRule
    }
}

mod push_grammar {
    use crate::{push_parser::*, Json, Punctuation, Token};

    fn punctuation(punct: Punctuation) -> impl Rule<Token = Token, Output = ()> {
        single(move |token: &Token| match token {
            Token::Punctuation(p) if *p == punct => true,
            _ => false,
        })
        .map(|_| ())
    }

    fn string() -> impl Rule<Token = Token, Output = String> {
        single(move |token: &Token| match token {
            Token::String(_) => true,
            _ => false,
        })
        .map(|token| {
            if let Token::String(value) = token {
                value
            } else {
                unreachable!()
            }
        })
    }

    fn number() -> impl Rule<Token = Token, Output = usize> {
        single(move |token: &Token| match token {
            Token::Number(_) => true,
            _ => false,
        })
        .map(|token| {
            if let Token::Number(value) = token {
                value
            } else {
                unreachable!()
            }
        })
    }

    fn prop() -> impl Rule<Token = Token, Output = (String, Json)> {
        string()
            .and(punctuation(Punctuation::Colon))
            .map(|(s, _)| s)
            .and(InnerJsonRule)
    }

    fn object() -> impl Rule<Token = Token, Output = Vec<(String, Json)>> {
        punctuation(Punctuation::LBrace)
            .and(prop())
            .and(
                punctuation(Punctuation::Comma)
                    .and(prop())
                    .map(|(_, json)| json)
                    .list(),
            )
            .and(punctuation(Punctuation::RBrace))
            .map(|(((_, first), mut props), _)| {
                props.insert(0, first);
                props
            })
    }

    fn array() -> impl Rule<Token = Token, Output = Vec<Json>> {
        punctuation(Punctuation::LBracket)
            .and(InnerJsonRule)
            .and(
                punctuation(Punctuation::Comma)
                    .and(InnerJsonRule)
                    .map(|(_, json)| json)
                    .list(),
            )
            .and(punctuation(Punctuation::RBracket))
            .map(|(((_, first), mut props), _)| {
                props.insert(0, first);
                props
            })
    }

    #[derive(Clone, Copy)]
    struct JsonRule;

    impl NamedRule for JsonRule {
        type Token = Token;
        type Output = Json;

        fn get(self) -> impl Rule<Token = Self::Token, Output = Self::Output> {
            string()
                .map(Json::String)
                .or(number().map(Json::Number))
                .or(array().map(Json::Array))
                .or(object().map(Json::Object))
        }
    }
    #[derive(Clone, Copy)]
    struct InnerJsonRule;

    impl NamedRule for InnerJsonRule {
        type Token = Token;
        type Output = Json;

        fn get(self) -> impl Rule<Token = Self::Token, Output = Self::Output> {
            string().map(Json::String).or(number().map(Json::Number))
        }
    }

    pub fn grammar() -> impl Rule<Token = Token, Output = Json> {
        JsonRule
    }
}

fn main() {
    let mut input = CycledInput {
        i: 0,
        items: vec![
            Token::Punctuation(Punctuation::LBrace),
            Token::String("key".to_string()),
            Token::Punctuation(Punctuation::Colon),
            Token::String("value".to_string()),
            Token::Punctuation(Punctuation::RBrace),
        ],
    };

    use pull_parser::Rule;
    let grammar = pull_grammar::grammar();

    eprintln!("{:#?}", grammar.parse(&mut input));

    let start = Instant::now();
    for _ in 0..1_000_000 {
        grammar.parse(&mut input);
    }
    let time = start.elapsed();

    println!("{}", time.as_millis())

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
