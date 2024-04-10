use crate::parser::{self, Rule};
use crate::{Source, Token};

pub struct Parser<S> {
    pub source: S,
}

impl<S: Source> parser::Parser for Parser<S> {
    type Token = S::Token;
    type VisitResult<Out> = Option<Out>;

    fn parse<R: parser::Rule<Token = S::Token>>(&mut self, rule: R) -> Option<R::Output> {
        rule.accept(self)
    }

    fn visit_single<F: FnOnce(&S::Token) -> bool>(
        &mut self,
        fun: F,
    ) -> Self::VisitResult<S::Token> {
        if fun(self.source.peek()) {
            Some(self.source.get())
        } else {
            None
        }
    }

    fn visit_and<L, R>(&mut self, left: L, right: R) -> Self::VisitResult<(L::Output, R::Output)>
    where
        L: Rule<Token = Self::Token>,
        R: Rule<Token = Self::Token>,
    {
        let l = self.parse(left)?;
        let r = self.parse(right).unwrap();
        Some((l, r))
    }

    fn visit_or<L, R, O>(&mut self, left: L, right: R) -> Self::VisitResult<O>
    where
        L: Rule<Token = Self::Token, Output = O>,
        R: Rule<Token = Self::Token, Output = O>,
    {
        self.parse(left).or_else(|| self.parse(right))
    }

    fn visit_list<R>(&mut self, inner: R) -> Self::VisitResult<Vec<R::Output>>
    where
        R: Rule<Token = Self::Token>,
    {
        let mut out = vec![];
        while let Some(item) = self.parse(inner) {
            out.push(item)
        }
        Some(out)
    }

    fn visit_map<O, F, R>(&mut self, inner: R, fun: F) -> Self::VisitResult<O>
    where
        F: FnOnce(R::Output) -> O,
        R: Rule<Token = Self::Token>,
    {
        self.parse(inner).map(fun)
    }
}

enum Fed<B, T> {
    Done(T),
    Ok(B),
    NoMatch(B),
}

trait Builder: Sized {
    type Output;
    type Token;

    fn feed(self, token: Self::Token) -> Fed<Self, Self::Output>;
}

enum AndBuilder<L: Builder, R> {
    Left(L, R),
    Right(L::Output, R),
}

enum OrBuilder<L, R> {
    Both(L, R),
    Left(L),
    Right(R),
}

struct MapBuilder<R, F> {
    rule: R,
    fun: F,
}

struct ListBuilder<O, T, R: Rule<Token = T, Output = O>, B: Builder<Output = O, Token = T>> {
    inner_rule: R,
    inner: B,
    in_progress: bool,
    list: Vec<O>,
}

impl<L, R, T> Builder for AndBuilder<L, R>
where
    L: Builder<Token = T>,
    R: Builder<Token = T>,
{
    type Token = T;
    type Output = (L::Output, R::Output);

    fn feed(self, token: Self::Token) -> Fed<Self, Self::Output> {
        match self {
            Self::Left(left, right) => match left.feed(token) {
                Fed::Ok(left) => Fed::Ok(Self::Left(left, right)),
                Fed::NoMatch(left) => Fed::NoMatch(Self::Left(left, right)),
                Fed::Done(left) => Fed::Ok(Self::Right(left, right)),
            },
            Self::Right(left, right) => match right.feed(token) {
                Fed::Ok(right) => Fed::Ok(Self::Right(left, right)),
                Fed::NoMatch(right) => Fed::NoMatch(Self::Right(left, right)),
                Fed::Done(right) => Fed::Done((left, right)),
            },
        }
    }
}

impl<L, R, O, T> Builder for OrBuilder<L, R>
where
    L: Builder<Output = O, Token = T>,
    R: Builder<Output = O, Token = T>,
{
    type Output = O;
    type Token = T;

    fn feed(self, token: T) -> Fed<Self, Self::Output> {
        match self {
            Self::Both(left, right) => match (left.feed(token), right.feed(token)) {
                (Fed::Done(left), _) => Fed::Done(left),
                (_, Fed::Done(right)) => Fed::Done(right),
                (Fed::Ok(left), Fed::Ok(right)) => Fed::Ok(OrBuilder::Both(left, right)),
                (Fed::NoMatch(left), Fed::NoMatch(right)) => {
                    Fed::NoMatch(OrBuilder::Both(left, right))
                }
                (Fed::Ok(left), Fed::NoMatch(right)) => Fed::Ok(Self::Left(left)),
                (Fed::NoMatch(left), Fed::Ok(right)) => Fed::Ok(Self::Right(right)),
            },
            Self::Left(left) => match left.feed(token) {
                Fed::Done(out) => Fed::Done(out),
                Fed::Ok(left) => Fed::Ok(Self::Left(left)),
                Fed::NoMatch(left) => Fed::NoMatch(Self::Left(left)),
            },
            Self::Right(right) => match right.feed(token) {
                Fed::Done(out) => Fed::Done(out),
                Fed::Ok(right) => Fed::Ok(Self::Right(right)),
                Fed::NoMatch(right) => Fed::NoMatch(Self::Right(right)),
            },
        }
    }
}

impl<R, F, O, T> Builder for MapBuilder<R, F>
where
    R: Builder<Token = T>,
    F: FnOnce(R::Output) -> O + Copy,
{
    type Output = O;
    type Token = T;

    fn feed(self, token: T) -> Fed<Self, Self::Output> {
        match self.rule.feed(token) {
            Fed::Done(inner) => Fed::Done((self.fun)(inner)),
            Fed::Ok(rule) => Fed::Ok(MapBuilder {
                rule,
                fun: self.fun,
            }),
            Fed::NoMatch(rule) => Fed::NoMatch(MapBuilder {
                rule,
                fun: self.fun,
            }),
        }
    }
}

impl<R, T, O, B> Builder for ListBuilder<O, T, R, B>
where
    B: Builder<Output = O, Token = T>,
    R: Rule<Token = T, Output = O>,
{
    type Token = T;
    type Output = Vec<O>;

    fn feed(self, token: T) -> Fed<Self, Self::Output> {
        let ListBuilder {
            inner_rule,
            inner,
            mut list,
            in_progress,
        } = self;
        match inner.feed(token) {
            Fed::Done(item) => {
                list.push(item);
                Fed::Ok(ListBuilder {
                    inner: inner_rule.builder(),
                    inner_rule,
                    list,
                    in_progress: false,
                })
            }
            Fed::Ok(inner) => Fed::Ok(ListBuilder {
                inner_rule,
                inner,
                list,
                in_progress,
            }),
            Fed::NoMatch(inner) => {
                if in_progress {
                    Fed::NoMatch(ListBuilder {
                        inner_rule,
                        inner,
                        in_progress,
                        list,
                    })
                } else {
                    Fed::Done(list)
                }
            }
        }
    }
}
