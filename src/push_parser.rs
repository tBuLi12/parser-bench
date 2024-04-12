use std::marker::PhantomData;

use crate::Source;

pub trait Rule: Copy {
    type Token: Clone;
    type Output;

    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output> {
        let mut builder = self.builder();
        loop {
            match builder.feed(input.get()) {
                Fed::Ok(next) => builder = next,
                Fed::Done(item, _) => return Some(item),
                Fed::NoMatch(_) => return None,
            }
        }
    }

    fn builder(self) -> impl Builder<Output = Self::Output, Token = Self::Token>;

    fn and<Other: Rule<Token = Self::Token>>(
        self,
        other: Other,
    ) -> impl Rule<Token = Self::Token, Output = (Self::Output, Other::Output)> {
        And(self, other)
    }

    fn or<Other: Rule<Token = Self::Token, Output = Self::Output>>(
        self,
        other: Other,
    ) -> impl Rule<Token = Self::Token, Output = Self::Output> {
        Or(self, other)
    }

    fn list(self) -> impl Rule<Token = Self::Token, Output = Vec<Self::Output>> {
        List(self)
    }

    fn map<O, F: FnOnce(Self::Output) -> O + Copy>(
        self,
        fun: F,
    ) -> impl Rule<Token = Self::Token, Output = O> {
        Map { rule: self, fun }
    }
}

pub trait NamedRule {
    type Token: Clone;
    type Output;

    fn get(self) -> impl Rule<Output = Self::Output, Token = Self::Token>;
}

impl<T: NamedRule + Copy> Rule for T {
    type Token = <T as NamedRule>::Token;
    type Output = <T as NamedRule>::Output;

    fn builder(self) -> impl Builder<Output = Self::Output, Token = Self::Token> {
        self.get().builder()
    }
}

pub trait Builder: Sized {
    type Output;
    type Token;

    fn feed(self, token: Self::Token) -> Fed<Self, Self::Output, Self::Token>;
}

pub enum Fed<B, T, Token> {
    Done(T, Option<Token>),
    Ok(B),
    NoMatch(B),
}

pub fn single<T: Clone, F: FnOnce(&T) -> bool + Copy>(fun: F) -> impl Rule<Token = T, Output = T> {
    Single(fun, PhantomData)
}

struct Single<T, F>(F, PhantomData<fn(&T)>);
impl<T, F: Copy> Copy for Single<T, F> {}
impl<T, F: Clone> Clone for Single<T, F> {
    fn clone(&self) -> Self {
        Single(self.0.clone(), PhantomData)
    }
}

#[derive(Clone, Copy)]
struct And<L, R>(L, R);

#[derive(Clone, Copy)]
struct Or<L, R>(L, R);

#[derive(Clone, Copy)]
struct Map<R, F> {
    rule: R,
    fun: F,
}

#[derive(Clone, Copy)]
struct List<R>(R);

impl<T: Clone, F: FnOnce(&T) -> bool + Copy> Rule for Single<T, F> {
    type Token = T;
    type Output = T;

    fn builder(self) -> Self {
        self
    }
}

impl<T: Clone, L, R> Rule for And<L, R>
where
    L: Rule<Token = T>,
    R: Rule<Token = T>,
{
    type Token = T;
    type Output = (L::Output, R::Output);

    fn builder(self) -> impl Builder<Output = (L::Output, R::Output), Token = T> {
        AndBuilder::Left(self.0.builder(), self.1.builder())
    }
}

impl<T: Clone, O, L, R> Rule for Or<L, R>
where
    L: Rule<Token = T, Output = O>,
    R: Rule<Token = T, Output = O>,
{
    type Token = T;
    type Output = O;

    fn builder(self) -> impl Builder<Output = O, Token = T> {
        OrBuilder::Both(self.0.builder(), self.1.builder())
    }
}

impl<T: Clone, O, F, R> Rule for Map<R, F>
where
    R: Rule<Token = T>,
    F: FnOnce(R::Output) -> O + Copy,
{
    type Token = T;
    type Output = O;

    fn builder(self) -> impl Builder<Output = O, Token = T> {
        MapBuilder {
            rule: self.rule.builder(),
            fun: self.fun,
        }
    }
}

impl<T: Clone, R> Rule for List<R>
where
    R: Rule<Token = T>,
{
    type Token = T;
    type Output = Vec<R::Output>;

    fn builder(self) -> impl Builder<Output = Vec<R::Output>, Token = T> {
        ListBuilder {
            in_progress: false,
            get_inner_rule: move || self.0.builder(),
            inner: self.0.builder(),
            list: vec![],
        }
    }
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

struct ListBuilder<F, B: Builder> {
    get_inner_rule: F,
    inner: B,
    in_progress: bool,
    list: Vec<B::Output>,
}

impl<T, F: FnOnce(&T) -> bool + Copy> Builder for Single<T, F> {
    type Token = T;
    type Output = T;

    fn feed(self, token: Self::Token) -> Fed<Self, Self::Output, Self::Token> {
        if (self.0)(&token) {
            Fed::Done(token, None)
        } else {
            Fed::NoMatch(self)
        }
    }
}

impl<L, R, T> Builder for AndBuilder<L, R>
where
    L: Builder<Token = T>,
    R: Builder<Token = T>,
{
    type Token = T;
    type Output = (L::Output, R::Output);

    fn feed(self, token: Self::Token) -> Fed<Self, Self::Output, Self::Token> {
        match self {
            Self::Left(left, right) => match left.feed(token) {
                Fed::Ok(left) => Fed::Ok(Self::Left(left, right)),
                Fed::NoMatch(left) => Fed::NoMatch(Self::Left(left, right)),
                Fed::Done(left, None) => Fed::Ok(Self::Right(left, right)),
                Fed::Done(left, Some(token)) => Self::Right(left, right).feed(token),
            },
            Self::Right(left, right) => match right.feed(token) {
                Fed::Ok(right) => Fed::Ok(Self::Right(left, right)),
                Fed::NoMatch(right) => Fed::NoMatch(Self::Right(left, right)),
                Fed::Done(right, token) => Fed::Done((left, right), token),
            },
        }
    }
}

impl<L, R, O, T: Clone> Builder for OrBuilder<L, R>
where
    L: Builder<Output = O, Token = T>,
    R: Builder<Output = O, Token = T>,
{
    type Output = O;
    type Token = T;

    fn feed(self, token: T) -> Fed<Self, Self::Output, Self::Token> {
        match self {
            Self::Both(left, right) => match (left.feed(token.clone()), right.feed(token)) {
                (Fed::Done(left, token), _) => Fed::Done(left, token),
                (_, Fed::Done(right, token)) => Fed::Done(right, token),
                (Fed::Ok(left), Fed::Ok(right)) => Fed::Ok(OrBuilder::Both(left, right)),
                (Fed::NoMatch(left), Fed::NoMatch(right)) => {
                    Fed::NoMatch(OrBuilder::Both(left, right))
                }
                (Fed::Ok(left), Fed::NoMatch(right)) => Fed::Ok(Self::Left(left)),
                (Fed::NoMatch(left), Fed::Ok(right)) => Fed::Ok(Self::Right(right)),
            },
            Self::Left(left) => match left.feed(token) {
                Fed::Done(out, token) => Fed::Done(out, token),
                Fed::Ok(left) => Fed::Ok(Self::Left(left)),
                Fed::NoMatch(left) => Fed::NoMatch(Self::Left(left)),
            },
            Self::Right(right) => match right.feed(token) {
                Fed::Done(out, token) => Fed::Done(out, token),
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

    fn feed(self, token: T) -> Fed<Self, Self::Output, Self::Token> {
        match self.rule.feed(token) {
            Fed::Done(inner, token) => Fed::Done((self.fun)(inner), token),
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

impl<F, T: Clone, O, B> Builder for ListBuilder<F, B>
where
    B: Builder<Output = O, Token = T>,
    F: Fn() -> B + Copy,
{
    type Token = T;
    type Output = Vec<O>;

    fn feed(self, token: T) -> Fed<Self, Self::Output, Self::Token> {
        let ListBuilder {
            get_inner_rule,
            inner,
            mut list,
            in_progress,
        } = self;
        match inner.feed(token.clone()) {
            Fed::Done(item, token) => {
                list.push(item);
                let builder = ListBuilder {
                    inner: get_inner_rule(),
                    get_inner_rule,
                    list,
                    in_progress: false,
                };
                if let Some(token) = token {
                    builder.feed(token)
                } else {
                    Fed::Ok(builder)
                }
            }
            Fed::Ok(inner) => Fed::Ok(ListBuilder {
                get_inner_rule,
                inner,
                list,
                in_progress,
            }),
            Fed::NoMatch(inner) => {
                if in_progress {
                    Fed::NoMatch(ListBuilder {
                        get_inner_rule,
                        inner,
                        in_progress,
                        list,
                    })
                } else {
                    Fed::Done(list, Some(token))
                }
            }
        }
    }
}
