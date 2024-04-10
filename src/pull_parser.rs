use std::marker::PhantomData;

use crate::Source;

pub trait Rule: Copy {
    type Token;
    type Output;
    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output>;

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

pub fn single<T, F: FnOnce(&T) -> bool>(fun: F) -> impl Rule<Token = T, Output = T> {
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

impl<T, F: FnOnce(&T) -> bool + Copy> Rule for Single<T, F> {
    type Token = T;
    type Output = T;

    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output> {
        if (self.0)(self.source.peek()) {
            Some(self.source.get())
        } else {
            None
        }
    }
}

impl<T, L, R> Rule for And<L, R>
where
    L: Rule<Token = T>,
    R: Rule<Token = T>,
{
    type Token = T;
    type Output = (L::Output, R::Output);

    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output> {
        let l = self.0.parse(input)?;
        let r = self.1.parse(input).unwrap();
        Some((l, r))
    }
}

impl<T, O, L, R> Rule for Or<L, R>
where
    L: Rule<Token = T, Output = O>,
    R: Rule<Token = T, Output = O>,
{
    type Token = T;
    type Output = O;

    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output> {
        self.0.parse(input).or_else(|| self.1.parse(input))
    }
}

impl<T, O, F, R> Rule for Map<R, F>
where
    R: Rule<Token = T>,
    F: FnOnce(R::Output) -> O + Copy,
{
    type Token = T;
    type Output = O;
    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output> {
        self.rule.parse(input).map(self.fun)
    }
}

impl<T, R> Rule for List<R>
where
    R: Rule<Token = T>,
{
    type Token = T;
    type Output = Vec<R::Output>;

    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output> {
        let mut out = vec![];
        while let Some(item) = self.rule.parse(input) {
            out.push(item)
        }
        Some(out)
    }
}
