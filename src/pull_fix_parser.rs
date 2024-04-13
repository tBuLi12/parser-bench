use std::marker::PhantomData;

use crate::Source;

#[derive(Clone, Copy)]
enum Edit<T> {
    Insert(T),
    Delete,
}

struct Edits<T>(Vec<Edit<T>>);

impl<T> Edits<T> {
    fn apply_to(self, iter: impl Iterator<Item = T>) -> impl Iterator<Item = T> {}
}

pub trait Rule: Copy {
    type Token: Default;
    type Output;
    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output>;
    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
    ) -> Edits<Self::Token>;

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
    type Token: Default;
    type Output;

    fn get(self) -> impl Rule<Token = Self::Token, Output = Self::Output>;
}

impl<T: NamedRule + Copy> Rule for T {
    type Token = <T as NamedRule>::Token;
    type Output = <T as NamedRule>::Output;

    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output> {
        self.get().parse(input)
    }

    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
    ) -> Edits<Self::Token> {
        self.get().get_edits(tokens, follow)
    }
}

pub fn single<T, F: FnOnce(&T) -> bool + Copy>(fun: F) -> impl Rule<Token = T, Output = T> {
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

impl<T: Default, F: FnOnce(&T) -> bool + Copy> Rule for Single<T, F> {
    type Token = T;
    type Output = T;

    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output> {
        if (self.0)(input.peek()) {
            Some(input.get())
        } else {
            None
        }
    }

    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
    ) -> Edits<Self::Token> {
        let del_count = tokens
            .iter()
            .position(|t| (self.0)(t))
            .unwrap_or(tokens.len());
        let dels = Edits(vec![Edit::Delete; del_count]);
        let insert = Edits(vec![Edit::Insert(T::default())]);

        let deleted = dels.apply_to(tokens.iter());
        let inserted = insert.apply_to(tokens.iter());

        let del_edits = follow.get_edits(&deleted, None);
        let insert_edits = follow.get_edits(&inserted, None);

        std::cmp::min(del_edits, insert_edits)
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
        while let Some(item) = self.0.parse(input) {
            out.push(item)
        }
        Some(out)
    }
}
