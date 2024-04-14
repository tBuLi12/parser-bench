use core::fmt;
use std::{fmt::Debug, marker::PhantomData};

use crate::Source;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Edit<T> {
    Insert(T),
    Delete,
    Keep,
}

#[derive(PartialEq, Eq)]
pub struct Edits<T>(Vec<Edit<T>>);

impl<T: PartialEq> std::cmp::PartialOrd for Edits<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.len().partial_cmp(&other.0.len())
    }
}

impl<T: Eq> std::cmp::Ord for Edits<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.len().cmp(&other.0.len())
    }
}

struct EditsIter<T, EI: Iterator<Item = Edit<T>>, TI: Iterator<Item = T>> {
    tokens: TI,
    edits: EI,
}

impl<T, EI: Iterator<Item = Edit<T>>, TI: Iterator<Item = T>> Iterator for EditsIter<T, EI, TI> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.edits.next().unwrap_or(Edit::Keep) {
            Edit::Insert(token) => Some(token),
            Edit::Delete => {
                self.tokens.next();
                self.next()
            }
            Edit::Keep => self.tokens.next(),
        }
    }
}

impl<T: Default + Clone + Eq> Edits<T> {
    fn apply_to(self, iter: impl Iterator<Item = T>) -> impl Iterator<Item = T> {
        EditsIter {
            tokens: iter,
            edits: self.0.into_iter(),
        }
    }
}

pub trait Rule: Copy + std::fmt::Debug {
    type Token: Default + Clone + Eq;
    type Output;
    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output>;
    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
    ) -> Edits<Self::Token>;
    fn get_edits_no_follow(self, tokens: &[Self::Token]) -> Edits<Self::Token>;

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
    type Token: Default + Clone + Eq;
    type Output;

    fn get(self) -> impl Rule<Token = Self::Token, Output = Self::Output>;
}

impl<T: NamedRule + Copy + Debug> Rule for T {
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

    fn get_edits_no_follow(self, tokens: &[Self::Token]) -> Edits<Self::Token> {
        self.get().get_edits_no_follow(tokens)
    }
}

pub fn single<T: Default + Clone + Eq, F: FnOnce(&T) -> bool + Copy>(
    fun: F,
) -> impl Rule<Token = T, Output = T> {
    Single(fun, PhantomData)
}

struct Single<T, F>(F, PhantomData<fn(&T)>);
impl<T, F: Copy> Copy for Single<T, F> {}
impl<T, F: Clone> Clone for Single<T, F> {
    fn clone(&self) -> Self {
        Single(self.0.clone(), PhantomData)
    }
}

impl<T, F> std::fmt::Debug for Single<T, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Single")
    }
}

#[derive(Clone, Copy, Debug)]
struct And<L, R>(L, R);

#[derive(Clone, Copy, Debug)]
struct Or<L, R>(L, R);

#[derive(Clone, Copy)]
struct Map<R, F> {
    rule: R,
    fun: F,
}

impl<R: fmt::Debug, F> fmt::Debug for Map<R, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Map({:?})", self.rule)
    }
}

#[derive(Clone, Copy, Debug)]
struct List<R>(R);

impl<T: Default + Clone + Eq, F: FnOnce(&T) -> bool + Copy> Rule for Single<T, F> {
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
        eprintln!("+{:?} -> {:?}", self, follow);
        let del_count = tokens
            .iter()
            .position(|t| (self.0)(t))
            .unwrap_or(tokens.len());

        let dels: Edits<T> = Edits(vec![Edit::Delete; del_count]);
        let insert = Edits(vec![Edit::Insert(T::default())]);

        let deleted: Vec<T> = dels.apply_to(tokens.iter().cloned()).skip(1).collect();
        let inserted: Vec<T> = insert.apply_to(tokens.iter().cloned()).skip(1).collect();
        let del_edits = follow.get_edits_no_follow(&deleted);
        let insert_edits = follow.get_edits_no_follow(&inserted);
        eprintln!("-{:?}", self);
        std::cmp::min(del_edits, insert_edits)
    }

    fn get_edits_no_follow(self, tokens: &[Self::Token]) -> Edits<Self::Token> {
        eprintln!("+{:?}", self);
        let del_count = tokens
            .iter()
            .position(|t| (self.0)(t))
            .unwrap_or(tokens.len());

        let dels: Edits<T> = Edits(vec![Edit::Delete; del_count]);
        let insert = Edits(vec![Edit::Insert(T::default())]);

        eprintln!("-{:?}", self);
        std::cmp::min(dels, insert)
    }
}

impl<T: Default + Clone + Eq, L, R> Rule for And<L, R>
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

    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
    ) -> Edits<Self::Token> {
        eprintln!("+{:?} -> {:?}", self, follow);
        let e = self.0.get_edits(tokens, self.1.and(follow));
        eprintln!("-{:?}", self);
        e
    }

    fn get_edits_no_follow(self, tokens: &[Self::Token]) -> Edits<Self::Token> {
        eprintln!("+{:?}", self);
        let e = self.0.get_edits(tokens, self.1);
        eprintln!("-{:?}", self);
        e
    }
}

impl<T: Default + Clone + Eq, O, L, R> Rule for Or<L, R>
where
    L: Rule<Token = T, Output = O>,
    R: Rule<Token = T, Output = O>,
{
    type Token = T;
    type Output = O;

    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output> {
        self.0.parse(input).or_else(|| self.1.parse(input))
    }

    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
    ) -> Edits<Self::Token> {
        eprintln!("+{:?} -> {:?}", self, follow);
        let l_edits = self.0.get_edits(tokens, follow);
        let r_edits = self.1.get_edits(tokens, follow);
        eprintln!("-{:?}", self);
        std::cmp::min(l_edits, r_edits)
    }

    fn get_edits_no_follow(self, tokens: &[Self::Token]) -> Edits<Self::Token> {
        eprintln!("+{:?}", self);
        let l_edits = self.0.get_edits_no_follow(tokens);
        let r_edits = self.1.get_edits_no_follow(tokens);
        eprintln!("-{:?}", self);
        std::cmp::min(l_edits, r_edits)
    }
}

impl<T: Default + Clone + Eq, O, F, R> Rule for Map<R, F>
where
    R: Rule<Token = T>,
    F: FnOnce(R::Output) -> O + Copy,
{
    type Token = T;
    type Output = O;
    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output> {
        self.rule.parse(input).map(self.fun)
    }

    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
    ) -> Edits<Self::Token> {
        eprintln!("+{:?} -> {:?}", self, follow);
        let r = self.rule.get_edits(tokens, follow);
        eprintln!("-{:?}", self);
        r
    }

    fn get_edits_no_follow(self, tokens: &[Self::Token]) -> Edits<Self::Token> {
        eprintln!("+{:?}", self);
        let r = self.rule.get_edits_no_follow(tokens);
        eprintln!("-{:?}", self);
        r
    }
}

impl<T: Default + Clone + Eq, R> Rule for List<R>
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

    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
    ) -> Edits<Self::Token> {
        eprintln!("+{:?} -> {:?}", self, follow);
        if tokens.is_empty() {
            eprintln!("-{:?}", self);
            return Edits(vec![]);
        }

        let one = self.0.and(self).and(follow).get_edits_no_follow(tokens);

        let two = follow.get_edits_no_follow(tokens);

        eprintln!("-{:?}", self);
        std::cmp::min(one, two)
    }
    fn get_edits_no_follow(self, tokens: &[Self::Token]) -> Edits<Self::Token> {
        eprintln!("+{:?}", self);
        if tokens.is_empty() {
            eprintln!("-{:?}", self);
            return Edits(vec![]);
        }

        let delete_all = Edits(vec![Edit::Delete; tokens.len()]);
        let other = self.0.and(self).get_edits_no_follow(tokens);
        eprintln!("-{:?}", self);
        std::cmp::min(delete_all, other)
    }
}

// impl<T: Default + Clone + Eq> Rule for PhantomData<T> {
//     type Token = T;
//     type Output = ();

//     fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output> {
//         Some(())
//     }

//     fn get_edits(
//         self,
//         tokens: &[Self::Token],
//         follow: impl Rule<Token = Self::Token>,
//         Edits(vec![])
// }
