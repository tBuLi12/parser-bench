use core::fmt;
use std::{fmt::Debug, marker::PhantomData};

use crate::Source;

pub trait Token: Clone + Eq + Debug {
    type Kind: Eq + Copy;

    fn kind(&self) -> Self::Kind;
    fn inserted(kind: Self::Kind) -> Self;
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Edit<T> {
    Insert(T),
    Delete,
    Keep,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Edits<T>(Vec<Edit<T>>);

impl<T: PartialEq + Token> std::cmp::PartialOrd for Edits<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.cost().partial_cmp(&other.cost())
    }
}

impl<T: Eq + Token> std::cmp::Ord for Edits<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.cost().cmp(&other.cost())
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

impl<T: Token> Edits<T> {
    fn apply_to(self, iter: impl Iterator<Item = T>) -> impl Iterator<Item = T> {
        EditsIter {
            tokens: iter,
            edits: self.0.into_iter(),
        }
    }

    fn cost(&self) -> usize {
        self.0.iter().filter(|&edit| *edit != Edit::Keep).count()
    }
}

pub trait Rule: Copy + std::fmt::Debug {
    type Token: Token;
    type Output;
    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output>;
    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
        inserts_remaining: usize,
    ) -> Edits<Self::Token>;
    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
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
    type Token: Token;
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
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        self.get().get_edits(tokens, follow, inserts_remaining)
    }

    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        self.get().get_edits_no_follow(tokens, inserts_remaining)
    }
}

// pub fn single<T: Token, F: FnOnce(&T) -> bool + Copy>(fun: F) -> impl Rule<Token = T, Output = T> {
//     Single(fun, PhantomData)
// }
pub fn single<T: Token>(kind: T::Kind) -> impl Rule<Token = T, Output = T> {
    Single(kind)
}

struct Single<T: Token>(T::Kind);
impl<T: Token> Copy for Single<T> {}
impl<T: Token> Clone for Single<T> {
    fn clone(&self) -> Self {
        Single(self.0.clone())
    }
}

impl<T: Token> std::fmt::Debug for Single<T> {
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

impl<T: Token> Rule for Single<T> {
    type Token = T;
    type Output = T;

    fn parse(self, input: &mut impl Source<Token = Self::Token>) -> Option<Self::Output> {
        if input.peek().kind() == self.0 {
            Some(input.get())
        } else {
            None
        }
    }

    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} -> {:?} : {:?}", self, follow, tokens);

        if tokens.len() == 0 {
            return Edits(vec![]);
        }

        if tokens[0].kind() == self.0 {
            let mut keep: Edits<T> = Edits(vec![Edit::Keep]);
            keep.0.extend(
                follow
                    .get_edits_no_follow(&tokens[1..], inserts_remaining)
                    .0
                    .into_iter(),
            );
            return keep;
        }

        let del_count = tokens
            .iter()
            .position(|t| t.kind() == self.0)
            .unwrap_or(tokens.len());

        let mut dels: Edits<T> = Edits(vec![Edit::Delete; del_count]);
        let mut insert = Edits(vec![Edit::Insert(T::inserted(self.0))]);

        let deleted: Vec<T> = dels
            .clone()
            .apply_to(tokens.iter().cloned())
            .skip(1)
            .collect();
        let inserted: Vec<T> = insert
            .clone()
            .apply_to(tokens.iter().cloned())
            .skip(1)
            .collect();
        let del_edits = follow.get_edits_no_follow(&deleted, inserts_remaining);
        dels.0.extend(del_edits.0.into_iter());

        if inserts_remaining > 0 {
            let insert_edits = follow.get_edits_no_follow(&inserted, inserts_remaining - 1);
            insert.0.extend(insert_edits.0.into_iter());
            //     eprintln!("-{:?}", self);
            return std::cmp::min(dels, insert);
        }

        // eprintln!("-{:?}", self);
        return dels;
    }

    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        if tokens.len() == 0 {
            return Edits(vec![]);
        }

        if tokens[0].kind() == self.0 {
            let keep: Edits<T> = Edits(vec![Edit::Keep]);
            return keep;
        }

        let del_count = tokens
            .iter()
            .position(|t| t.kind() == self.0)
            .unwrap_or(tokens.len());

        let dels: Edits<T> = Edits(vec![Edit::Delete; del_count]);

        if inserts_remaining > 0 {
            let insert = Edits(vec![Edit::Insert(T::inserted(self.0))]);
            return std::cmp::min(dels, insert);
        }

        // eprintln!("-{:?}", self);
        return dels;
    }
}

impl<T: Token, L, R> Rule for And<L, R>
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
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} -> {:?} : {:?}", self, follow, tokens);
        let e = self
            .0
            .get_edits(tokens, self.1.and(follow), inserts_remaining);
        // eprintln!("-{:?}", self);
        e
    }

    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} : {:?}", self, tokens);
        let e = self.0.get_edits(tokens, self.1, inserts_remaining);
        // eprintln!("-{:?}", self);
        e
    }
}

impl<T: Token, O, L, R> Rule for Or<L, R>
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
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} -> {:?} : {:?}", self, follow, tokens);
        let l_edits = self.0.get_edits(tokens, follow, inserts_remaining);
        let r_edits = self.1.get_edits(tokens, follow, inserts_remaining);
        // eprintln!("-{:?}", self);
        std::cmp::min(l_edits, r_edits)
    }

    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} : {:?}", self, tokens);
        let l_edits = self.0.get_edits_no_follow(tokens, inserts_remaining);
        let r_edits = self.1.get_edits_no_follow(tokens, inserts_remaining);
        // eprintln!("-{:?}", self);
        std::cmp::min(l_edits, r_edits)
    }
}

impl<T: Token, O, F, R> Rule for Map<R, F>
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
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} -> {:?} : {:?}", self, follow, tokens);
        let r = self.rule.get_edits(tokens, follow, inserts_remaining);
        // eprintln!("-{:?}", self);
        r
    }

    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} : {:?}", self, tokens);
        let r = self.rule.get_edits_no_follow(tokens, inserts_remaining);
        // eprintln!("-{:?}", self);
        r
    }
}

impl<T: Token, R> Rule for List<R>
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
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} -> {:?} : {:?}", self, follow, tokens);
        if tokens.is_empty() {
            //     eprintln!("-{:?}", self);
            return Edits(vec![]);
        }

        let one = self
            .0
            .and(self)
            .and(follow)
            .get_edits_no_follow(tokens, inserts_remaining);

        let two = follow.get_edits_no_follow(tokens, inserts_remaining);

        // eprintln!("-{:?}", self);
        std::cmp::min(one, two)
    }

    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} : {:?}", self, tokens);
        if tokens.is_empty() {
            //     eprintln!("-{:?}", self);
            return Edits(vec![]);
        }

        let delete_all = Edits(vec![Edit::Delete; tokens.len()]);
        let other = self
            .0
            .and(self)
            .get_edits_no_follow(tokens, inserts_remaining);
        // eprintln!("-{:?}", self);
        std::cmp::min(delete_all, other)
    }
}

// impl<T: Token> Rule for PhantomData<T> {
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
