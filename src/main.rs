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

enum Fed<B, T> {
    Done(T),
    Ok(B),
    NoMatch(B),
}

trait ParsePushBuilder: Sized {
    type Output;
    fn feed(self, token: Token) -> Fed<Self, Self::Output>;
}

trait ParsePush: Sized {
    type Builder: ParsePushBuilder;
    fn builder(&self) -> Self::Builder;

    fn and<Other: ParsePush>(self, other: Other) -> AndPush<Self, Other> {
        AndPush(self, other)
    }
    fn or<Other: ParsePush>(self, other: Other) -> OrPush<Self, Other> {
        OrPush(self, other)
    }
    fn list(self) -> ListPush<Self> {
        ListPush(self)
    }
    fn map<O, F: FnOnce(<Self::Builder as ParsePushBuilder>::Output) -> O>(
        self,
        fun: F,
    ) -> MapPush<Self, F> {
        MapPush { rule: self, fun }
    }
}

struct AndPush<L, R>(L, R);
struct OrPush<L, R>(L, R);
struct MapPush<R, F> {
    rule: R,
    fun: F,
}
struct ListPush<R>(R);

enum AndPushBuilder<L: ParsePushBuilder, R> {
    Left(L, R),
    Right(L::Output, R),
}

enum OrPushBuilder<L, R> {
    Both(L, R),
    Left(L),
    Right(R),
}

struct MapPushBuilder<R, F> {
    rule: R,
    fun: F,
}
struct ListPushBuilder<R: ParsePush> {
    inner_rule: R,
    inner: R::Builder,
    in_progress: bool,
    list: Vec<<R::Builder as ParsePushBuilder>::Output>,
}

impl<L, R> ParsePush for AndPush<L, R>
where
    L: ParsePush,
    R: ParsePush,
{
    type Builder = AndPushBuilder<L::Builder, R::Builder>;

    fn builder(&self) -> Self::Builder {
        AndPushBuilder::Left(self.0.builder(), self.1.builder())
    }
}

impl<L, R, LB, RB, O> ParsePush for OrPush<L, R>
where
    L: ParsePush<Builder = LB>,
    R: ParsePush<Builder = RB>,
    LB: ParsePushBuilder<Output = O>,
    RB: ParsePushBuilder<Output = O>,
{
    type Builder = OrPushBuilder<L::Builder, R::Builder>;

    fn builder(&self) -> Self::Builder {
        OrPushBuilder::Both(self.0.builder(), self.1.builder())
    }
}

impl<R, F, O> ParsePush for MapPush<R, F>
where
    R: ParsePush,
    F: FnOnce(<R::Builder as ParsePushBuilder>::Output) -> O + Copy,
{
    type Builder = MapPushBuilder<R::Builder, F>;

    fn builder(&self) -> Self::Builder {
        MapPushBuilder {
            rule: self.rule.builder(),
            fun: self.fun,
        }
    }
}

impl<R> ParsePush for ListPush<R>
where
    R: ParsePush + Copy,
{
    type Builder = ListPushBuilder<R>;

    fn builder(&self) -> Self::Builder {
        ListPushBuilder {
            inner_rule: self.0,
            inner: self.0.builder(),
            list: vec![],
            in_progress: false,
        }
    }
}

impl<L, R> ParsePushBuilder for AndPushBuilder<L, R>
where
    L: ParsePushBuilder,
    R: ParsePushBuilder,
{
    type Output = (L::Output, R::Output);

    fn feed(self, token: Token) -> Fed<Self, Self::Output> {
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

impl<L, R, O> ParsePushBuilder for OrPushBuilder<L, R>
where
    L: ParsePushBuilder<Output = O>,
    R: ParsePushBuilder<Output = O>,
{
    type Output = O;

    fn feed(self, token: Token) -> Fed<Self, Self::Output> {
        match self {
            Self::Both(left, right) => match (left.feed(token), right.feed(token)) {
                (Fed::Done(left), _) => Fed::Done(left),
                (_, Fed::Done(right)) => Fed::Done(right),
                (Fed::Ok(left), Fed::Ok(right)) => Fed::Ok(OrPushBuilder::Both(left, right)),
                (Fed::NoMatch(left), Fed::NoMatch(right)) => {
                    Fed::NoMatch(OrPushBuilder::Both(left, right))
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

impl<R, F, O> ParsePushBuilder for MapPushBuilder<R, F>
where
    R: ParsePushBuilder,
    F: FnOnce(R::Output) -> O + Copy,
{
    type Output = O;

    fn feed(self, token: Token) -> Fed<Self, Self::Output> {
        match self.rule.feed(token) {
            Fed::Done(inner) => Fed::Done((self.fun)(inner)),
            Fed::Ok(rule) => Fed::Ok(MapPushBuilder {
                rule,
                fun: self.fun,
            }),
            Fed::NoMatch(rule) => Fed::NoMatch(MapPushBuilder {
                rule,
                fun: self.fun,
            }),
        }
    }
}

impl<R> ParsePushBuilder for ListPushBuilder<R>
where
    R: ParsePush,
{
    type Output = Vec<<R::Builder as ParsePushBuilder>::Output>;

    fn feed(self, token: Token) -> Fed<Self, Self::Output> {
        let ListPushBuilder {
            inner_rule,
            inner,
            mut list,
            in_progress,
        } = self;
        match inner.feed(token) {
            Fed::Done(item) => {
                list.push(item);
                Fed::Ok(ListPushBuilder {
                    inner: inner_rule.builder(),
                    inner_rule,
                    list,
                    in_progress: false,
                })
            }
            Fed::Ok(inner) => Fed::Ok(ListPushBuilder {
                inner_rule,
                inner,
                list,
                in_progress,
            }),
            Fed::NoMatch(inner) => {
                if in_progress {
                    Fed::NoMatch(ListPushBuilder {
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

#[derive(Clone, Copy)]
struct KeywordParsePush(Keyword);
#[derive(Clone, Copy)]
struct IdentParsePush;
#[derive(Clone, Copy)]
struct PunctuationParsePush(Punctuation);

impl ParsePush for KeywordParsePush {
    type Builder = Self;
    fn builder(&self) -> Self::Builder {
        *self
    }
}
impl ParsePush for PunctuationParsePush {
    type Builder = Self;
    fn builder(&self) -> Self::Builder {
        *self
    }
}
impl ParsePush for IdentParsePush {
    type Builder = Self;
    fn builder(&self) -> Self::Builder {
        *self
    }
}
impl ParsePushBuilder for IdentParsePush {
    type Output = u32;

    fn feed(self, token: Token) -> Fed<Self, Self::Output> {
        match token {
            Token::Ident(val) => Fed::Done(val),
            _ => Fed::NoMatch(self),
        }
    }
}
impl ParsePushBuilder for KeywordParsePush {
    type Output = u32;

    fn feed(self, token: Token) -> Fed<Self, Self::Output> {
        match token {
            Token::Keyword(kw) if kw == self.0 => Fed::Done(0),
            _ => Fed::NoMatch(self),
        }
    }
}
impl ParsePushBuilder for PunctuationParsePush {
    type Output = u32;

    fn feed(self, token: Token) -> Fed<Self, Self::Output> {
        match token {
            Token::Punctuation(p) if p == self.0 => Fed::Done(0),
            _ => Fed::NoMatch(self),
        }
    }
}

trait Input {
    fn get(&mut self) -> Token;
    fn peek(&self) -> Token;
}

trait ParsePull: Sized {
    type Output;
    fn parse(&self, input: &mut impl Input) -> Option<Self::Output>;
    fn and<Other: ParsePull>(self, other: Other) -> AndPull<Self, Other> {
        AndPull(self, other)
    }
    fn or<Other: ParsePull>(self, other: Other) -> OrPull<Self, Other> {
        OrPull(self, other)
    }
    fn list(self) -> ListPull<Self> {
        ListPull(self)
    }
    fn map<O, F: FnOnce(Self::Output) -> O>(self, fun: F) -> MapPull<Self, F> {
        MapPull { rule: self, fun }
    }
}

struct AndPull<L, R>(L, R);
struct OrPull<L, R>(L, R);
struct MapPull<R, F> {
    rule: R,
    fun: F,
}
struct ListPull<R>(R);

impl<L, R> ParsePull for AndPull<L, R>
where
    L: ParsePull,
    R: ParsePull,
{
    type Output = (L::Output, R::Output);

    fn parse(&self, input: &mut impl Input) -> Option<Self::Output> {
        let l = self.0.parse(input)?;
        let r = self.1.parse(input).unwrap();
        Some((l, r))
    }
}

impl<L, R, O> ParsePull for OrPull<L, R>
where
    L: ParsePull<Output = O>,
    R: ParsePull<Output = O>,
{
    type Output = O;

    fn parse(&self, input: &mut impl Input) -> Option<Self::Output> {
        self.0.parse(input).or_else(|| self.1.parse(input))
    }
}

impl<R, F, O> ParsePull for MapPull<R, F>
where
    R: ParsePull,
    F: FnOnce(R::Output) -> O + Copy,
{
    type Output = O;

    fn parse(&self, input: &mut impl Input) -> Option<Self::Output> {
        self.rule.parse(input).map(self.fun)
    }
}

impl<R> ParsePull for ListPull<R>
where
    R: ParsePull,
{
    type Output = Vec<R::Output>;

    fn parse(&self, input: &mut impl Input) -> Option<Self::Output> {
        let mut out = vec![];
        while let Some(item) = self.0.parse(input) {
            out.push(item)
        }
        Some(out)
    }
}

struct KeywordParsePull(Keyword);
struct IdentParsePull;
struct PunctuationParsePull(Punctuation);

impl ParsePull for KeywordParsePull {
    type Output = u32;

    fn parse(&self, input: &mut impl Input) -> Option<Self::Output> {
        match input.peek() {
            Token::Keyword(kw) if kw == self.0 => {
                input.get();
                Some(0)
            }
            _ => None,
        }
    }
}
impl ParsePull for IdentParsePull {
    type Output = u32;

    fn parse(&self, input: &mut impl Input) -> Option<Self::Output> {
        match input.peek() {
            Token::Ident(val) => {
                input.get();
                Some(val)
            }
            _ => None,
        }
    }
}
impl ParsePull for PunctuationParsePull {
    type Output = u32;

    fn parse(&self, input: &mut impl Input) -> Option<Self::Output> {
        match input.peek() {
            Token::Punctuation(p) if p == self.0 => {
                input.get();
                Some(0)
            }
            _ => None,
        }
    }
}

struct CycledInput {
    i: usize,
    items: Vec<Token>,
}

impl Input for CycledInput {
    fn get(&mut self) -> Token {
        let token = self.items[self.i];
        self.i += 1;
        if self.i == self.items.len() {
            self.i = 0;
        }
        token
    }
    fn peek(&self) -> Token {
        self.items[self.i]
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

    let pull_fun = KeywordParsePull(Keyword::Fun)
        .and(PunctuationParsePull(Punctuation::Dot))
        .and(ListPull(IdentParsePull))
        .and(PunctuationParsePull(Punctuation::Dot));
    let push_fun = KeywordParsePush(Keyword::Fun)
        .and(PunctuationParsePush(Punctuation::Dot))
        .and(ListPush(IdentParsePush))
        .and(PunctuationParsePush(Punctuation::Dot));

    // for _ in 0..1_000_000 {
    //     pull_fun.parse(&mut input);
    // }

    'outer: for _ in 0..1_000_000 {
        let mut builder = push_fun.builder();
        for _ in 0..6 {
            match builder.feed(input.get()) {
                Fed::Done(_) => {
                    continue 'outer;
                }
                Fed::Ok(next) => builder = next,

                Fed::NoMatch(_) => {
                    panic!("error")
                }
            }
        }
    }
}
