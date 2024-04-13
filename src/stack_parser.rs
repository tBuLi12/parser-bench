struct Builder {
    data: *const u8,
    next: fn(*mut Vec<Builder>),
}

trait Rule {
    fn builder() -> Builder;
}

fn parse() {
    let mut stack = Vec::<Builder>::new();
}
