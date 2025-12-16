use std::{fmt::Display, marker::PhantomData};

pub struct Puzzle<Input, Parts: PartsList<Input>> {
    pub number: u32,
    pub parser: fn(&str) -> Input,
    pub parts: Parts,
}

/// A heterogeneous list of part functions, similar to Haskell's OutputParameterisedFunctionList.
/// Each part can have a different output type, as long as it implements Display.
pub trait PartsList<Input> {
    fn run(&self, input: &Input) -> Vec<String>;
}

/// The empty list (nil)
pub struct PNil;

impl<Input> PartsList<Input> for PNil {
    fn run(&self, _input: &Input) -> Vec<String> {
        vec![]
    }
}

/// A cons cell: a function followed by more parts
pub struct PCons<Input, Output, F, Rest>
where
    F: Fn(&Input) -> Output,
    Output: Display,
    Rest: PartsList<Input>,
{
    pub head: F,
    pub tail: Rest,
    phantom: PhantomData<(Input, Output)>,
}

impl<Input, Output, F, Rest> PartsList<Input> for PCons<Input, Output, F, Rest>
where
    F: Fn(&Input) -> Output,
    Output: Display,
    Rest: PartsList<Input>,
{
    fn run(&self, input: &Input) -> Vec<String> {
        let mut results = vec![(self.head)(input).to_string() + "\n"];
        results.extend(self.tail.run(input));
        results
    }
}

/// Constructor for PCons - the /\ operator equivalent
pub fn cons<Input, Output, F, Rest>(head: F, tail: Rest) -> PCons<Input, Output, F, Rest>
where
    F: Fn(&Input) -> Output,
    Output: Display,
    Rest: PartsList<Input>,
{
    PCons {
        head,
        tail,
        phantom: PhantomData,
    }
}

/// parts![f1, f2, f3] = cons(f1, cons(f2, cons(f3, PNil)))
#[macro_export]
macro_rules! parts {
    () => { $crate::puzzle::PNil };
    ($f:expr $(,)?) => { $crate::puzzle::cons($f, $crate::puzzle::PNil) };
    ($f:expr, $($rest:expr),+ $(,)?) => { $crate::puzzle::cons($f, parts!($($rest),+)) };
}
