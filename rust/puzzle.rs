use std::fmt::Display;

pub struct Puzzle<Input, Parts: PartsList<Input>> {
    pub number: u32,
    pub parser: fn(&str) -> Input,
    pub parts: Parts,
}

/// A trait for collections of part functions that can have heterogeneous output types.
/// Implemented for tuples of function pointers.
pub trait PartsList<Input> {
    fn run(&self, input: &Input) -> Vec<String>;
}

// Implementation for empty tuple (no parts)
impl<Input> PartsList<Input> for () {
    fn run(&self, _input: &Input) -> Vec<String> {
        vec![]
    }
}

// Implementation for 1-tuple
impl<Input, O1: Display> PartsList<Input> for (fn(&Input) -> O1,) {
    fn run(&self, input: &Input) -> Vec<String> {
        vec![(self.0)(input).to_string() + "\n"]
    }
}

// Implementation for 2-tuple (most common case for AoC)
impl<Input, O1: Display, O2: Display> PartsList<Input> for (fn(&Input) -> O1, fn(&Input) -> O2) {
    fn run(&self, input: &Input) -> Vec<String> {
        vec![
            (self.0)(input).to_string() + "\n",
            (self.1)(input).to_string() + "\n",
        ]
    }
}

// Implementation for 3-tuple (in case a puzzle has extra parts)
impl<Input, O1: Display, O2: Display, O3: Display> PartsList<Input>
    for (fn(&Input) -> O1, fn(&Input) -> O2, fn(&Input) -> O3)
{
    fn run(&self, input: &Input) -> Vec<String> {
        vec![
            (self.0)(input).to_string() + "\n",
            (self.1)(input).to_string() + "\n",
            (self.2)(input).to_string() + "\n",
        ]
    }
}
