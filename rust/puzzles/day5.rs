use crate::puzzle::Puzzle;
use itertools::Itertools;
use nom::{
    Parser,
    character::complete::{char, newline, usize},
    combinator::eof,
    error::Error,
    multi::{separated_list0, separated_list1},
    sequence::{separated_pair, terminated},
};

pub const PUZZLE: Puzzle<(Vec<Range>, Vec<usize>), 2> = Puzzle {
    number: 5,
    parser: |_, input| {
        terminated::<_, _, Error<&str>, _, _>(
            separated_pair(
                separated_list1(
                    newline,
                    separated_pair(usize, char('-'), usize)
                        .map(|(lower, upper)| Range { lower, upper }),
                ),
                (newline, newline),
                separated_list0(newline, usize),
            ),
            (newline, eof),
        )
        .parse(input)
        .unwrap()
        .1
    },
    parts: [
        |(ranges, vals)| {
            vals.iter()
                .filter(|v| ranges.iter().any(|r| r.contains(**v)))
                .count()
                .to_string()
        },
        |(ranges, _)| {
            let mut merged = Ranges::new();
            for r in ranges.iter().cloned().sorted_by_key(|r| r.lower) {
                merged.add(r);
            }
            merged.length().to_string()
        },
    ],
};

#[derive(Clone, Debug)]
pub struct Range {
    lower: usize,
    upper: usize,
}
impl Range {
    fn length(&self) -> usize {
        self.upper - self.lower + 1
    }
    fn contains(&self, n: usize) -> bool {
        n >= self.lower && n <= self.upper
    }
    fn extend(&mut self, upper: usize) {
        self.upper = upper
    }
}

struct Ranges(Vec<Range>);
impl Ranges {
    fn new() -> Self {
        Ranges(Vec::new())
    }
    fn add(&mut self, new: Range) {
        if let Some(last) = self.0.last_mut() {
            if last.contains(new.lower) {
                last.extend(last.upper.max(new.upper));
                return;
            }
        }
        self.0.push(new);
    }
    fn length(&self) -> usize {
        self.0.iter().map(|r| r.length()).sum()
    }
}
