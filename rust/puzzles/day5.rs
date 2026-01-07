use crate::puzzle::Puzzle;
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
    parser: |input| {
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
            let mut sorted_ranges = ranges.clone();
            sorted_ranges.sort_by_key(|r| r.lower);
            let mut merged = Ranges::new();
            for r in sorted_ranges {
                merged.add(r);
            }
            merged.total_length().to_string()
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
    fn extend(&self, upper: usize) -> Range {
        Range {
            lower: self.lower,
            upper: self.upper.max(upper),
        }
    }
}

struct Ranges(Vec<Range>);
impl Ranges {
    fn new() -> Self {
        Ranges(Vec::new())
    }
    fn add(&mut self, new: Range) {
        if self.0.is_empty() {
            self.0.push(new);
        } else {
            let first = &self.0[0];
            if first.contains(new.lower) {
                self.0[0] = first.extend(new.upper);
            } else {
                self.0.insert(0, new);
            }
        }
    }
    fn total_length(&self) -> usize {
        self.0.iter().map(|r| r.length()).sum()
    }
}
