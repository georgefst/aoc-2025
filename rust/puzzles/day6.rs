use crate::puzzle::Puzzle;
use itertools::Itertools;
use nom::{
    Parser,
    branch::alt,
    character::complete::{char, newline, satisfy, space1},
    combinator::{eof, value},
    error::Error,
    multi::{many1, separated_list1},
    sequence::terminated,
};
use std::ops::{Add, Mul};

pub const PUZZLE: Puzzle<(Vec<Op>, Vec<Vec<Option<u8>>>), 2> = Puzzle {
    number: 6,
    parser: |input| {
        terminated::<_, _, Error<&str>, _, _>(
            (
                terminated(
                    separated_list1(
                        newline,
                        many1(alt((
                            satisfy(|c| c.is_digit(10)).map(|c| Some(c as u8 - '0' as u8)),
                            value(None, char(' ')),
                        ))),
                    ),
                    newline,
                ),
                terminated(
                    terminated(
                        separated_list1(
                            space1,
                            alt((value(Op::Multiply, char('*')), value(Op::Add, char('+')))),
                        ),
                        space1,
                    ),
                    newline,
                ),
            ),
            eof,
        )
        .map(|(grid, ops)| (ops, grid))
        .parse(input)
        .unwrap()
        .1
    },
    parts: [
        |(ops, grid)| {
            transpose(
                &grid
                    .iter()
                    .map(|row| {
                        row.group_somes()
                            .iter()
                            .filter(|v| !v.is_empty())
                            .map(|digits| digits_to_int(&digits))
                            .collect()
                    })
                    .collect::<Vec<_>>(),
            )
            .iter()
            .zip(ops.iter())
            .map(|(list, op)| op.apply_to_list(list))
            .sum::<usize>()
            .to_string()
        },
        |(ops, grid)| {
            transpose(grid)
                .iter()
                .map(|l| {
                    if l.iter().all(|x| x.is_none()) {
                        None
                    } else {
                        Some(digits_to_int(
                            &l.iter().filter_map(|x| *x).collect::<Vec<_>>(),
                        ))
                    }
                })
                .collect::<Vec<_>>()
                .group_somes()
                .iter()
                .zip(ops.iter())
                .map(|(list, op)| op.apply_to_list(list))
                .sum::<usize>()
                .to_string()
        },
    ],
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Add,
    Multiply,
}
impl Op {
    fn apply<N: Add<Output = N> + Mul<Output = N>>(self, a: N, b: N) -> N {
        match self {
            Op::Add => a + b,
            Op::Multiply => a * b,
        }
    }
    fn unit<N: From<u8>>(self) -> N {
        match self {
            Op::Add => N::from(0),
            Op::Multiply => N::from(1),
        }
    }
    fn apply_to_list(self, list: &[usize]) -> usize {
        list.iter().fold(self.unit(), |acc, &x| self.apply(acc, x))
    }
}

trait GroupSomes<T> {
    fn group_somes(&self) -> Vec<Vec<T>>;
}
impl<T: Clone> GroupSomes<T> for [Option<T>] {
    fn group_somes(&self) -> Vec<Vec<T>> {
        self.iter()
            .chunk_by(|x| x.is_some())
            .into_iter()
            .filter_map(|(is_some, group)| {
                is_some.then(|| group.filter_map(|x| x.clone()).collect())
            })
            .collect()
    }
}

fn transpose<T: Clone>(grid: &[Vec<T>]) -> Vec<Vec<T>> {
    let w = grid.iter().map(|r| r.len()).max().unwrap_or(0);
    (0..w)
        .map(|x| grid.iter().filter_map(|r| r.get(x).cloned()).collect())
        .collect()
}

fn digits_to_int(digits: &[u8]) -> usize {
    digits.iter().fold(0, |acc, &d| acc * 10 + (d as usize))
}
