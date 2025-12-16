use crate::puzzle::Puzzle;
use nom::{
    Parser,
    branch::alt,
    character::complete::{char, digit1, newline},
    combinator::{eof, map_res, value},
    error::Error,
    multi::separated_list1,
    sequence::{pair, terminated},
};

pub const PUZZLE: Puzzle<Vec<(Direction, i32)>, 2> = Puzzle {
    number: 1,
    parser: |input| {
        terminated(
            terminated(
                separated_list1(
                    newline,
                    pair(
                        alt((
                            value(Direction::L, char('L')),
                            value(Direction::R, char('R')),
                        )),
                        parse_int(),
                    ),
                ),
                newline,
            ),
            eof,
        )
        .parse(input)
        .unwrap()
        .1
    },
    parts: [
        |instructions| {
            let mut p = 50;
            let mut r = 0;
            for (d, i) in instructions {
                let (_, p1) = step(*i, *d, p);
                if p1 == 0 {
                    r += 1;
                }
                p = p1;
            }
            r.to_string()
        },
        |instructions| {
            let mut p = 50;
            let mut r = 0;
            for (d, i) in instructions {
                let (c, p1) = step(*i, *d, p);
                let c1 = match d {
                    Direction::R => c.abs(),
                    Direction::L => {
                        if p == 0 {
                            c.abs() - 1
                        } else if p1 == 0 {
                            c.abs() + 1
                        } else {
                            c.abs()
                        }
                    }
                };
                r += c1;
                p = p1;
            }
            r.to_string()
        },
    ],
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    L,
    R,
}

fn step(i: i32, d: Direction, p: i32) -> (i32, i32) {
    let p1 = match d {
        Direction::L => p - i,
        Direction::R => p + i,
    };
    (p1.div_euclid(100), p1.rem_euclid(100))
}

fn parse_int<'a>() -> impl Parser<&'a str, Output = i32, Error = Error<&'a str>> {
    map_res(digit1, |s: &str| s.parse::<i32>())
}
