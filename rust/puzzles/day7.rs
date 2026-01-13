use crate::puzzle::Puzzle;
use itertools::Itertools;
use nom::{
    Parser,
    branch::alt,
    character::complete::{char, newline},
    combinator::{eof, value},
    error::Error,
    multi::{many1, separated_list1},
    sequence::terminated,
};
use std::collections::{HashMap, HashSet};

pub const PUZZLE: Puzzle<(usize, Vec<HashSet<usize>>), 2> = Puzzle {
    number: 7,
    parser: |input| {
        terminated::<_, _, Error<&str>, _, _>(
            terminated(
                (
                    terminated(
                        many1(alt((value(false, char('.')), value(true, char('S'))))),
                        newline,
                    )
                    .map_opt(|v| v.into_iter().position(|b| b)),
                    separated_list1(
                        newline,
                        many1(alt((value(false, char('.')), value(true, char('^'))))),
                    )
                    .map(|rows| {
                        rows.into_iter()
                            .map(|row| {
                                row.into_iter()
                                    .enumerate()
                                    .filter(|(_, b)| *b)
                                    .map(|(i, _)| i)
                                    .collect()
                            })
                            .collect()
                    }),
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
        |(start, splitter_rows)| {
            let mut beams = HashSet::from([*start]);
            let mut count = 0;
            for splitters in splitter_rows {
                beams = beams
                    .iter()
                    .flat_map(|x| {
                        if splitters.contains(x) {
                            count += 1;
                            vec![x - 1, x + 1]
                        } else {
                            vec![*x]
                        }
                    })
                    .collect();
            }
            count.to_string()
        },
        |(start, splitter_rows)| {
            let mut beams = HashMap::from([(*start, 1)]);
            for splitters in splitter_rows {
                beams = beams
                    .into_iter()
                    .flat_map(|(x, n)| {
                        if splitters.contains(&x) {
                            vec![x - 1, x + 1]
                        } else {
                            vec![x]
                        }
                        .into_iter()
                        .map(move |x1| (x1, n))
                    })
                    .into_group_map()
                    .into_iter()
                    .map(|(k, vs)| (k, vs.into_iter().sum()))
                    .collect();
            }
            beams.values().sum::<usize>().to_string()
        },
    ],
};
