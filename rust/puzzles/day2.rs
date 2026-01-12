use crate::puzzle::Puzzle;
use nom::{
    Parser,
    character::complete::{char, newline, usize},
    combinator::eof,
    error::Error,
    multi::separated_list1,
    sequence::{separated_pair, terminated},
};

pub const PUZZLE: Puzzle<Vec<(usize, usize)>, 2> = Puzzle {
    number: 2,
    parser: |_, input| {
        terminated::<_, _, Error<&str>, _, _>(
            terminated(
                separated_list1(char(','), separated_pair(usize, char('-'), usize)),
                newline,
            ),
            eof,
        )
        .parse(input)
        .unwrap()
        .1
    },
    parts: [
        |input| {
            input
                .into_iter()
                .flat_map(|(l, u)| {
                    (*l..(u + 1)).flat_map(|n| if is_repetition_2(n) { Some(n) } else { None })
                })
                .sum::<usize>()
                .to_string()
        },
        |input| {
            input
                .into_iter()
                .flat_map(|(l, u)| {
                    (*l..(u + 1)).flat_map(|n| if is_repetition_n(n) { Some(n) } else { None })
                })
                .sum::<usize>()
                .to_string()
        },
    ],
};

fn is_repetition_2(n: usize) -> bool {
    let n = n.to_string();
    let l = n.len();
    let d = l / 2;
    let r = l % 2;
    if r == 0 { equal_chunks(&n, d) } else { false }
}

fn is_repetition_n(n: usize) -> bool {
    let n = n.to_string();
    let l = n.len();
    let d = l / 2;
    (1..(d + 1)).any(|i| equal_chunks(&n, i))
}

fn equal_chunks(n: &String, i: usize) -> bool {
    let chars = n.chars().collect::<Vec<char>>();
    let mut chunks = chars.chunks(i);
    match chunks.next() {
        None => true,
        Some(x) => chunks.all(|y| y == x),
    }
}
