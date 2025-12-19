use crate::puzzle::Puzzle;
use nom::{
    Parser,
    character::complete::{newline, satisfy},
    combinator::eof,
    error::Error,
    multi::{many1, separated_list1},
    sequence::terminated,
};

pub const PUZZLE: Puzzle<Vec<Vec<u8>>, 2> = Puzzle {
    number: 3,
    parser: |input| {
        terminated::<_, _, Error<&str>, _, _>(
            terminated(
                separated_list1(
                    newline,
                    many1(satisfy(|c| c.is_digit(10)).map(|c| c as u8 - '0' as u8)),
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
        |input| {
            input
                .iter()
                .map(|bank| {
                    digits_to_int(&max_batteries(2, &bank).expect("battery list too short"))
                })
                .sum::<usize>()
                .to_string()
        },
        |input| {
            input
                .iter()
                .map(|bank| {
                    digits_to_int(&max_batteries(12, &bank).expect("battery list too short"))
                })
                .sum::<usize>()
                .to_string()
        },
    ],
};

fn max_batteries(n: usize, v: &[u8]) -> Option<Vec<u8>> {
    let mut result = Vec::with_capacity(n);
    let mut remaining = n;
    let mut slice = v;
    while remaining > 0 {
        if slice.len() - remaining + 1 == 0 {
            return None;
        }
        let (b, i) = find_max(&slice[..slice.len() - remaining + 1]);
        result.push(b);
        remaining -= 1;
        slice = &slice[i + 1..];
    }
    Some(result)
}

fn find_max<A: Ord + Copy>(v: &[A]) -> (A, usize) {
    let (n, x) = v.iter().enumerate().rev().max_by_key(|x| x.1).unwrap();
    (*x, n)
}

fn digits_to_int(digits: &[u8]) -> usize {
    digits.iter().fold(0, |acc, &d| acc * 10 + (d as usize))
}
