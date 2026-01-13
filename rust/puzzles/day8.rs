use crate::puzzle::Puzzle;
use itertools::Itertools;
use nom::{
    Parser,
    character::complete::{char, newline, usize},
    combinator::eof,
    error::Error,
    multi::separated_list1,
    sequence::{separated_pair, terminated},
};
use partitions::PartitionVec;

pub const PUZZLE: Puzzle<(usize, Vec<V3>), 2> = Puzzle {
    number: 8,
    parser: |is_real_data, input| {
        terminated::<_, _, Error<&str>, _, _>(
            terminated(
                separated_list1(
                    newline,
                    separated_pair(usize, char(','), separated_pair(usize, char(','), usize))
                        .map(|(x, (y, z))| V3 { x, y, z }),
                ),
                newline,
            ),
            eof,
        )
        .map(|v| (if is_real_data { 1000 } else { 10 }, v))
        .parse(input)
        .unwrap()
        .1
    },
    parts: [
        // TODO parts take about 28s each on real data!
        // compared to 0.25 for Haskell
        // maybe I should be using a mutable DSU instead of cloning?
        // commit initial version first, as that could be quite a big refactor
        // if that is it, worth noting that persistent immutable version is obviously a lot nicer etc.
        // and that maybe laziness is helping in Haskell
        |(n, boxes)| {
            connect_boxes(boxes)
                .get(*n)
                .expect("not enough boxes")
                .1
                .all_sets()
                .map(|s| s.count())
                .sorted_by(|a, b| b.cmp(a))
                .take(3)
                .product::<usize>()
                .to_string()
        },
        |(_, boxes)| {
            // TODO it's a bit of a shame we can't fully chain things here
            // I mean we could, e.g. with the `pipe` crate
            // ask Belle?
            // note this might disappear if we used a mutable DSU
            let (pair, _) = connect_boxes(boxes)
                .into_iter()
                .take_while(|(_, ds)| ds.amount_of_sets() > 1)
                .last()
                .expect("sets never unified");
            (pair.0.x * pair.1.x).to_string()
        },
    ],
};

// TODO not fully checked
fn connect_boxes(boxes: &[V3]) -> Vec<((V3, V3), PartitionVec<V3>)> {
    let all_pairs: Vec<(V3, V3)> = all_unordered_pairs(boxes)
        .into_iter()
        .sorted_by_key(|(a, b)| distance_squared(a, b))
        .collect();
    let mut partition: PartitionVec<V3> = boxes.iter().copied().collect();
    all_pairs
        .into_iter()
        .map(|(a, b)| {
            let result = ((a, b), partition.clone());
            let a_idx = boxes.iter().position(|v| *v == a).unwrap();
            let b_idx = boxes.iter().position(|v| *v == b).unwrap();
            partition.union(a_idx, b_idx);
            result
        })
        .collect()
}

// TODO return iterator
fn all_unordered_pairs<T: Copy>(items: &[T]) -> Vec<(T, T)> {
    items
        .iter()
        .enumerate()
        .flat_map(|(i, x)| items[i + 1..].into_iter().map(|y| (*x, *y)))
        .collect()
}

// TODO library, surely?
// nalgebra looks standard, but has bad support for integer vectors
// see e.g. `distance_squared` requiring floats
// just given up on finding an alternative for now
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct V3 {
    x: usize,
    y: usize,
    z: usize,
}
fn distance_squared(a: &V3, b: &V3) -> usize {
    a.x.abs_diff(b.x).pow(2) + a.y.abs_diff(b.y).pow(2) + a.z.abs_diff(b.z).pow(2)
}
