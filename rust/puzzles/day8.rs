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
use std::collections::HashMap;

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
        // TODO factor out commonality between parts 1 and 2
        // first block is identical
        // second has some duplication...
        // maybe we can get half way back to something like the Haskell `connectBoxes`, as an iterator
        // it would have to be some sort of iterator where the mutability is internal, so we never need to clone
        // third is (a subset of) the part-specific code in the Haskell version
        |(n, boxes)| {
            // TODO can we do more in a single iteration of `boxes`, or at least do less copying?
            let index_map: HashMap<V3, usize> = boxes
                .iter()
                .copied()
                .enumerate()
                .map(|(i, v)| (v, i))
                .collect();
            let all_pairs = sorted_pairs(boxes);
            let mut partition: PartitionVec<V3> = boxes.iter().copied().collect();

            for (a, b) in all_pairs.iter().take(*n) {
                // TODO eugh, what to do about unsafe lookups?
                // API seems to be less high-level and type safe then the Haskell lib
                // what we really want is to be able to just pass two items (in our case, `V3`s) and union their sets
                // Haskell people are just better at API design...
                partition.union(index_map[a], index_map[b]);
            }

            partition
                .all_sets()
                .map(|s| s.count())
                .sorted_by(|a, b| b.cmp(a))
                .take(3)
                .product::<usize>()
                .to_string()
        },
        |(_, boxes)| {
            let index_map: HashMap<V3, usize> = boxes
                .iter()
                .copied()
                .enumerate()
                .map(|(i, v)| (v, i))
                .collect();
            let all_pairs = sorted_pairs(boxes);
            let mut partition: PartitionVec<V3> = boxes.iter().copied().collect();

            let mut last_pair = None;
            for (a, b) in all_pairs.iter() {
                if partition.amount_of_sets() == 1 {
                    break;
                }
                last_pair = Some((a, b));
                partition.union(index_map[a], index_map[b]);
            }

            let pair = last_pair.expect("sets never unified");
            (pair.0.x * pair.1.x).to_string()
        },
    ],
};

fn sorted_pairs(boxes: &[V3]) -> Vec<(V3, V3)> {
    all_unordered_pairs(boxes)
        .into_iter()
        .sorted_by_key(|(a, b)| distance_squared(a, b))
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct V3 {
    x: usize,
    y: usize,
    z: usize,
}
fn distance_squared(a: &V3, b: &V3) -> usize {
    a.x.abs_diff(b.x).pow(2) + a.y.abs_diff(b.y).pow(2) + a.z.abs_diff(b.z).pow(2)
}
