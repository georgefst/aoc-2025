use crate::puzzle::Puzzle;
use nom::{
    Parser,
    branch::alt,
    character::complete::{char, newline},
    combinator::{eof, value},
    error::Error,
    multi::{many1, separated_list1},
    sequence::terminated,
};

pub const PUZZLE: Puzzle<Vec<Vec<InTile>>, 2> = Puzzle {
    number: 4,
    parser: |input| {
        terminated::<_, _, Error<&str>, _, _>(
            terminated(
                separated_list1(
                    newline,
                    many1(alt((
                        value(InTile::Empty, char('.')),
                        value(InTile::Roll, char('@')),
                    ))),
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
            let initial_rolls = count_rolls(&input);
            let final_rolls = count_rolls(&remove_accessible_rolls(&find_accessible(&input)));
            (initial_rolls - final_rolls).to_string()
        },
        |input| {
            let mut grid = input.clone();
            let initial_rolls = count_rolls(&grid);
            loop {
                let out_grid = find_accessible(&grid);
                if none_accessible(&out_grid) {
                    break;
                }
                grid = remove_accessible_rolls(&out_grid);
            }
            let final_rolls = count_rolls(&grid);
            (initial_rolls - final_rolls).to_string()
        },
    ],
};

#[derive(Clone, PartialEq, Eq)]
pub enum InTile {
    Empty,
    Roll,
}

#[derive(Clone, PartialEq, Eq)]
enum OutTile {
    Empty,
    Roll,
    Accessible,
}

fn find_accessible(grid: &Vec<Vec<InTile>>) -> Vec<Vec<OutTile>> {
    grid.iter()
        .enumerate()
        .map(|(y, row)| {
            row.iter()
                .enumerate()
                .map(|(x, t)| match t {
                    InTile::Empty => OutTile::Empty,
                    InTile::Roll => {
                        let mut neighbor_rolls = 0;
                        for dx in -1..=1 {
                            for dy in -1..=1 {
                                if dx == 0 && dy == 0 {
                                    continue;
                                }
                                if grid
                                    .get(y.wrapping_add_signed(dy))
                                    .and_then(|r| r.get(x.wrapping_add_signed(dx)))
                                    == Some(&InTile::Roll)
                                {
                                    neighbor_rolls += 1;
                                }
                            }
                        }
                        if neighbor_rolls < 4 {
                            OutTile::Accessible
                        } else {
                            OutTile::Roll
                        }
                    }
                })
                .collect()
        })
        .collect()
}

fn remove_accessible_rolls(grid: &Vec<Vec<OutTile>>) -> Vec<Vec<InTile>> {
    grid.iter()
        .map(|r| {
            r.iter()
                .map(|t| {
                    (|tile| match tile {
                        OutTile::Empty => InTile::Empty,
                        OutTile::Roll => InTile::Roll,
                        OutTile::Accessible => InTile::Empty,
                    })(t.clone())
                })
                .collect()
        })
        .collect()
}

fn none_accessible(grid: &Vec<Vec<OutTile>>) -> bool {
    !grid
        .iter()
        .any(|row| row.iter().any(|t| *t == OutTile::Accessible))
}

fn count_rolls(grid: &Vec<Vec<InTile>>) -> usize {
    grid.iter()
        .map(|row| row.iter().filter(|t| **t == InTile::Roll).count())
        .sum()
}
