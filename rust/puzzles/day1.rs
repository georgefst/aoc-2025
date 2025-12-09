use crate::puzzle::Puzzle;

pub const PUZZLE: Puzzle<Vec<(Direction, i32)>, i32, 2> = Puzzle {
    number: 1,
    parser: |input| {
        input
            .lines()
            .filter(|line| !line.is_empty())
            .map(|line| {
                let d = match line.chars().next().unwrap() {
                    'L' => Direction::L,
                    'R' => Direction::R,
                    c => panic!("Unknown direction: {}", c),
                };
                let i: i32 = line[1..].parse().unwrap();
                (d, i)
            })
            .collect()
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
            r
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
            r
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
