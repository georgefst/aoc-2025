mod puzzle;
mod puzzles;
use crate::puzzle::Puzzle;
use puzzles::day1;
use puzzles::day2;
use puzzles::day3;
use puzzles::day4;
use puzzles::day5;
use puzzles::day6;
use puzzles::day7;
use std::fs;
use std::time::Instant;

const PUZZLES: [&dyn SomePuzzle; 7] = [
    &day1::PUZZLE,
    &day2::PUZZLE,
    &day3::PUZZLE,
    &day4::PUZZLE,
    &day5::PUZZLE,
    &day6::PUZZLE,
    &day7::PUZZLE,
];

fn main() {
    [false, true].iter().for_each(|is_real_data| {
        let t = if *is_real_data { "real" } else { "examples" };
        println!("{}", t);
        PUZZLES.into_iter().for_each(|puzzle| {
            println!("  {}", puzzle.number());
            let input = fs::read_to_string(format!("../inputs/{}/{}", t, puzzle.number()))
                .expect("no input file");
            puzzle.with_parts(&input, &|n, run| {
                let expected =
                    fs::read_to_string(format!("../outputs/{}/{}/{}", t, puzzle.number(), n))
                        .expect("no golden file");
                let expected = expected.trim_end();
                let start = Instant::now();
                let output = run();
                let elapsed = start.elapsed();
                println!(
                    "    {}: {}",
                    n,
                    if expected == output {
                        format!("OK ({:?})", elapsed)
                    } else {
                        format!("expected {}, got {}", expected, output)
                    }
                );
            });
        });
    })
}

pub trait SomePuzzle {
    fn number(&self) -> u32;
    fn with_parts(&self, input: &str, f: &dyn Fn(usize, &dyn Fn() -> String));
}
impl<Input, const N: usize> SomePuzzle for Puzzle<Input, { N }> {
    fn number(&self) -> u32 {
        self.number
    }
    fn with_parts(&self, s: &str, f: &dyn Fn(usize, &dyn Fn() -> String)) {
        let input = (self.parser)(s);
        for (i, p) in self.parts.iter().enumerate() {
            f(i + 1, &|| p(&input));
        }
    }
}
