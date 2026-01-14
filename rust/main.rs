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
use puzzles::day8;
use std::fs;
use std::time::Instant;

const PUZZLES: [&dyn SomePuzzle; 8] = [
    &day1::PUZZLE,
    &day2::PUZZLE,
    &day3::PUZZLE,
    &day4::PUZZLE,
    &day5::PUZZLE,
    &day6::PUZZLE,
    &day7::PUZZLE,
    &day8::PUZZLE,
];

// we do this instead of the Haskell approach because commenting out elements from the list/array literal is fiddly:
// - as it's a constant, a type annotation is required, and we need to update the type to reflect the new size
// - dead code checking is program-wide rather than by module, so we get loads of IDE warnings
// - rustfmt wants the whole literal to be on one line (when we have three puzzles), and we have no say over this
fn test_filter(_is_real_data: bool, p: &dyn SomePuzzle) -> bool {
    // !(_is_real_data && p.number() == 2)
    // !_is_real_data && (p.number() == 1 || p.number() == 5 || p.number() == 6)
    !_is_real_data &&
     p.number() == 8
}

fn main() {
    [false, true].iter().for_each(|is_real_data| {
        let t = if *is_real_data { "real" } else { "examples" };
        println!("{}", t);
        // PUZZLES.into_iter().for_each(|puzzle| {
            PUZZLES
                .into_iter()
                .filter(|x| test_filter(*is_real_data, *x))
                .for_each(|puzzle| {
            println!("  {}", puzzle.number());
            let input = fs::read_to_string(format!("../inputs/{}/{}", t, puzzle.number()))
                .expect("no input file");
            puzzle.with_parts(*is_real_data, &input, &|n, run| {
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
    fn with_parts(&self, is_real_data: bool, input: &str, f: &dyn Fn(usize, &dyn Fn() -> String));
}
impl<Input, const N: usize> SomePuzzle for Puzzle<Input, { N }> {
    fn number(&self) -> u32 {
        self.number
    }
    fn with_parts(&self, is_real_data: bool, s: &str, f: &dyn Fn(usize, &dyn Fn() -> String)) {
        let input = (self.parser)(is_real_data, s);
        for (i, p) in self.parts.iter().enumerate() {
            f(i + 1, &|| p(&input));
        }
    }
}
