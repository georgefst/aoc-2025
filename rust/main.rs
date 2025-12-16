mod puzzle;
mod puzzles;
use crate::puzzle::Puzzle;
use puzzles::day1;
use puzzles::day2;
use std::fs;

const PUZZLES: [&dyn SomePuzzle; 2] = [&day1::PUZZLE, &day2::PUZZLE];

fn main() {
    [false, true].iter().for_each(|is_real_data| {
        let t = if *is_real_data { "real" } else { "examples" };
        println!("{}", t);
        PUZZLES.into_iter().for_each(|puzzle| {
            println!("  {}", puzzle.number());
            let input = fs::read_to_string(format!("../inputs/{}/{}", t, puzzle.number()))
                .expect("no input file");
            puzzle.run(&input).iter().zip(1..).for_each(|(output, n)| {
                let expected =
                    fs::read_to_string(format!("../outputs/{}/{}/{}", t, puzzle.number(), n))
                        .expect("no golden file");
                print!("    {}: ", n);
                if expected == *output {
                    println!("OK");
                } else {
                    println!(
                        "expected {}, got {}",
                        expected.trim_end(),
                        output.trim_end()
                    );
                };
            });
        });
    })
}

pub trait SomePuzzle {
    fn number(&self) -> u32;
    fn run(&self, input: &str) -> Vec<String>;
}
impl<Input, const N: usize> SomePuzzle for Puzzle<Input, { N }> {
    fn number(&self) -> u32 {
        self.number
    }
    fn run(&self, s: &str) -> Vec<String> {
        let input = (self.parser)(s);
        self.parts.map(|p| p(&input) + "\n").to_vec()
    }
}
