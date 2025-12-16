mod puzzle;
mod puzzles;
use crate::puzzle::PartsList;
use puzzles::day1;
use puzzles::day2;
use std::fs;

fn main() {
    let puzzles: Vec<Box<dyn SomePuzzle>> = vec![
        Box::new(day1::puzzle()),
        Box::new(day2::puzzle()),
    ];

    [false, true].iter().for_each(|is_real_data| {
        let t = if *is_real_data { "real" } else { "examples" };
        println!("{}", t);
        puzzles.iter().for_each(|puzzle| {
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
impl<Input, Parts: PartsList<Input>> SomePuzzle for puzzle::Puzzle<Input, Parts> {
    fn number(&self) -> u32 {
        self.number
    }
    fn run(&self, s: &str) -> Vec<String> {
        let input = (self.parser)(s);
        self.parts.run(&input)
    }
}
