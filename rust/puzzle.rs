pub struct Puzzle<Input, const N: usize> {
    pub number: u32,
    pub parser: fn(bool, &str) -> Input,
    pub parts: [fn(&Input) -> String; N],
}
