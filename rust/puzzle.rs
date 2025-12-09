pub struct Puzzle<Input, Output, const N: usize> {
    pub number: u32,
    pub parser: fn(&str) -> Input,
    pub parts: [fn(&Input) -> Output; N],
}
