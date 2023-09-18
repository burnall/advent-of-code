mod util;

#[derive(Debug)]
struct Parts(Vec<char>, Vec<char>);

fn get_parts(s: String) -> Parts {
    Parts(s.chars(), s.chars())
}

fn find_common(c1: Vec<char>, c2: Vec<char>) -> char {
    'a'
}

fn to_priority(ch: char) {
    10
}

fn main() {
    let lines = util::read_lines("../data/t03.txt");
    let sum = lines.iter()
        .map(get_parts)
        .map(find_common)
        .map(to_priority)
        .sum::<i32>();

    println!("Part 1: {:?}", sum);    
}