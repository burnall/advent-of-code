use crate::util;

type Map = Vec<Vec<i8>>;
type Point = (i32, i32);

fn parse_input(file_name: &str) -> Map {
    let v = util::read_lines(file_name)
        .iter()
        .map(parse_line)
        .collect();
    v
}

fn parse_line(s: &String) -> Vec<i8> {
    s.chars()
        .map(|ch| ch.to_digit(10).unwrap() as i8)
        .collect()
}

pub fn task1() {
   let map = parse_input("../data/t10.txt");
   println!("{:?}", &map);
}

fn step(map: &Map, target: i8, positions: &Vec<Point> ) -> Vec<Point> {
    vec![(1, 3)]
}