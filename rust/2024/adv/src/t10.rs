use crate::util;
use std::collections::HashSet;

type Map = Vec<Vec<i8>>;
type Point = (i32, i32);
type Trailheads = Vec<HashSet<Point>>;

fn parse_input(file_name: &str) -> Map {
    util::read_lines(file_name)
        .iter()
        .map(parse_line)
        .collect()
}

fn parse_line(s: &String) -> Vec<i8> {
    s.chars()
        .map(|ch| ch.to_digit(10).unwrap() as i8)
        .collect()
}

pub fn task1() {
    let map = parse_input("../data/t10.txt");
    
    let mut trailheads = find_zeroes(&map);
    //println!("{:?}", positions);
    for target in 1..10 {
        trailheads = step(&map, target, &trailheads);
        //println!("{:?} {:?}", target, trailheads);
    }
    let sum: i32 = trailheads.iter()
        .map(|positions| positions.len() as i32)
        .sum();
    println!("{:?}", sum);
}

const OFFSETS: [Point; 4] = [(0, 1), (1, 0), (0, -1), (-1, 0)];

fn get_nears((x, y): &Point, &(max_x, max_y): &Point) -> Vec<Point> {    
    OFFSETS.iter()
        .map(|(dx, dy)| (x + dx, y + dy))
        .filter(|&(nx, ny)| nx >= 0 && nx < max_x && ny >= 0 && ny < max_y)
        .collect() 
}

fn step(map: &Map, target: i8, trailheads: &Trailheads ) -> Trailheads {
    let max = (map[0].len() as i32, map.len() as i32);
    trailheads.iter()
        .map(|trailhead| trailhead.into_iter()
            .flat_map(|pos| get_nears(pos, &max).into_iter()
                .filter(|&(x, y)| map[y as usize][x as usize] == target))
            .collect::<HashSet<Point>>())
        .collect()
}

fn find_zeroes(map: &Map) -> Trailheads {
    let mut trailhead = Vec::new();
    for y in 0..map.len() {
        for x in 0..map[y].len() {
            if map[y][x] == 0 {
                trailhead.push(HashSet::from([(x as i32, y as i32)])); 
            }
        }    
    }
    trailhead
}

fn step2(map: &Map, target: i8, positions: &Vec<Point> ) -> Vec<Point> {
    let max = (map[0].len() as i32, map.len() as i32);
    positions.iter()
        .flat_map(|pos| get_nears(pos, &max).into_iter()
            .filter(|&(x, y)| map[y as usize][x as usize] == target)
            .collect::<Vec<Point>>())
        .collect()
}

fn find_zeroes2(map: &Map) -> Vec<Point> {
    let mut points = Vec::new();
    for y in 0..map.len() {
        for x in 0..map[y].len() {
            if map[y][x] == 0 {
                points.push((x as i32, y as i32)); 
            }
        }    
    }
    points
}

pub fn task2() {
    let map = parse_input("../data/t10.txt");
    
    let mut positions = find_zeroes2(&map);
    //println!("{:?}", positions);
    for target in 1..10 {
        positions = step2(&map, target, &positions);
        // println!("{:?} {:?}", target, positions);
    }
    println!("{:?}", positions.len());
}