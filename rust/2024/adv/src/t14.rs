use crate::util;
use regex::Regex;
use once_cell::sync::Lazy;
use std::io::{self};

const X: i64 = 101;
const Y: i64 = 103;

type Point = (i64, i64);
type Map = Vec<Vec<i64>>;

// #[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
#[derive(Debug)]
struct Robot {
    p: Point,
    v: Point
}

const RE_IN: Lazy<Regex> = Lazy::new(||Regex::new(r"p=(.+),(.+) v=(.+),(.+)").unwrap());

fn parse_input(file_name: &str) -> Vec<Robot> {
    util::read_lines(file_name).iter()
        .map(parse_line)
        .collect()
}

fn parse_line(text: &String) -> Robot {
    let caps = RE_IN.captures(text).unwrap();
    let ns: Vec<i64> = (1..=4)
        .map(|i| caps[i].to_string().parse::<i64>().unwrap())
        .collect();
    Robot {
        p: (ns[0], ns[1]),
        v: (ns[2], ns[3])
    }    
}

pub fn task1() {
    let robots = parse_input("../data/t14.txt");
    let quadrants: Vec<_> = robots.iter()
        .map(|robot| get_position(robot, 100))
        .map(|p| get_quadrant(&p))
        .filter(|&q| q > 0)
        .collect();
    let p: i64 = util::frequencies(&quadrants).values().product();

    println!("{:#?}", p);
}

fn get_position(robot: &Robot, ticks: i64) -> Point {
    // let Foo { x : x0, y: y0 } = faa;
    let Robot {p: (px, py), v: (vx, vy)} = robot; 
    ((px + (vx + X) * (ticks % X)) % X, (py + (vy + Y) * (ticks % Y)) % Y)
}

fn get_quadrant(&(x, y): &Point) -> i32 {
    if x == X / 2 || y == Y / 2 {
        return 0;
    }
    1 + if x < X / 2 { 0 } else { 1 }
      + if y < Y / 2 { 0 } else { 2 }
} 

pub fn task2() {
    let mut robots = parse_input("../data/t14.txt");
    infinite_draw(&mut robots);
}

fn infinite_draw(robots: &mut Vec<Robot>) {
    let mut map = vec![vec![0_i64; X as usize]; Y as usize];
    let mut count = 1;

    for robot in robots.iter() {
        let Robot {p: (px, py), ..} = *robot; 
        map[py as usize][px as usize] += 1;
    }    

    loop {
        for robot in robots.iter_mut() {
            let Robot {p: (px, py), v: (vx, vy)} = *robot; 
            let (nx, ny) = ((px + vx + X) % X, (py + vy + Y) % Y);
            robot.p = (nx, ny);

            map[py as usize][px as usize] -= 1;
            map[ny as usize][nx as usize] += 1;
        }    

        if is_tree(&map) {
            draw(&map, count);
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();
        }
        count += 1;

    }    
}

fn draw(map: &Map, count: i32) {
    println!("Iter #{:?}", count);
    for line in map {
        let text: String = line.iter().map(|&i| {
            if i == 0 {' '}
            else if i > 9 { '*' }
            else { (b'0' + i as u8) as char }
        }).collect();
        println!("{:?}", text);
    }   
}

// Not really a good search method - it was a small tree in rectangle border
fn is_tree(map: &Map) -> bool {
    let mut cnt = 0;
    for y in 0..Y {
        for x in 45..55 {
            if map[y as usize][x as usize] > 0 {
                cnt += 1;
            }
        }
    }
    return cnt > 70;
}