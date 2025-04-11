use crate::util;
use std::collections::HashSet;

type Map = Vec<Vec<char>>;

type Point = (i32, i32);

type Region = HashSet<Point>;

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
enum Dir {
    Up, Right, Down, Left
}

type Side = (Point, Dir);


fn parse_input(file_name: &str) -> Map {
    util::read_lines(file_name)
        .iter()
        .map(|s| s.chars().collect())
        .collect()
}

pub fn task1() {
    let map = parse_input("../data/t12.txt");
    let regions = discover(&map);
    // println!("{:?}", regions);

    let value: i32 = regions.iter()
        .map(|r| perimeter(r) * area(r))
        .sum();

    println!("{:?}", value);
}

fn discover(map: &Map) -> Vec<Region> {
    let mut used = HashSet::new();
    let mut regions = vec![];

    for y in 0..map.len() {
        for x in 0..map[y].len() {
            let p = (x as i32, y as i32);
            if !used.contains(&p) {
                // println!("d {:?}", p);
                let region = discover_one(&p, map);
                used.extend(region.clone());
                regions.push(region);
            }
        }
    }

    regions
}

fn discover_one(&start_p: &Point, map: &Map) -> Region {
    let ch = map[start_p.1 as usize][start_p.0 as usize];
    let max = (map[0].len() as i32, map.len() as i32);

    let mut points: HashSet<Point> = HashSet::new();
    let mut leads: HashSet<Point> = HashSet::from([start_p]);
    
    while !leads.is_empty() {
        points = points.union(&leads).cloned().collect();   
        leads = leads.iter()
            .flat_map(|lead| get_nears(lead, &max).into_iter()
                .filter(|&np| map[np.1 as usize][np.0 as usize] == ch && !points.contains(&np))
                .collect::<HashSet<Point>>())
            .collect();
        // println!("do {:?} {:?}", start_p, leads);    
    }
    points
}

fn perimeter(region: &Region) -> i32 {
    let max = (100000, 100000);
    region.iter()
        .map(|p| 4 - get_nears(p, &max).into_iter()
            .filter(|near| region.contains(near))
            .count() as i32)
        .sum::<i32>()
}

fn area(region: &Region) -> i32 {
    region.len() as i32
}

const OFFSETS: [Point; 4] = [(0, 1), (1, 0), (0, -1), (-1, 0)];

fn get_nears((x, y): &Point, &(max_x, max_y): &Point) -> Vec<Point> {    
    OFFSETS.iter()
        .map(|(dx, dy)| (x + dx, y + dy))
        .filter(|&(nx, ny)| nx >= 0 && nx < max_x && ny >= 0 && ny < max_y)
        .collect() 
}

pub fn task2() {
    let map = parse_input("../data/t12.txt");
    let regions = discover(&map);

    let value: i32 = regions.iter()
        .map(|r| number_of_sides(r) * area(r))
        .sum();

    println!("{:?}", value);
}


const X_MOVES: [(i32, i32); 2] = [(-1, 0), (1, 0)];
const Y_MOVES: [(i32, i32); 2] = [(0, -1), (0, 1)];

fn number_of_sides(region: &Region) -> i32 {
    let mut used = HashSet::new();
    let mut count = 0;
    let sides = get_sides(region);

    for side in sides.iter() {
        if used.contains(side) {
            continue;
        }

        used.insert(side.clone());
        let &(p0, dir) = side;
        let moves = if dir == Dir::Left || dir == Dir::Right {Y_MOVES} else {X_MOVES};

        for (dx, dy) in moves {
            let (mut x, mut y) = p0;
            loop {
                (x, y) = (x + dx, y + dy); 
                if !sides.contains(&((x, y), dir)) {
                    break;
                }
                used.insert(((x, y), dir));
            }
        }
        count += 1;
    }

    count
}

fn get_sides(region: &Region) -> HashSet<Side> {
    region.iter()
        .flat_map(|p| get_nears2(p).into_iter()
            .filter(|(near, _)| !region.contains(near))
            .map(|(_, dir)| (*p, dir))
            .collect::<Vec<_>>())
        .collect()
}

const OFFSETS2: [Side; 4] = [((0, 1), Dir::Up), ((1, 0), Dir::Right), 
    ((0, -1), Dir::Down), ((-1, 0), Dir::Left)];

fn get_nears2((x, y): &Point) -> Vec<Side> {    
    OFFSETS2.iter()
        .map(|((dx, dy), dir)| ((x + dx, y + dy), *dir))
        .collect()
}
