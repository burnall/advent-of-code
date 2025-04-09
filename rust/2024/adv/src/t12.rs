use crate::util;
use std::collections::HashSet;

type Map = Vec<Vec<char>>;

type Point = (i32, i32);

type Region = HashSet<Point>;

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
