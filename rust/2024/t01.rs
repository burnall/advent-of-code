mod util;
use std::collections::HashMap;

fn read_pairs(file_name: &str) -> Vec<(i32, i32)> {
    util::read_lines(file_name)
        .into_iter()
        .map(|line| {
            let parts: Vec<i32> = line.split("   ")
                .map(|part| part.parse::<i32>().unwrap())
                .collect();
            (parts[0], parts[1])
        })
        .collect::<Vec<(i32, i32)>>()
}

fn extract(pairs: &Vec<(i32, i32)>, f: fn(&(i32, i32)) -> i32) -> Vec<i32> {
    let mut values: Vec<_> = pairs
        .into_iter()
        .map(f)
        .collect();
    values.sort();    
    values
}

fn part1(pairs: &Vec<(i32, i32)>) {
    let firsts: Vec<i32> = extract(&pairs, |p| p.0);
    let seconds: Vec<i32> = extract(&pairs, |p| p.1);

    let mut diff = 0;
    for i in 0..firsts.len() {
        diff += (firsts[i] - seconds[i]).abs();
    }    
    println!("Part 1: {:?}", diff);
}

fn frequencies(v: &Vec<i32>) -> HashMap<i32, i32> {
    v.iter()
        .fold(HashMap::new(), |mut map, val| {
              map.entry(*val)
                 .and_modify(|frq|*frq+=1)
                 .or_insert(1);
              map
          })
}

fn part2(pairs: &Vec<(i32, i32)>) {
    let firsts: Vec<i32> = extract(&pairs, |p| p.0);
    let seconds: Vec<i32> = extract(&pairs, |p| p.1);
    let freqs = frequencies(&seconds);

    let mut diff = 0;
    for el in firsts {
        diff += el * freqs.get(&el).unwrap_or(&0);
    }    
    println!("Part 2: {:?}", diff);
}

fn main() {
    let pairs = read_pairs("data/t01.txt");
    part1(&pairs);
    part2(&pairs);
}
