use crate::util;
use std::collections::HashMap;
use std::hash::Hash;

fn parse_input(file_name: &str) -> Vec<i64> {
    util::read_lines(file_name)[0]
        .split(" ")
        .map(|s| s.parse::<i64>().unwrap())
        .collect()
}

fn count_digits(num: i64) -> i64 {
    (num.abs() as f64).log10().floor() as i64 + 1
}

fn blink(&n: &i64) -> Vec<i64> {
    if n == 0 { 
        vec![1] 
    } else {
        let cnt = count_digits(n);
        if cnt % 2 == 1 {
            vec![n * 2024]
        } else {
            let d = 10_i64.pow((cnt / 2) as u32);
            vec![n / d, n % d]
        }

    }    
}

pub fn task1() {
    let mut numbers = parse_input("../data/t11.txt");
    //let mut numbers = vec![0, 1, 10, 99, 999];
    //let mut numbers = vec![125, 17];
    
    (0..25).for_each(|_| {
        numbers = numbers.iter()
            .flat_map(blink)
            .collect();
    });
    println!("{:?}", numbers.len());
}

fn frequencies<T: Eq + Hash + Clone>(v: &Vec<T>) -> HashMap<T, i64> {
    let mut map = HashMap::new();
    for elem in v {
        *map.entry(elem.clone()).or_insert(0) += 1;
    }
    map
}

pub fn task2() {
    let numbers = parse_input("../data/t11.txt");
    let mut map = frequencies(&numbers);
    
    (0..75).for_each(|_| {
        let mut new_map = HashMap::new();
        for (n, count) in &map {
            for m in blink(&n) {
                *new_map.entry(m).or_insert(0) += count;
            }    
        }
        map = new_map
    });
    let count: i64 = map.values().sum();
    println!("{:?}", count);   
}