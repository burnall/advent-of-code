use crate::util;
use std::collections::HashMap;

pub fn task1() {
    let r: i64 = read_numbers().iter().map(|n| nth(*n, 2000)).sum();
    println!("Answer: {:?}", r);
}

pub fn task2() {
    let numbers = read_numbers();
    // let numbers = vec![1, 2, 3, 2024];
    let mut stat: HashMap<i64, HashMap<i64, i64>> = HashMap::new();
    for n in numbers {
        let mut queue = Queue::new(4);
        let mut curr = n;
        let mut price: i64 = curr % 10;
        for i in 0..2000 {
            let digit = (curr % 10) as i8;
            curr = next(curr);
            let new_digit = (curr % 10) as i8;
            let delta = new_digit - digit;
            price += delta as i64;
            queue.push(delta);
            if i > 2 {
                stat.entry(queue.hash())
                    .or_insert(HashMap::new())
                    .entry(n)
                    .or_insert(price);
            }
        }
    }

    let max = stat.values().map(|v| v.values().sum::<i64>()).max();
    // println!("Hash: {:?}", stat);
    println!("Max: {:?}", max);
}

fn read_numbers() -> Vec<i64> {
    util::read_lines("../data/t22.txt")
        .iter()
        .map(|line| line.trim().parse::<i64>().unwrap())
        .collect()
}

fn next(n: i64) -> i64 {
    let a = (n * 64 ^ n) % 16777216;
    let b = (a / 32 ^ a) % 16777216;
    (b * 2048 ^ b) % 16777216
}

fn nth(n: i64, count: i64) -> i64 {
    let mut result = n;
    for _ in 0..count {
        result = next(result);
    }
    result
}

#[derive(Debug)]
struct Queue {
    data: Vec<i8>,
    pos: usize,
}

impl Queue {
    fn new(size: usize) -> Self {
        Queue {
            data: vec![0; size],
            pos: 0,
        }
    }

    fn push(&mut self, n: i8) {
        self.data[self.pos] = n;
        self.pos = (self.pos + 1) % self.data.len();
    }

    fn hash(&self) -> i64 {
        let mut h: i64 = 0;
        for i in self.pos..self.data.len() {
            h = 20 * h + 10 + self.data[i] as i64;
        }
        for i in 0..self.pos {
            h = 20 * h + 10 + self.data[i] as i64;
        }
        h
    }
}
