use crate::util;

#[derive(Debug)]
struct Equation {
    target: i64,
    items: Vec<i64>
}

fn parse_input(file_name: &str) -> Vec<Equation> {
    util::read_lines(file_name)
      .iter()
      .map(parse_line)
      .collect()
}

fn parse_line(line: &String) -> Equation {
   let items: Vec<i64> = line.split(|c: char| !c.is_ascii_digit())
      .filter(|s| !s.is_empty())
      .map(|s| s.parse().unwrap())
      .collect();
   Equation {target: items[0], items: items[1..].to_vec()}   
}

fn is_true(e: &&Equation) -> bool {
   let capacity = 2_i32.pow((e.items.len() - 1) as u32);
   let mut v: Vec<i64> = vec![0; capacity as usize];
   v[0] = e.items[0];
   let mut size = 1;
   for i in 1..e.items.len() {
      for j in 0..size {
         v[size + j] = v[j] * e.items[i];   
         v[j] = v[j] + e.items[i];   
      }
      size *= 2;
   }
   // println!("{:?}", v);
   for i in 0..size {
      if v[i] == e.target {
         return true;
      }
   }
   false
}

pub fn task1() {
   let cnt: i64 = parse_input("../data/t07.txt")
      .iter()
      .filter(is_true)
      .map(|e| e.target )
      .sum();
   println!("{:?}", cnt);
   // println!("{:?}", is_true(&&Equation {target: 292, items: vec![11, 6, 16, 20]}));
}

fn concat(a: i64, b: i64) -> i64 {
    let multiplier = 10_f64.powf((b as f64).log10().floor() + 1.0) as i64;
    a * multiplier + b
}

fn is_true2(e: &&Equation) -> bool {
   let capacity = 3_i32.pow((e.items.len() - 1) as u32);
   let mut v: Vec<i64> = vec![0; capacity as usize];
   v[0] = e.items[0];
   let mut size = 1;
   for i in 1..e.items.len() {
      for j in 0..size {
         v[size + j] = v[j] * e.items[i];
         v[2 * size + j] = concat(v[j], e.items[i]);
         v[j] = v[j] + e.items[i];
      }
      size *= 3;
   }
   // println!("{:?}", v);
   for i in 0..size {
      if v[i] == e.target {
         return true;
      }
   }
   false
}

pub fn task2() {
   let cnt: i64 = parse_input("../data/t07.txt")
      .iter()
      .filter(is_true2)
      .map(|e| e.target )
      .sum();
   println!("{:?}", cnt);
   // println!("{:?}", is_true(&&Equation {target: 292, items: vec![11, 6, 16, 20]}));
}