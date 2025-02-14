mod perm;

use std::collections::HashMap;
use std::collections::HashSet;

pub fn solve(input: &str) -> Option<HashMap<char, u8>> {
    todo!("Solve the alphametic {input:?}")
}

fn parse(input: &str) -> Vec<Vec<char>> {
   input.split(|c: char| !c.is_ascii_uppercase())
      .filter(|&s| !s.is_empty())
      .map(|s| s.chars().collect())
      .collect()
}

fn get_variant(letters: &[char], numbers: &[i32]) -> HashMap<char, i32> {
   let mut v = HashMap::new();
   for i in 0..letters.len() {
      v.insert(letters[i], numbers[i]);
   }
   v
}

fn encode(word: &[char], variant: &HashMap<char, i32>) -> i32 {
   word.into_iter().fold(0, |acc, c| 10 * acc + variant.get(c).unwrap())
}

fn check_solution(words: &[Vec<char>], variant: &HashMap<char, i32>) -> bool {
   let sum = &words[..(words.len() - 1)]
       .into_iter()
       .map(|w| encode(&w, variant))
       .reduce(|a, b| a + b)
       .unwrap();

   let res = encode(&words[words.len() - 1], variant);    
   *sum == res
}

fn pp_solution(words: &Vec<Vec<char>>, variant: &HashMap<char, i32>) -> String {
   words.into_iter()
      .map(|w| w.into_iter().map(|ch| char::from_digit(*variant.get(ch).unwrap() as u32, 10).unwrap()).collect())
      .collect::<Vec<String>>()
      .join(" ")
}

fn main() {
   // let input = "SEND + MORE = MONEY";
   let input = "I + BB = ILL";
   let words = parse(input);
   println!("{:?}", words); 
   let letters: Vec<char> = input.chars()
        .filter(|c: &char| c.is_ascii_uppercase())
        .collect::<HashSet<_>>()
        .into_iter()
        .collect();
   println!("{:?}", letters);   

   let mut digits: Vec<i32> = (0..10).collect(); 
   //let mut solutions: Vec<String> = vec![];
   let cell = std::cell::RefCell::new(vec![]);
   perm::k_perm(&mut digits, letters.len(), 10, &|p| {
      let v = get_variant(&letters, p);
      println!("{:?}", v);
      if check_solution(&words, &v) {
         let mut solutions = cell.borrow_mut();
         solutions.push(pp_solution(&words, &v));
      }
   });   

   println!("{:?}", &*cell.borrow_mut());
   for sol in &*cell.borrow_mut() {
      println!("Solution {}", sol);
   }
}

/*
SIX + SEVEN + SEVEN == TWENTY and LEET + CODE == POINT
"I + BB == ILL"
*/