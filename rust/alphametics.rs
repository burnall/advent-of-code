mod perm;

use std::collections::HashMap;
use std::collections::HashSet;

pub fn solve(input: &str) -> Option<HashMap<char, u8>> {
    todo!("Solve the alphametic {input:?}")
}

fn parse(input: &str) -> Vec<Vec<char>> {
   let words: Vec<&str> = input.split(|c: char| !c.is_ascii_uppercase())
      .filter(|&s| !s.is_empty())
      .collect();
   words.into_iter().map(|s: &str|s.chars().collect()).collect()
}

fn main() {
   let input = "SEND + MORE = MONEY";
   let words = parse(input);
   println!("{:?}", words); 
   let letters: Vec<char> = input.chars()
        .filter(|c: &char| c.is_ascii_uppercase())
        .collect::<HashSet<_>>()
        .into_iter()
        .collect();
   println!("{:?}", letters);   

   let mut digits: Vec[i32] = (0..10).collect(); 
   let mut solutions: Vec<&str> = vec![];
   perm::k_perm(&mut digits, letters.len(), 10, &|p| {

   });   
}

/*
SIX + SEVEN + SEVEN == TWENTY and LEET + CODE == POINT
"I + BB == ILL"
*/