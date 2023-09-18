mod util;
use std::collections::HashMap;

fn get_scores() -> HashMap<String, i32> {
	// A for Rock, B for Paper, and C for Scissors
	// The score for a single round is the score for the shape you selected 
	// (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome of the round 
	// (0 if you lost, 3 if the round was a draw, and 6 if you won).
    let mut map = HashMap::new();
    map.insert(String::from("A X"), 4);
	map.insert(String::from("A Y"), 8);
	map.insert(String::from("A Z"), 3);
	map.insert(String::from("B X"), 1);
	map.insert(String::from("B Y"), 5);
	map.insert(String::from("B Z"), 9);
	map.insert(String::from("C X"), 7);
	map.insert(String::from("C Y"), 2);
	map.insert(String::from("C Z"), 6);
    map
}

fn get_scores2() -> HashMap<String, i32> {
	// X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win	
    let mut map = HashMap::new();
    map.insert(String::from("A X"), 3);
	map.insert(String::from("A Y"), 4);
	map.insert(String::from("A Z"), 8);
	map.insert(String::from("B X"), 1);
	map.insert(String::from("B Y"), 5);
	map.insert(String::from("B Z"), 9);
	map.insert(String::from("C X"), 2);
	map.insert(String::from("C Y"), 6);
	map.insert(String::from("C Z"), 7);
    map
}

fn main() {
    let lines = util::read_lines("../data/t02.txt");
    let scores = get_scores();
    let sum = lines.iter()
    	.map(|line| scores.get(line).unwrap())
    	.sum::<i32>();

    println!("Part 1: {:?}", sum);

	let scores2 = get_scores2();
    let sum2 = lines.iter()
    	.map(|line| scores2.get(line).unwrap())
    	.sum::<i32>();

    println!("Part 1: {:?}", sum2);
}    
