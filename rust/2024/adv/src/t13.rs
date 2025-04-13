use crate::util;
use rational::Rational;
use regex::Regex;
use once_cell::sync::Lazy;

type Matrix = Vec<Vec<Rational>>;

const RE_BUTTON: Lazy<Regex> = Lazy::new(||Regex::new(r"Button .: X\+(\d+), Y\+(\d+)").unwrap());
const RE_PRIZE: Lazy<Regex> = Lazy::new(||Regex::new(r"Prize: X=(\d+), Y=(\d+)").unwrap());

fn read_matrixes(lines: &Vec<String>) -> Vec<Matrix> {
	let mut i = 0;
	let mut matrixes = vec![];
	while i < lines.len() {
		let cols = vec![parse_vec(&RE_BUTTON, &lines[i]), 
			parse_vec(&RE_BUTTON, &lines[i + 1]), 
			parse_vec(&RE_PRIZE, &lines[i + 2])];
		matrixes.push(get_matrix(&cols)); 
		i += 4;		
	}
	matrixes
}

fn get_matrix(cols: &Matrix) -> Matrix {
	let mut m: Vec<_> = (0..cols[0].len())
		.map(|_| vec![])
		.collect();

	for col in cols {
		for i in 0..col.len() {
			m[i].push(col[i]);
		}
	} 	
	m
}

fn parse_vec(regex: &Regex, text: &String) -> Vec<Rational> {
	// println!("{:?} {:?}", regex, text);
	let caps = regex.captures(text).unwrap();
	let x = caps.get(1).map_or(0, |m| m.as_str().parse::<i128>().unwrap());
	let y = caps.get(2).map_or(0, |m| m.as_str().parse::<i128>().unwrap());
	vec![Rational::new(x, 1), Rational::new(y, 1)]	
}

fn parse_input(file_name: &str) -> Vec<Matrix> {
    let lines = util::read_lines(file_name);
    read_matrixes(&lines)
}

/*
a1*x + b1*y = M
a2*x + b2*y = N 
  a1*x + a1*b2/a2*y = N*a1/a2

  (b1 - a1*b2/a2)y = M - N*a1/a2
*/
fn solve(m: &Matrix) -> Vec<Rational> {
	let d = m[0][1] - m[0][0]*m[1][1]/m[1][0];
	if d == 0 {
		return vec![];
	}
	let y = (m[0][2] - m[1][2] * m[0][0] / m[1][0]) / d;
	let x = (m[0][2] - m[0][1] * y) / m[0][0];
	vec![x, y]
}

fn get_cost(solution: &Vec<Rational>) -> i128 {
	if solution.is_empty() {
		-1
	} else if solution[0].denominator() == 1 && solution[1].denominator() == 1 {
		3 * solution[0].numerator() + solution[1].numerator()
	} else {
		0
	}

}

pub fn task1() {
    let ms = parse_input("../data/t13.txt");
    let sum: i128 = ms.iter()
    	.map(|m| get_cost(&solve(m)))
    	.sum();

    println!("{:?}", sum);
}

pub fn task2() {
	let mut data = parse_input("../data/t13.txt");
    let ms = tweak(&mut data);
    let sum: i128 = ms.iter()
    	.map(|m| get_cost(&solve(m)))
    	.sum();

    println!("{:?}", sum);
}

fn tweak(matrices: &mut Vec<Matrix>) -> &mut Vec<Matrix> {
	let delta = 10000000000000_i128;
	for m in matrices.iter_mut() {
		m[0][2] += delta;
		m[1][2] += delta;
	}	
	matrices
}
