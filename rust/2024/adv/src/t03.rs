use crate::util;
use regex::Regex;
use regex::Captures;

fn parse_input(file_name: &str) -> String {
    util::read_lines(file_name).join("")
}

fn caps_to_int(caps: &Captures, name: &str) -> i32 {
    let s = caps.name(name).unwrap().as_str();
    s.parse::<i32>().unwrap()
}

pub fn task1() {
    let text = parse_input("../data/t03.txt");
    let re = Regex::new(r"mul\((?<a>\d+),(?<b>\d+)\)").unwrap();
    
    let res: i32 = re.captures_iter(&text)
        .map(|caps| (caps_to_int(&caps, "a"), caps_to_int(&caps, "b")))
        .map(|(a, b)| a * b)
        .sum();
    println!("{:?}", res);
}

pub fn task2() {
    let text = parse_input("../data/t03.txt");
    let re = Regex::new(r"(?<oper>do\(\)|don't\(\))|mul\((?<a>\d+),(?<b>\d+)\)").unwrap();

    let res = re.captures_iter(&text)
        .fold((true, 0), |(enabled, sum), caps| {
           if caps.name("oper").is_none() {
               let val = if enabled {
                   caps_to_int(&caps, "a") * caps_to_int(&caps, "b")
               } else { 
                   0 
               };
               (enabled, sum + val) 
           } else if caps.name("oper").unwrap().as_str() == "do()" {
               (true, sum)
           } else {
               (false, sum)
           }
        });
    println!("{:?}", res);
}