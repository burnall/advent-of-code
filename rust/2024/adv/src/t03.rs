use crate::util;
use regex::Regex;
use regex::Captures;

fn parse_input(file_name: &str) -> String {
    util::read_lines(file_name).join("")
    //"a".to_string()
}

fn caps_to_int(caps: &Captures, name: &str) -> i32 {
    let s = caps.name(name).unwrap().as_str();
    s.parse::<i32>().unwrap()
}

pub fn task1() {
    let text = parse_input("../data/t03.txt");
    //println!("Result: {:?}", s);

    let re = Regex::new(r"mul\((?<a>\d+),(?<b>\d+)\)").unwrap();
    let res: i32 = re.captures_iter(&text)
        .map(|caps| (caps_to_int(&caps, "a"), caps_to_int(&caps, "b")))
        .map(|(a, b)| a * b)
        .sum();
    println!("{:?}", res);
    //for cap in re.captures_iter(&text) {
    //    println!("Hashtag: {}", &cap[1]); 
    //}
}

