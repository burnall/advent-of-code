use std::collections::HashMap;

use regex::Regex;

use crate::util::read_lines;

#[derive(Debug)]
struct Device {
    initial_wires: HashMap<String, u8>,
    inputs: HashMap<String, Vec<Gate>>,
}

#[derive(Debug, Copy, Clone)]
enum Oper {
    And,
    Or,
    Xor,
}

#[derive(Debug)]
struct Gate {
    in2: String,
    out: String,
    oper: Oper,
}

pub fn task1() {
    let device = Device::read_device();
    let wires = device.run();
    let mut z_wires = wires
        .keys()
        .filter(|wire| wire.starts_with('z'))
        .collect::<Vec<_>>();
    z_wires.sort();
    // println!("Wires: {:?}", z_wires);
    let answer = z_wires
        .iter()
        .rev()
        .fold(0_usize, |acc, wire| 2 * acc + (wires[*wire] as usize));
    println!("Answer: {:?}", answer);
}

impl Device {
    fn read_device() -> Self {
        let lines = read_lines("../data/t24.txt");
        let mut initial_wires = HashMap::new();
        let mut inputs = HashMap::new();
        let mut reading_initial_wires = true;
        let initial_wires_regex = Regex::new(r"^(\w+): (0|1)$").unwrap();
        let output_regex = Regex::new(r"^(\w+) (\w+) (\w+) -> (\w+)$").unwrap();

        for line in lines {
            if line.is_empty() {
                reading_initial_wires = false;
                continue;
            }
            if reading_initial_wires {
                let caps = initial_wires_regex.captures(&line).unwrap();
                let (name, value) = (
                    caps.get(1).unwrap().as_str().to_string(),
                    caps.get(2).unwrap().as_str().parse().unwrap(),
                );
                initial_wires.insert(name, value);
            } else {
                let caps = output_regex.captures(&line).unwrap();
                let (in1, oper, in2, out) = (
                    caps.get(1).unwrap().as_str(),
                    match caps.get(2).unwrap().as_str() {
                        "AND" => Oper::And,
                        "OR" => Oper::Or,
                        "XOR" => Oper::Xor,
                        _ => panic!("Unknown operation"),
                    },
                    caps.get(3).unwrap().as_str(),
                    caps.get(4).unwrap().as_str(),
                );
                // println!("Parsed: {} {:?} {} -> {}", in1, oper, in2, out);
                inputs.entry(in1.to_string()).or_insert(vec![]).push(Gate {
                    in2: in2.to_string(),
                    out: out.to_string(),
                    oper,
                });
                inputs.entry(in2.to_string()).or_insert(vec![]).push(Gate {
                    in2: in1.to_string(),
                    out: out.to_string(),
                    oper,
                });
            }
        }
        Device {
            initial_wires,
            inputs,
        }
    }

    fn run(&self) -> HashMap<String, u8> {
        let mut wires = self.initial_wires.clone();
        let mut active_wires = self.initial_wires.clone();

        while !active_wires.is_empty() {
            let mut new_active_wires = HashMap::new();
            for (wire, value) in &active_wires {
                if let Some(gates) = self.inputs.get(wire) {
                    for gate in gates {
                        if let Some(v2) = wires.get(&gate.in2) {
                            let new_value = match gate.oper {
                                Oper::And => value & v2,
                                Oper::Or => value | v2,
                                Oper::Xor => value ^ v2,
                            };
                            wires.insert(gate.out.clone(), new_value);
                            new_active_wires.insert(gate.out.clone(), new_value);
                        }
                    }
                }
            }
            active_wires = new_active_wires;
        }

        wires
    }
}
