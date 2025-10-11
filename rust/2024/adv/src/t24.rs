use std::collections::HashMap;

use regex::Regex;

use crate::util::read_lines;

#[derive(Debug)]
struct Device {
    initial_wires: HashMap<String, u8>,
    inputs: HashMap<String, Vec<Gate>>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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
    println!("Wires: {:?}", z_wires.len());
    let answer = z_wires
        .iter()
        .rev()
        .fold(0_usize, |acc, wire| 2 * acc + (wires[*wire] as usize));
    println!("Answer: {:?}", answer);
}

pub fn task2() {
    let device = Device::read_device();
    // println!("Wires: {:?}", wires.len());
    device.validate();
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
                            if !wires.contains_key(&gate.out) {
                                let new_value = match gate.oper {
                                    Oper::And => value & v2,
                                    Oper::Or => value | v2,
                                    Oper::Xor => value ^ v2,
                                };
                                wires.insert(gate.out.clone(), new_value);
                                new_active_wires.insert(gate.out.clone(), new_value);
                                println!(
                                    "Applying gate: {} {:?} {} -> {}",
                                    wire, gate.oper, gate.in2, gate.out
                                );
                            }
                        }
                    }
                }
            }
            active_wires = new_active_wires;
        }

        wires
    }

    /*
        x00 XOR y00 -> z00
    x00 AND y00 -> mtk

    x01 XOR y01 -> kvc
    x01 And y01 -> vfb
    mtk AND kvc -> jjp
    kvc XOR mtk -> z01
    vfb Or jjp -> cqp
         */
    fn validate(&self) {
        let mut idx = 0;
        let Device {inputs, initial_wires} = self;
        if !self.has_wire(&get_id('x', 0), &get_id('y', 0), Oper::Xor, &get_id('z', 0)) {
            println!("No z00");
        }
        let mut a = self.get_output(&get_id('x', 0), &get_id('y', 0), Oper::And);
        println!("00: {:?}", a);
        loop {
            idx += 1;
            if !initial_wires.contains_key(&get_id('x', idx)) {
                break;
            }
            let b = self.get_output(&get_id('x', idx), &get_id('y', idx), Oper::Xor);
            println!("\n{:02} [0]: {:?}", idx, b);
            let c = self.get_output(&get_id('x', idx), &get_id('y', idx), Oper::And);
            println!("{:02} [1]: {:?}", idx, c);
            let d = if a.is_some() && b.is_some() {
                 self.get_output(&a.unwrap().out, &b.unwrap().out, Oper::And)
            } else {
                None
            };
            println!("{:02} [2]: {:?}", idx, d);
            let e = if a.is_some() && b.is_some() {
                 self.get_output(&a.unwrap().out, &b.unwrap().out, Oper::Xor)
            } else {
                None
            };
            println!("{:02} [3]: {:?}", idx, e);
            a = if c.is_some() && d.is_some() {
                 self.get_output(&c.unwrap().out, &d.unwrap().out, Oper::Or)
            } else {
                None
            };
            println!("{:02} [4]: {:?}", idx, a);
        }
    }

    fn has_wire(&self, in1: &str, in2: &str, oper: Oper, out: &str) -> bool {
        match &self.inputs.get(in1) {
            Some(gates) => gates
                .iter()
                .any(|gate| gate.in2 == in2 && gate.oper == oper && gate.out == out),
            None => false,
        }
    }

    fn get_output(&self, in1: &str, in2: &str, oper: Oper) -> Option<&Gate> {
        let mut idx = 0;
        match &self.inputs.get(in1) {
            Some(gates) => gates
                .iter()
                .find(|gate| gate.in2 == in2 && gate.oper == oper),
            None => None,
        }
    }
}

fn get_id(prefix: char, id: usize) -> String {
    format!("{}{:02}", prefix, id)
}
