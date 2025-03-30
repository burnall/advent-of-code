use crate::util;

fn parse_input(file_name: &str) -> Vec<i8> {
    let v = util::read_lines(file_name)[0]
        .chars()
        .map(|ch| ch.to_digit(10).unwrap() as i8)
        .collect();
    v
}

fn interpret(disk_map: &Vec<i8>) -> Vec<i32> {
    let mut disk = Vec::with_capacity(disk_map.len() * 10);
    let mut id = 0;
    for i in 0..disk_map.len() {
        let block = if i % 2 == 0 { id += 1; id - 1} else { -1 };
        for _j in 0..(disk_map[i] as i32) {
            disk.push(block);  
        }   
    }
    disk
}

fn compact(disk: &mut Vec<i32>) {
   let mut len = disk.len();
   for i in 0..len {
      if disk[i] != -1 {
           continue;
      }
      for j in (0..len).rev() {
          if disk[j] != -1 {
              break;
          }  
          len -= 1;
      }
      if i >= len {
          break;
      }
      disk[i] = disk[len - 1];
      len -= 1;
   }
   disk.resize(len, 0);
}

fn crc(disk: &Vec<i32>) -> i64 {
    let mut crc = 0;
    for i in 0..disk.len() {
        let val = if disk[i] == -1 {0} else {disk[i]}; 
        crc += (val as i64) * (i as i64);
    }
    crc
}

pub fn task1() {
   let disk_map = parse_input("../data/t09.txt");
   //let disk_map = vec![1, 2, 3, 4, 5, 6];
   // let disk_map = vec![2,3,3,3,1,3,3,1,2,1,4,1,4,1,3,1,4,0,2];
   let mut disk = interpret(&disk_map);
   compact(&mut disk);
   println!("{:?}", crc(&disk));
}

fn get_file_length_back(disk: &Vec<i32>, pos: usize) -> i32 {
    let mut i = pos as i32 - 1;
    while i >= 0 {
        if disk[i as usize] != disk[pos] {
            break; 
        }
        i -= 1;
    }
    return pos as i32 - i;
}

fn get_free_spot(disk: &Vec<i32>, length: i32, end_pos: usize) -> i32 {
    //println!("fs {} {}", length, end_pos);
    let mut i = 0;
    let mut cnt = 0;
    while i < end_pos {
        if disk[i] != -1 {
            cnt = 0;
            i += 1;
            continue; 
        }
        cnt += 1;
        if cnt >= length {
            return i as i32; 
        }
        i += 1;
    }
    return -1;
}

fn move_file(disk: &mut Vec<i32>, src: usize, length: i32, dest: usize) {
    for i in 0..length {
        disk[dest - i as usize] = disk[src - i as usize];
        disk[src - i as usize] = -1; 
    }
}

fn compact2(disk: &mut Vec<i32>) {
   let len = disk.len();
   let mut pos = len as i32 - 1;

   while pos >= 0 {
       if disk[pos as usize] == -1 {
           pos -= 1; 
           continue;
       }
       let file_len = get_file_length_back(disk, pos as usize);
       let free_pos = get_free_spot(disk, file_len, (1 + pos as i32 - file_len) as usize);
       if free_pos != -1 {
           move_file(disk, pos as usize, file_len, free_pos as usize);
       }  
       pos -= file_len;
       //println!("{:?} {:?}", &disk, pos);
   }
}


pub fn task2() {
   let disk_map = parse_input("../data/t09.txt");
   //let disk_map = vec![2,3,3,3,1,3,3,1,2,1,4,1,4,1,3,1,4,0,2];
   let mut disk = interpret(&disk_map);
   compact2(&mut disk);
   println!("{:?}", crc(&disk));
}   
