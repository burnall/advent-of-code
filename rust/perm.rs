pub fn perm(a: &mut [i32], n: usize) {
    if n == 1 {
        println!("{:?}", a); 
        return;
    }
    perm(a, n - 1);
    
    for i in 0..(n - 1) {
        let j = if n % 2 == 0 {i} else {0}; 
        let temp = a[j];
        a[j] = a[n - 1];
        a[n - 1] = temp;
        
        perm(a, n - 1);
    }
}

fn main() {
    let mut a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]; 
    k_perm(&mut a, 4    , 10, &|a| println!("{:?}", a));
}

// https://cs.stackexchange.com/questions/161540/heaps-algorithm-for-k-permutations
pub fn k_perm(a: &mut [i32], K: usize, n: usize, f: &dyn Fn(&[i32])) {
    let N = a.len();
    if n == N - K {
        // println!("{:?}", &a[N-K..]); 
        f(&a[N-K..]);
        if N > K + 1 {
            if (N - K) % 2 == 0 || K == 2 {
                let temp = a[0];
                a[0] = a[N - K - 1];
                a[N - K - 1] = temp;    
            } else {
                let a0 = a[0];
                let ank3 = a[N - K - 3];
                let ank2 = a[N - K - 2];
                let ank1 = a[N - K - 1];
                for i in (1..(N - K - 3)).rev() {
                    a[i] = a[i-1];
                }
                a[0] = ank3;
                a[1] = ank2;
                a[N - K - 2] = ank1;
                a[N - K - 1] = a0;   
            }
        }    
        return;
    }

    k_perm(a, K, n - 1, f);
    
    for i in 0..(n - 1) {
        let j = if n % 2 == 0 {i} else {0}; 
        let temp = a[j];
        a[j] = a[n - 1];
        a[n - 1] = temp;
        
        k_perm(a, K, n - 1, f);
    }
}

