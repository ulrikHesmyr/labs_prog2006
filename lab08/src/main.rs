// Lab 08
use std::time::Instant;

const TEST_NUMBER_1 : u64 = 28;
const TEST_NUMBER_2 : u64 = 496;
// const TEST_NUMBER_3 : u64 = 2305843008139952128;
// const TEST_NUMBER_4 : u128 = 2658455991569831744654692615953842176;                              
// const TEST_NUMBER_5 : ??? = 191561942608236107294793378084303638130997321548169216;


fn main() {
    println!("======== Lab 08 fibonacci ========");
    println!("10th Fibonacci number:\nlazy:\t\t {}", fib(10));
    println!("loop:\t\t {}", fibonacci_l(10));
    println!("recursive:\t {}", fibonacci_r(10));

    test_fib();

    //println!("Is TEST_NUMBER_4 a perfect number? Result: {:?}", classify_number(TEST_NUMBER_4).unwrap());

    println!("======== Lab 08 numbers ========");
    for i in [TEST_NUMBER_1, TEST_NUMBER_2, 1000000000].iter() {
        let start_time = Instant::now();
        let result = classify_number(*i as u128).unwrap();
        let duration = start_time.elapsed();
        println!("The number {} is {:?}\n\tCalculated in: {} nano secs", *i, result, duration.as_nanos());
    }
    test_numbers();
}


// Test function for the Fibonacci implementations.
fn test_fib() {
    // Can we test for lazy 0th element? 
    // assert_eq!(fib(0), 0);
    assert_eq!(fibonacci_l(0), 0);
    assert_eq!(fibonacci_r(0), 0);
    for (q, a) in 
        [(1, 1), (2, 1), (3, 2), (4, 3), (5, 5), 
         (6, 8), (7, 13), (8, 21), (9, 34), (10, 55), (11, 89)].iter() {
        assert_eq!(fib(*q), *a);
        assert_eq!(fibonacci_l(*q as u64), *a);
        assert_eq!(fibonacci_r(*q as u64), *a);
    }
}

fn test_numbers() {
    assert_eq!(classify_number(0), None);
    
    for n in [1, 2, 3, 4, 5, 8, 9, 10, 11].iter() {
        assert_eq!(classify_number(*n), Some(NumberType::Deficient));
    }
    
    for n in [6, 28, 496, 8128, 33550336].iter() {
        assert_eq!(classify_number(*n), Some(NumberType::Perfect));
    }
    
    for n in [12, 18, 20].iter() {
        assert_eq!(classify_number(*n), Some(NumberType::Abundant));
    }

    // harder, longer cases
    // assert_eq!(classify_number(2305843008139952128), Some(NumberType::Perfect));
}


//ANSWEERS:
// 18446744073709551615 (aka 2^64) is the largest n we can calculate the fibonacci number for when using the u64 primitive data type.
// That is because u64 is an unsigned integer of 64 bits, which is only positive because it is unsigned, and the highest number is then 2^64

// Implementation of the Fibonacci sequence with a loop.
fn fibonacci_l(n: u64) -> u64 {

    let mut number = 0;

    let mut new_next;
    let mut current = 0;
    let mut next = 1;

    while number < n {

        new_next = next + current;
        current = next;
        next = new_next;

        number += 1;
    }

    return current;
}

// Implementation of the Fibonacci sequence with recursion.
fn fibonacci_r(n: u64) -> u64 {
    if n == 0 || n == 1 {
        return n; 
    }
    
    return fibonacci_r(n-1) + fibonacci_r(n-2);
    
}



// ========= Lab 08 numbers =========


//Aliquot sum: Using an improved version of exhaustive trial divison (dividing n by 1..n)
// 7x raskere enn funksjonen under
// Used this source to learn about proper divisors of a number: https://www.math.uh.edu/~minru/web/divis2.html
fn aliquot(n: u128) -> u128 {

    if n == 0 || n == 1 {
        return 0;
    }

    //Starting sum at 1 and for loop at 2 because all numbers of n has 1 as proper divisor
    let mut sum = 1;

    for i in 2..(((n as f64).sqrt() as u128)+1) { //From 2 till sqrt(n)+1

        //Checking if i is proper divisor && avoiding adding same divisor twice
        if n % i == 0{

            //Does not add the second divisor of n if n is a perfect square
            if n/i != i {
                sum += i + (n/i);
            } else {
                sum += i;
            }
        }
    }

    return sum;
}

//Basic aliquot
// fn aliquot(n: u128) -> u128 {
//     let mut sum = 0;

//     for i in 1..n {
//         if n % i == 0 {
//             sum += i;
//         }
//     }

//     return sum;
// }

// Implementation of the number type.
#[derive(Debug, PartialEq, Eq)]
enum NumberType {
    Perfect,
    Deficient,
    Abundant,
}

// Implementation of number classification.
//Why do we use u128 here? Why not u64? What is the performance impact?
fn classify_number(n: u128) -> Option<NumberType> {
    
    if n == 0 {
        return None;
    }
    
    let numbertype;
    let aliquot_sum = aliquot(n);

    if n == aliquot_sum {
        numbertype = NumberType::Perfect;
    } else if aliquot_sum < n {
        numbertype = NumberType::Deficient;
    } else {
        numbertype = NumberType::Abundant;
    }

    return Some(numbertype);
}



// ========= Lab 08 fibonacci lazy =========



// Implementation of the Fibonacci sequence with the lazy iterator
// Please study the code below and think of answers for the following questions:
// 1. What is the purpose of the `Fibs` struct? For object-oriented data container
// 2. What is the purpose of the `new` function? Purpose is to define the first two Fibonacci numbers on initialization
// 3. What is the purpose of the `Iterator` trait? It is to implement the Fibs abstract datatype to be compatible with all methods that apply to the Iterator trait
// 4. What is the purpose of the `next` function? The next function is the implementation of the Iterator::next() method for the Fibs abstract datatype
// 5. What is the purpose of the `fib` function? The fib function finds the n-th number of the fibonacci sequence, just like the two other fibonacci functions, but another paradigm/approach (object-oriented (???) it is not a method, but could be even though it is not necessary)

fn fib(n: usize) -> u64 {
    let fibs = Fibs::new();
    *fibs.take(n)
    .collect::<Vec<u64>>()
    .last()
    .unwrap()
}

struct Fibs {
    curr: u64,
    next: u64,
}

impl Fibs {
    fn new() -> Fibs {
        Fibs { curr: 0, next: 1 }
    }
}

impl Iterator for Fibs {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        let new_next = self.curr + self.next;
        self.curr = self.next;
        self.next = new_next;
        Some(self.curr)
    }
}
