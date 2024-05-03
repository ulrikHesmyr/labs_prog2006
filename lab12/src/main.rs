use std::io;

#[derive(Debug)]
enum ProgramError {
    InvalidOperation,
}

#[derive(Debug, Clone, Copy)]
enum Datatype {
    Int(i128),
    Float(f64),
}

fn tests() {
    let testings: Vec<(String, String)> = vec![
        ("3".to_string(), "3".to_string()),
        ("121231324135634563456363567".to_string(), "121231324135634563456363567".to_string()),
        ("1.0".to_string(), "1.0".to_string()),
        ("0.0".to_string(), "0.0".to_string()),
        ("-1".to_string(), "-1".to_string()),
        ("-1.1".to_string(), "-1.1".to_string()),
        ("False".to_string(), "False".to_string()),
        ("True".to_string(), "True".to_string()),
        ("[ [ ] [ ] ]".to_string(), "[[],[]]".to_string()),
        ("[ False [ ] True [ 1 2 ] ]".to_string(), "[False,[],True,[1,2]]".to_string()),
        ("\" [ so { not if ] and } \"".to_string(), "\"[ so { not if ] and }\"".to_string()),
        ("{ 20 10 + }".to_string(), "{ 20 10 + }".to_string()),
        ("[ { + } { 10 + } { 20 10 + } ]".to_string(), "[{ + },{ 10 + },{ 20 10 + }]".to_string()),
        ("1 1 +".to_string(), "2".to_string()),
        ("10 20 *".to_string(), "200".to_string()),
        ("20 2 div".to_string(), "10".to_string()),
        ("20 2 /".to_string(), "10.0".to_string()),
        ("1 1.0 +".to_string(), "2.0".to_string()),
        ("10 20.0 *".to_string(), "200.0".to_string()),
        ("20 2.0 div".to_string(), "10".to_string()),
        ("20.0 2.0 div".to_string(), "10".to_string()),
        ("False False &&".to_string(), "False".to_string()),
        ("False True ||".to_string(), "True".to_string()),
        ("False not".to_string(), "True".to_string()),
        ("True not".to_string(), "False".to_string()),
    ];

    
    for (input, output) in testings {
        let result = interpreter(&input);
        assert!(result == output, "FAIL\n- test: {}\n- result: {}\n- expected: {}", input, result, output);
    }

}

fn main() {
    let mut quit = false;
    while !quit {
        interpreter(&read_line());
        println!("Continue? (y/n)");
        let input = read_line();
        if input.trim() == "n" {
            quit = true;
        }
    }

    println!("Running tests...");
    tests();
}

fn interpreter(line : &String) -> String{
    let mut tokens: Vec<_> = line.trim().split(' ').rev().collect();
    
    let mut stack : Vec<Datatype> = Vec::new();

    while !tokens.is_empty() {
        let token = tokens.pop().unwrap();
        if let Some(datatype) = datatype(token) {
            stack.push(datatype);
        } else {
            //operation = token.to_string();
            let a = stack.pop().unwrap();
            let b = stack.pop().unwrap();
            let result : Result<Datatype, ProgramError> = match token {
                "+" => add(a, b),
                "-" => subtract(a, b),
                "*" => multiply(a, b),
                "/" => divide(a, b),
                _ => Err(ProgramError::InvalidOperation),
            };

            match result {
                Ok(value) => stack.push(value),
                Err(e) => println!("Error: {:?}", e),
            }
        }
    }

    if stack.len() != 1 {
        println!("Stack does not have a single value at the end.");
    } else {
        println!("{:?}", stack);
    }
    return match stack.pop() {
        Some(Datatype::Int(value)) => value.to_string(),
        Some(Datatype::Float(value)) => value.to_string(),
        None => "None".to_string(),
    };
}

fn read_line() -> String {
    let mut input = String::new();
    match io::stdin().read_line(&mut input) {
        Ok(_) => input,
        Err(error) => panic!("Error: {}", error), 
    }
}

fn datatype(token: &str) -> Option<Datatype> {
    if token.parse::<i128>().is_ok() {
        return Some(Datatype::Int(token.parse::<i128>().unwrap()));
    } else if token.parse::<f64>().is_ok() {
        return Some(Datatype::Float(token.parse::<f64>().unwrap()));
    }
    return None
}

fn add(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Int(a + b)),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Float(a + b)),
        (Datatype::Int(a), Datatype::Float(b)) => Ok(Datatype::Float(a as f64 + b)),
        (Datatype::Float(a), Datatype::Int(b)) => Ok(Datatype::Float(a + b as f64)),
    };
    result
}

fn subtract(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Int(a - b)),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Float(a - b)),
        (Datatype::Int(a), Datatype::Float(b)) => Ok(Datatype::Float(a as f64 - b)),
        (Datatype::Float(a), Datatype::Int(b)) => Ok(Datatype::Float(a - b as f64)),
    };
    result
}

fn multiply(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Int(a * b)),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Float(a * b)),
        (Datatype::Int(a), Datatype::Float(b)) => Ok(Datatype::Float(a as f64 * b)),
        (Datatype::Float(a), Datatype::Int(b)) => Ok(Datatype::Float(a * b as f64)),
    };
    result
}

fn divide(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Float((b as f64) / (a as f64))),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Float(b / a)),
        (Datatype::Int(a), Datatype::Float(b)) => Ok(Datatype::Float(b / a as f64)),
        (Datatype::Float(a), Datatype::Int(b)) => Ok(Datatype::Float(b as f64 / a)),
    };
    result
}
