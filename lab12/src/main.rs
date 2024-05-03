use std::io;

#[derive(Debug)]
enum ProgramError {
    InvalidOperation,
}

#[derive(Debug, Clone, Copy)]
enum Datatype {
    Int(i32),
    Float(f32),
}

fn main() {
    //let my_string = String::from("20 2 / 3.0 4.0 * +");
    let my_string = read_line();
    let mut tokens: Vec<_> = my_string.trim().split(' ').rev().collect();
    
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

    println!("{:?}", stack);

    
    
}

fn read_line() -> String {
    let mut input = String::new();
    match io::stdin().read_line(&mut input) {
        Ok(_) => input,
        Err(error) => panic!("Error: {}", error), 
    }
}

fn datatype(token: &str) -> Option<Datatype> {
    if token.parse::<i32>().is_ok() {
        return Some(Datatype::Int(token.parse::<i32>().unwrap()));
    } else if token.parse::<f32>().is_ok() {
        return Some(Datatype::Float(token.parse::<f32>().unwrap()));
    }
    return None
}

fn add(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Int(a + b)),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Float(a + b)),
        (Datatype::Int(_a), Datatype::Float(_b)) => Err(ProgramError::InvalidOperation),
        (Datatype::Float(_a), Datatype::Int(_b)) => Err(ProgramError::InvalidOperation),
    };
    result
}

fn subtract(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Int(a - b)),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Float(a - b)),
        (Datatype::Int(_a), Datatype::Float(_b)) => Err(ProgramError::InvalidOperation),
        (Datatype::Float(_a), Datatype::Int(_b)) => Err(ProgramError::InvalidOperation),
    };
    result
}

fn multiply(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Int(a * b)),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Float(a * b)),
        (Datatype::Int(_a), Datatype::Float(_b)) => Err(ProgramError::InvalidOperation),
        (Datatype::Float(_a), Datatype::Int(_b)) => Err(ProgramError::InvalidOperation),
    };
    result
}

fn divide(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Float((b as f32) / (a as f32))),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Float(b / a)),
        (Datatype::Int(_a), Datatype::Float(_b)) => Err(ProgramError::InvalidOperation),
        (Datatype::Float(_a), Datatype::Int(_b)) => Err(ProgramError::InvalidOperation),
    };
    result
}
