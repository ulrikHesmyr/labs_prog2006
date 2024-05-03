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
    let my_string = String::from("2 10 /");
    let tokens: Vec<_> = my_string.trim().split(' ').rev().collect();
    
    let mut arguments : Vec<Datatype> = Vec::new();
    let mut operation : String = String::new();

    for token in tokens.iter() {
        println!("{}", token);
        match datatype(token) {
            Some(datatype) => arguments.push(datatype),
            None => operation = token.to_string(),
        }
    }

    let result : Result<Datatype, ProgramError> = match operation.as_str() {
        "+" => add(arguments[0], arguments[1]),
        "-" => subtract(arguments[0], arguments[1]),
        "*" => multiply(arguments[0], arguments[1]),
        "/" => divide(arguments[0], arguments[1]),
        _ => Err(ProgramError::InvalidOperation),
    };
    
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
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Int(a / b)),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Float(a / b)),
        (Datatype::Int(_a), Datatype::Float(_b)) => Err(ProgramError::InvalidOperation),
        (Datatype::Float(_a), Datatype::Int(_b)) => Err(ProgramError::InvalidOperation),
    };
    result
}
