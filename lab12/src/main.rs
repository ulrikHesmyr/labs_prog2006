use std::io;
use std::fmt::Write;

#[derive(Debug)]
enum ProgramError {
    InvalidOperation,
    IncompleteList,
}

#[derive(Debug, Clone)]
enum Datatype {
    Int(i128),
    Float(f64),
    Boolean(bool),
    List(Vec<Datatype>),
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
        if let Some(dt) = datatype(token) {
            //Check if it is a list
            if let Datatype::List(_) = dt {
                let list_result = match list(&mut tokens) {
                    Ok(value) => value,
                    Err(e) => return format!("{:?}", e),
                };
                
                stack.push(list_result);
            } else {
                stack.push(dt);
            }
        } else {
            
            //Checking the input for operators and function-calls
            let result : Result<Datatype, ProgramError> = match token {
                "+" => add(stack.pop().unwrap(), stack.pop().unwrap()),
                "-" => subtract(stack.pop().unwrap(), stack.pop().unwrap()),
                "*" => multiply(stack.pop().unwrap(), stack.pop().unwrap()),
                "/" => divide(stack.pop().unwrap(), stack.pop().unwrap()),
                "&&" => and(stack.pop().unwrap(), stack.pop().unwrap()),
                "||" => or(stack.pop().unwrap(), stack.pop().unwrap()),
                "not" => not(stack.pop().unwrap()),
                "<" => less_than(stack.pop().unwrap(), stack.pop().unwrap()),
                ">" => larger_than(stack.pop().unwrap(), stack.pop().unwrap()),
                
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

    let stack_single_value = format_stack_item(stack.pop().unwrap());
    
    return stack_single_value
}

fn format_stack_item(stack_item : Datatype) -> String {
    match stack_item {
        Datatype::Int(value) => format!("{}", value),
        Datatype::Float(value) => format!("{:?}", value),
        Datatype::Boolean(value) => format!("{}", if value { "True" } else { "False" }),
        Datatype::List(list) => format!("[{}]", format_list(&list)),
    }
}

fn read_line() -> String {
    let mut input = String::new();
    match io::stdin().read_line(&mut input) {
        Ok(_) => input,
        Err(error) => panic!("Error: {}", error), 
    }
}

fn datatype(token: &str) -> Option<Datatype> {

    if token == "[" {
        return Some(Datatype::List(Vec::new()));
    } else if token.parse::<f64>().is_ok() && token.contains('.') {
        return Some(Datatype::Float(token.parse::<f64>().unwrap()));
    } else if token.parse::<i128>().is_ok() {
        return Some(Datatype::Int(token.parse::<i128>().unwrap()));
    } else if token == "False" || token == "True" {
        let bool_value = token.to_ascii_lowercase();
        return Some(Datatype::Boolean(bool_value.parse::<bool>().unwrap()));
    }
    return None
}

fn list(tokens: &mut Vec<&str>) -> Result<Datatype, ProgramError> {
    let mut list_ : Vec<Datatype> = Vec::new();
    let mut token = "";

    //Looping over all the list elements till the closing bracket
    while token != "]" {

        //If the token is a list, call the list function recursively
        token = tokens.pop().unwrap();
        if let Some(d) = datatype(token) {
            if let Datatype::List(_) = d {
                let result = match list(tokens) {
                    Ok(value) => value,
                    Err(e) => return Err(e),
                
                };
                
                list_.push(result);
            } else {
                list_.push(d); 
            }
        } else {
            if token != "]" {
                return Err(ProgramError::IncompleteList)
            }
            
        }
    }
    Ok(Datatype::List(list_))
}

fn larger_than(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Boolean(b > a)),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Boolean(b > a)),
        (Datatype::Int(a), Datatype::Float(b)) => Ok(Datatype::Boolean(b > a as f64)),
        (Datatype::Float(a), Datatype::Int(b)) => Ok(Datatype::Boolean((b as f64) > a)),
        _ => Err(ProgramError::InvalidOperation),
    };
    result
}

fn less_than(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Boolean(b < a)),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Boolean(b < a)),
        (Datatype::Int(a), Datatype::Float(b)) => Ok(Datatype::Boolean(b < a as f64)),
        (Datatype::Float(a), Datatype::Int(b)) => Ok(Datatype::Boolean((b as f64) < a)),
        _ => Err(ProgramError::InvalidOperation),
    };
    result
}

fn and(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Boolean(a), Datatype::Boolean(b)) => Ok(Datatype::Boolean(a && b)),
        _ => Err(ProgramError::InvalidOperation),
    };
    result
}

fn or(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Boolean(a), Datatype::Boolean(b)) => Ok(Datatype::Boolean(a || b)),
        _ => Err(ProgramError::InvalidOperation),
    };
    result
}

fn not(a : Datatype) -> Result<Datatype, ProgramError> {
    let result = match a {
        Datatype::Boolean(a) => Ok(Datatype::Boolean(!a)),
        _ => Err(ProgramError::InvalidOperation),
    };
    result
}

fn add(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Int(a + b)),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Float(a + b)),
        (Datatype::Int(a), Datatype::Float(b)) => Ok(Datatype::Float(a as f64 + b)),
        (Datatype::Float(a), Datatype::Int(b)) => Ok(Datatype::Float(a + b as f64)),
        _ => Err(ProgramError::InvalidOperation),
    };
    result
}

fn subtract(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Int(b - a)),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Float(b - a)),
        (Datatype::Float(a), Datatype::Int(b)) => Ok(Datatype::Float(b as f64 - a)),
        (Datatype::Int(a), Datatype::Float(b)) => Ok(Datatype::Float(b - a as f64)),
        _ => Err(ProgramError::InvalidOperation),
    };
    result
}

fn multiply(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Int(a * b)),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Float(a * b)),
        (Datatype::Int(a), Datatype::Float(b)) => Ok(Datatype::Float(a as f64 * b)),
        (Datatype::Float(a), Datatype::Int(b)) => Ok(Datatype::Float(a * b as f64)),
        _ => Err(ProgramError::InvalidOperation),
    };
    result
}

fn divide(a : Datatype, b : Datatype) -> Result<Datatype, ProgramError> {
    let result = match (a, b) {
        (Datatype::Int(a), Datatype::Int(b)) => Ok(Datatype::Float((b as f64) / (a as f64))),
        (Datatype::Float(a), Datatype::Float(b)) => Ok(Datatype::Float(b / a)),
        (Datatype::Int(a), Datatype::Float(b)) => Ok(Datatype::Float(b / a as f64)),
        (Datatype::Float(a), Datatype::Int(b)) => Ok(Datatype::Float(b as f64 / a)),
        _ => Err(ProgramError::InvalidOperation),
    };
    result
}

fn format_list(list : &Vec<Datatype>) -> String {
    let mut list_str = String::new();
    let length = list.len();
    for (i, item) in list.iter().enumerate() {
        if let Datatype::List(f) = item {
            write!(list_str, "[{}]", format_list(f)).unwrap();
        } else {
            write!(list_str, "{}", format_stack_item(item.clone())).unwrap();
        }
        if i < length - 1 {
            write!(list_str, ",").unwrap();
        }
    }
    return list_str
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
        ("20 10 <".to_string(), "False".to_string()),
    ("20 10 >".to_string(), "True".to_string()),
    ("20 10.0 >".to_string(), "True".to_string()),
    ("20.0 20.0 >".to_string(), "False".to_string()),
    ("10 10 ==".to_string(), "True".to_string()),
    ("10 10.0 ==".to_string(), "True".to_string()),
    ("True True ==".to_string(), "True".to_string()),
    ("True 40 40 == ==".to_string(), "True".to_string()),
    ("\" abba \" \" abba \" ==".to_string(), "True".to_string()),
    ("[ ] [ ] ==".to_string(), "True".to_string()),
    ("[ 1 2 ] [ 1 2 ] ==".to_string(), "True".to_string()),
    ("[ [ ] ] [ [ ] ] ==".to_string(), "True".to_string()),

    // Stack operations
    ("10 20 swap pop".to_string(), "20".to_string()),
    ("10 dup dup + swap pop".to_string(), "20".to_string()),
    ("10 20 swap dup + div".to_string(), "1".to_string()),

    // Length
    ("\" hello \" length".to_string(), "5".to_string()),
    ("\" hello world \" length".to_string(), "11".to_string()),
    ("[ 1 2 3 [ ] ] length".to_string(), "4".to_string()),
    ("{ 10 20 + } length".to_string(), "3".to_string()),

    // String parsing
    ("\" 12 \" parseInteger".to_string(), "12".to_string()),
    ("\" 12.34 \" parseFloat".to_string(), "12.34".to_string()),
    ("\" adam bob charlie \" words".to_string(), "[\"adam\",\"bob\",\"charlie\"]".to_string()),

    // Lists
    ("[ 1 2 3 ]".to_string(), "[1,2,3]".to_string()),
    ("[ 1 \" bob \" ]".to_string(), "[1,\"bob\"]".to_string()),
    ("[ 1 2 ] empty".to_string(), "False".to_string()),
    ("[ ] empty".to_string(), "True".to_string()),
    ("[ 1 2 3 ] head".to_string(), "1".to_string()),
    ("[ 1 2 3 ] length".to_string(), "3".to_string()),
    ("[ 1 2 3 ] tail".to_string(), "[2,3]".to_string()),
    ("1 [ ] cons".to_string(), "[1]".to_string()),
    ("1 [ 2 3 ] cons".to_string(), "[1,2,3]".to_string()),
    ("[ 1 2 ] [ ] append".to_string(), "[1,2]".to_string()),
    ("[ 1 ] [ 2 3 ] append".to_string(), "[1,2,3]".to_string()),
    ("[ 1 ] [ 2 3 ] cons".to_string(), "[[1],2,3]".to_string()),

    // List quotations
    ("[ 1 2 3 ] map { 10 * }".to_string(), "[10,20,30]".to_string()),
    ("[ 1 2 3 ] map { 1 + }".to_string(), "[2,3,4]".to_string()),
    ("[ 1 2 3 4 ] map { dup 2 > if { 10 * } { 2 * } }".to_string(), "[2,4,30,40]".to_string()),
    ("[ 1 2 3 4 ] each { 10 * } + + +".to_string(), "100".to_string()),
    ("[ 1 2 3 4 ] 0 foldl { + }".to_string(), "10".to_string()),
    ("[ 2 5 ] 20 foldl { div }".to_string(), "2".to_string()),
    ("[ \" 1 \" \" 2 \" \" 3 \" ] each { parseInteger } [ ] cons cons cons".to_string(), "[1,2,3]".to_string()),
    ("[ \" 1 \" \" 2 \" \" 3 \" ] each parseInteger [ ] 3 times cons".to_string(), "[1,2,3]".to_string()),
    ("[ 1 2 3 4 ] 0 foldl +".to_string(), "10".to_string()),
    ("[ 2 5 ] 20 foldl div".to_string(), "2".to_string()),

    // Assignments
    ("age".to_string(), "age".to_string()),
    ("age 10 := age".to_string(), "10".to_string()),
    ("10 age swap := age".to_string(), "10".to_string()),
    ("[ 1 2 3 ] list swap := list".to_string(), "[1,2,3]".to_string()),
    ("age 20 := [ 10 age ]".to_string(), "[10,20]".to_string()),

    // Functions
    ("inc { 1 + } fun 1 inc".to_string(), "2".to_string()),
    ("mul10 { 10 * } fun inc { 1 + } fun 10 inc mul10".to_string(), "110".to_string()),

    // Quotations
    ("{ 20 10 + } exec".to_string(), "30".to_string()),
    ("10 { 20 + } exec".to_string(), "30".to_string()),
    ("10 20 { + } exec".to_string(), "30".to_string()),
    ("{ { 10 20 + } exec } exec".to_string(), "30".to_string()),
    ("{ { 10 20 + } exec 20 + } exec".to_string(), "50".to_string()),

    // If statements
    ("True if { 20 } { }".to_string(), "20".to_string()),
    ("True if { 20 10 + } { 3 }".to_string(), "30".to_string()),
    ("10 5 5 == if { 10 + } { 100 + }".to_string(), "20".to_string()),
    ("False if { } { 45 }".to_string(), "45".to_string()),
    ("True if { False if { 50 } { 100 } } { 30 }".to_string(), "100".to_string()),

    // If without quotation
    ("True if 20 {}".to_string(), "20".to_string()),
    ("True if { 20 10 + } 3".to_string(), "30".to_string()),
    ("10 10 5 5 == if + { 100 + }".to_string(), "20".to_string()),
    ("False if { } 45".to_string(), "45".to_string()),
    ("True if { False if 50 100 } 30".to_string(), "100".to_string()),

    // Times
    ("1 times { 100 50 + }".to_string(), "150".to_string()),
    ("5 times { 1 } [ ] 5 times { cons } 0 foldl { + }".to_string(), "5".to_string()),
    ("5 times 1 [ ] 5 times cons 0 foldl +".to_string(), "5".to_string()),
    ("5 times { 10 } + + + +".to_string(), "50".to_string()),
    ("5 times 10 4 times +".to_string(), "50".to_string()),

    // Loop
    ("1 loop { dup 4 > } { dup 1 + } [ ] 5 times { cons }".to_string(), "[1,2,3,4,5]".to_string()),
    ("1 loop { dup 4 > } { dup 1 + } [ ] 5 times cons".to_string(), "[1,2,3,4,5]".to_string()),
    ("[ 1 ] loop { dup length 9 > } { dup head 1 + swap cons }".to_string(), "[10,9,8,7,6,5,4,3,2,1]".to_string()),

    ];

    
    for (index, (input, output)) in testings.iter().enumerate() {
        let result = interpreter(&input);
        assert!(result == *output, "FAIL on test {}\n- test: {}\n- result: {}\n- expected: {}", index, input, result, output);
    }

}