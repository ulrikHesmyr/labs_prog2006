fn main() {
    let my_string = String::from("hei = hihi");
    let tokens: Vec<&str> = my_string.split(" ").collect();
    
    for token in tokens {
        println!("{}", token);
    }

    
}

