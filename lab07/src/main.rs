// Lab 07

use std::collections::HashMap;

fn main() {
    test_acronym();
    //
    test_reverse();
//
    test_find_most_frequent_bird_count();
    test_find_most_frequent_bird(find_most_frequent_bird);
    test_find_most_frequent_bird(find_most_frequent_bird_no_order);
//
    test_find_most_frequent_bird_with_order();
    test_find_most_frequent_bird_no_order();


}


fn reverse(input: &str) -> String {
    // TODO: implement the function
    let mut reversed_string = String::new();

    //Gjør stringen akksessible med indekser ved å prosessere den som bytes
    let mut text = input.chars().collect::<Vec<_>>();

    while let Some(letter) = text.pop() {
        reversed_string.push(letter);
    }

    reversed_string
}


fn acronym(phrase: &str) -> String {
    // TODO: implement the function
    let mut new_acronym = String::new();
    let separators : &[char; 5] = &[' ', '\t', '|', '_', '-'];
    let mut current : char;
    
    let mut current_word = String::new();
    let words = phrase.as_bytes();
    let mut n = 0;
    
    for letter in words {

        current = *letter as char;

        if separators.contains(&current) || n == words.len()-1 {
            
            //boolean variable to check wether an acronym
            let mut valid = false;

            //boolean variable to include the first letter
            let mut first_letter = false;

            
            //vector to contain possible words for our new_acronym
            let mut potential_letters = String::new();
            //for loop going through the word
            let word = current_word.as_bytes();
            let mut previous: u8 = b' ';

            for l in word {

                //Adding the letter to a temporary acronym vector in case the word is not an already existing acronym
                if !first_letter {
                    
                    first_letter = true;
                    potential_letters.push((*l as char).to_ascii_uppercase());
                } else if (*l as char).is_uppercase() && !(previous as char).is_uppercase() {
                    potential_letters.push((*l as char).to_ascii_uppercase());
                    valid = true;
                }
                previous = *l;
            }

            if valid {
                new_acronym.push_str(&potential_letters);
            } else if word.len() > 0 {
                new_acronym.push((word[0] as char).to_ascii_uppercase());
            }

            potential_letters.clear();

            current_word.clear();
        } else {
            current_word.push(current)
        }

        n += 1;

        
    }
    new_acronym
}


fn find_most_frequent_bird_count(birds: &Vec<&str>) -> u64 {
    
    //One vector to contain the ID of the birds that have already been counted frequency
    let mut bird_ids: HashMap<&str, u64> = HashMap::new();
     
    //For loop going through each item of the vector
    for bird in birds {
        *bird_ids.entry(&bird).or_default() += 1;
    }

    bird_ids.into_values().max().unwrap_or(0)
}


fn find_most_frequent_bird_no_order<'a>(birds: &[&'a str]) -> Option<&'a str> {

    //Early return if empty 
    if birds.len() == 0 {
        return None;
    }
    
    let mut bird_ids: HashMap<&str, i64> = HashMap::new(); //Hash dos for sikkerhet, gjør rekkefølgen tilfeldig
     
    //For loop going through each item of the vector
    for bird in birds {
        *bird_ids.entry(&bird).or_default() += 1;
    }

    let max = bird_ids.values().copied().max().unwrap_or(0);

    bird_ids.into_iter().find_map(|(key, value)| if value == max {Some(key)} else {None})
}


fn find_most_frequent_bird<'a>(birds: &[&'a str]) -> Option<&'a str> {
   
    //Early return if empty 
    if birds.len() == 0 {
        return None;
    }
    
    let mut bird_ids: HashMap<&str, i64> = HashMap::new(); //Hash dos for sikkerhet, gjør rekkefølgen tilfeldig
     
    //For loop going through each item of the vector
    for bird in birds {
        *bird_ids.entry(&bird).or_default() += 1;
    }

    let max = bird_ids.values().copied().max().unwrap_or(0);

    //bird_ids.into_iter().find_map(|(key, value)| if value == max {Some(key)} else {None})
    for bird in birds {
        if *bird_ids.get(bird).unwrap() == max {
            return Some(bird);
        }
    }
    None
}




// /////////////////////////////// Tests //////////////////////////////////
// // Run the tests with `cargo run --release` to see if everything worked.


fn test_acronym() {
    let data = vec![
        ("Lecturer's like to use acronyms in their lectures", "LLTUAITL"),
        ("Lecturer's like-to_use|acronyms in their\tlectures", "LLTUAITL"),
        ("Portable Network Graphics", "PNG"),
        ("GNU Image Manipulation Program", "GIMP"),
        ("GNU Image Manipulation Program", "GIMP"),
        ("Rolling On The Floor Laughing So Hard", "ROTFLSH"),
        ("Ruby on Rails", "ROR"),
        ("HyperText Markup Language", "HTML"),
        ("First In, First Out", "FIFO"),
        ("PHP: Hypertext Preprocessor", "PHP"),
        ("PNG: Network Graphics", "PNG"),
        ("Make IT easy", "MIE"),
        ("Make I:T easy", "MITE"),
        ("Make I'T easy", "MITE"),
        ("Complementary metal-oxide semiconductor", "CMOS"),
        ("Complementary:metal-oxide semiconductor", "COS"),
    ];
    for (input, expected) in data {
        let output = acronym(input);
        assert_eq!(output, expected);
    }
}


fn test_reverse() {
    let data = [ 
                ("", ""), ("a", "a"), // edge cases
                ("Hello", "olleH"), ("World", "dlroW"), // Hellow World, of course :)
                ("1234567890", "0987654321"), ("123456789", "987654321"),
                ("This is my string", "gnirts ym si sihT"),
                ("This\tis my\n string", "gnirts \nym si\tsihT"), // with tabs and newlines
                ];
    for (input, expected) in data {
        let output = reverse(input);
        assert_eq!(output, expected);
    }
}


fn test_find_most_frequent_bird_count() {
    let data = [
        (vec!("a1","bz2","a3","a1","bz2","a1"), 3 as u64),
        (vec!("a1","bz2","a3","a1","bz2","a1","a1"), 4),
        (vec!("a1","bz2","a3","a1","bz2","a1","a1","a1"), 5),
        (vec!("a1","bz2","a3","a1","bz2","a1","a1","a1","a1"), 6),
        (vec!("a1","bz2","a3","a1","bz2","a1","a1","a1","a1","a1"), 7),
        (vec!("a1","bz2","a3","a1","bz2","a1","a1","a1","a1","a1","a1"), 8),
        (vec!("a1","bz2","a3","a1","bz2","a1","a1","a1","a1","a1","a1","a1"), 9),
        (vec!("a1","bz2","a3","a1","bz2","a1","a1","a1","a1","a1","a1","a1","a1"), 10),
        (vec!("a1","bz2","a3","a1","bz2","a1","a1","a1","a1","a1","a1","a1","a1","a1"), 11),
        (vec!("a1","bz2","a3","a1","bz2","a1","a1","a1","a1","a1","a1","a1","a1","a1","a1"), 12)];
    for (input, expected) in data {
        let output = find_most_frequent_bird_count(&input);
        assert_eq!(output, expected);
    }
}


// // Normal test cases that are independent on the order of the most frequent bird appearance
// // in the data log.
fn test_find_most_frequent_bird(f: for<'a> fn(&[&'a str]) -> Option<&'a str>){
    let data = [
        (vec!(), None), // edge case
        (vec!("a1"), Some("a1")), // edge case
        (vec!["a1","bz2","a3","a1","bz2","a1"], Some("a1")),
        (vec!["a1","bz2","a3","a1","bz2","a1","a1"], Some("a1")),
        (vec!["a1","bz2","a3","a1","bz2","a1","a1","a1"], Some("a1")),
        (vec!["a1","bz2","a3","a1","bz2","a1","a1","a1","a1"], Some("a1")),
        (vec!["a1","bz2","a3","a1","bz2","a1","a1","a1","a1","a1"], Some("a1")),
        (vec!["a1","bz2","a3","a1","bz2","a1","a1","a1","a1","a1","a1"], Some("a1")),
        (vec!["a1","bz2","a3","a1","bz2","a1","a1","a1","a1","a1","a1","a1"], Some("a1")),
        (vec!["a1","bz2","a3","a1","bz2","a1","a1","a1","a1","a1","a1","a1","a1"], Some("a1")),
        (vec!["a1","bz2","a3","a1","bz2","a1","a1","a1","a1","a1","a1","a1","a1","a1"], Some("a1")),
        (vec!["a1","bz2","a3","a1","bz2","a1","a1","a1","a1","a1","a1","a1","a1","a1","a1"], Some("a1")),
        ];

    for (input, expected) in data {
        let output = f(&input);
        assert_eq!(output, expected);
    }
}

// // Test cases that are dependent on the order of the most frequent bird appearance
// // in the data log. These test if the spec has been completed properly. 
fn test_find_most_frequent_bird_with_order() {
    let data = [
        (vec!(), None), // edge case
        (vec!("a1"), Some("a1")), // edge case
        (vec!("a1","a2","a3"), Some("a1")), // edge case, must return the first most frequent
        (vec!("a1","a2","a3","a1","a2","a3"), Some("a1")), // edge case, must return the first most frequent
        (vec!("a2","a1","a1","a2","a2","a3"), Some("a2")),
        (vec!("a1","a2","a2","a1","a2","a1"), Some("a1")), 
        (vec!("a1","a2","a3","a3","a3","a2","a2"), Some("a2")) // edge case, must return the first most frequent
        ];

    for (input, expected) in data {
        let output = find_most_frequent_bird(&input);
        assert_eq!(output, expected);
    }
}


fn test_find_most_frequent_bird_no_order() {
    let data = [
        (vec!(), vec!(None)), // edge case
        (vec!("a1"), vec!(Some("a1"))), // edge case
        (vec!("a1","a2","a3"), vec!(Some("a1"), Some("a2"), Some("a3"))), // edge case, must return the first most frequent
        (vec!("a1","a2","a3","a1","a2","a3"), vec!(Some("a1"), Some("a2"), Some("a3"))), // edge case, must return the first most frequent
        (vec!("a2","a1","a1","a2","a2","a3"), vec!(Some("a2"))),
        (vec!("a1","a2","a2","a1","a2","a1"), vec!(Some("a1"), Some("a2"))), 
        (vec!("a1","a2","a3","a3","a3","a2","a2"), vec!(Some("a2"), Some("a3"))) // edge case, must return the first most frequent
        ];

    for (input, expected) in data {
        let output = find_most_frequent_bird_no_order(&input);
        assert!(expected.contains(&output));
    }
}


