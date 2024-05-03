use std::fs::read_to_string;

const FILENAME : &str = "data2.txt";
fn main(){

    let relevant_letters = "otfsen"; //Check if the letter is relevant to our digit-words, so o for one, t for two and three, f for four and five, s for six and seven, e for eight and n for nine

    let mut bearing_component_sum = 0;

    let inscriptions : Vec<String> = read_from_file();
    
    for inscription in inscriptions {

        let length = inscription.len();	
        let mut digits : Vec<char> = Vec::new(); //First and last will be sat together to the 2-digit value

	    'letter_: for (index, letter) in inscription.chars().enumerate() {

            //Checks whether current letter is digit, otherwise we check if it is a letter what could be a potential digit
	    	if letter.is_digit(10){
	    		digits.push(letter);
	    	} else {

                //If the current letter is relvant, we check if the word is a digit-word
	    		if relevant_letters.contains(letter) { 
	    			match word_digit(letter, &inscription, index, &length) {
	    				Ok(digit) => {
	    					digits.push(digit);
	    				}
	    				Err(_) => continue 'letter_,
	    			}
	    		}
            
	    	}
        

	    }
        
        //We may not find any digits in the inscription and therefore just continiue
        if digits.len() == 0 {
            continue;
        }

        //Concatinate the first and last digit to get the 2-digit value
        let two_digit : String = digits[0].to_string() + &digits[digits.len() - 1].to_string();
        bearing_component_sum += two_digit.parse::<u32>().unwrap();

    }

    print!("New bearing component of the BPROG Voyager: {}", bearing_component_sum%360);
}

fn word_digit(letter : char, inscripton : &String, index : usize, length : &usize) -> Result<char, ()>{
	
	let mut checks : Vec<(&str, char)> = Vec::new();
	match letter {
		'o' => checks.push(("one", '1')),
		't' => {
            checks.push(("two", '2')); checks.push(("three", '3'));
        },
        'f' => {
            checks.push(("four", '4')); checks.push(("five", '5'));
        },
        's' => {
            checks.push(("six", '6')); checks.push(("seven", '7'));
        },
        'e' => checks.push(("eight", '8')),
        'n' => checks.push(("nine", '9')),
        _ => return Err(()),
    }

	for (word, digit) in checks {
		let range = index + word.len(); 
        if range <= *length {
			if inscripton[index..range] == *word {
				return Ok(digit)
			}
		}
	}

	return Err(())
}

fn read_from_file() -> Vec<String> {
    let contents = read_to_string(FILENAME).unwrap();
    let data : Vec<String> = contents.lines().map(|s| s.to_string()).filter(|s| s != "").collect();
    data
}

	