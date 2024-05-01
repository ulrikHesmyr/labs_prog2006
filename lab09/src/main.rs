use serde::Deserialize; 

const JSON_FILE: &str = "test-data.json";
const WRITEFILE: &str = "treasures.txt";

const EARTH_ID: &str = "Greenstone weight:";
const FIRE_ID: &str = "Vulcanic cave temperature:";
const WATER_ID: &str = "Water guardian name:";
const AIR_ID: &str = "Sky riddle:";

const RANGE_MSG: &str = " outside the range";
const CAP_MSG: &str = " not capitalized";
const EVEN_MSG: &str = " not even";
const MISS_DOT_MSG: &str = " missing dot";
const MISS_A_MSG: &str = " missing 'a'";



const VALID: &str = "VALID";

const MIN_EARTH: u32 = 13;
const MAX_EARTH: u32 = 113;

const MIN_WATER: u32 = 3;
const MAX_WATER: u32 = 15;

const MIN_FIRE: u32 = 400;
const MAX_FIRE: u32 = 700;

enum Error {
    Ok,
    Err(String)
}

impl Default for Error {
    fn default() -> Self {
        Error::Ok
    }

}

trait Semigroup {
    fn combine(&mut self, other: Self) -> Self;
}


impl Semigroup for Error {
    fn combine(&mut self, other: Error) -> Self {
        match (self, other) {
            (Error::Ok, Error::Ok) => Error::Ok,
            (Error::Err(s), Error::Ok) => Error::Err(s.to_string()),
            (Error::Ok, Error::Err(s)) => Error::Err(s),
            (Error::Err(s1), Error::Err(s2)) => Error::Err(s1.to_owned() + ", " +&s2),
        }
    }
}

//Source for alias usage: https://serde.rs/field-attrs.html

#[derive(Deserialize)]
struct Gemdata {
    uid: u32,
    #[serde(alias = "greenstoneWeight")] 
    greenstone_weight: u32,
    #[serde(alias = "waterGuardian")] 
    water_guardian: String,
    #[serde(alias = "vulcanicCave")] 
    vulcanic_cave: u32,
    #[serde(alias = "skyRiddle")]
    sky_riddle: String,
}

fn main() {

    let data: Vec<Gemdata> = match read_json_from_file() {
        Ok(data) => data,
        Err(e) => {
            println!("Error reading json from file: {}", e);
            return;
        }
    };

    let string_to_write_file = data.iter().map(|gem| {
        match Error::Ok
            .combine(validate_earth(gem.greenstone_weight)
            .combine(validate_water(&gem.water_guardian))
            .combine(validate_fire(gem.vulcanic_cave))
            .combine(validate_air(&gem.sky_riddle)) ) {
                Error::Ok => format!("{}: {}\n", gem.uid, VALID),
                Error::Err(s) => format!("{}: {}\n", gem.uid, s)
            }
    }).collect::<String>();

    write(string_to_write_file);
        
        
}

//Propogating the error to the main function by using Result type here
fn read_json_from_file() -> Result<Vec<Gemdata>, serde_json::Error> {
    //Reading the file containing the json data
    let contents = std::fs::read_to_string(JSON_FILE).unwrap();
    let data = serde_json::from_str(&contents);
    data
}


fn write(string: String) {
    std::fs::write(WRITEFILE, string).expect("Could not write to file");
}


fn validate_earth(greenstone_weight: u32) -> Error {
    check_even(greenstone_weight, format!("{}{}", EARTH_ID, EVEN_MSG))
    .combine(check_range(MIN_EARTH, MAX_EARTH, greenstone_weight, format!("{}{}", EARTH_ID, RANGE_MSG)))
}

fn validate_water(water_guardian: &str) -> Error {
    check_length_name(&water_guardian, MIN_WATER, MAX_WATER, format!("{}{}", WATER_ID, RANGE_MSG))
    .combine(check_start_w_cap_letter(&water_guardian, format!("{}{}", WATER_ID, CAP_MSG)))
    .combine(check_two_lowercase_a(&water_guardian, format!("{}{}", WATER_ID, MISS_A_MSG)))
    
}

fn validate_fire(vulcanic_cave: u32) -> Error {
    check_even(vulcanic_cave, format!("{}{}", FIRE_ID, EVEN_MSG))
    .combine(check_range(MIN_FIRE, MAX_FIRE, vulcanic_cave, format!("{}{}", FIRE_ID, RANGE_MSG)))
}

fn validate_air(sky_riddle: &str) -> Error {
    check_ends_w_dot(&sky_riddle, format!("{}{}", AIR_ID, MISS_DOT_MSG))
    .combine(check_count_vowels(&sky_riddle, format!("{}{}", AIR_ID, EVEN_MSG)))
    .combine(check_start_w_cap_letter(&sky_riddle, format!("{}{}", AIR_ID, CAP_MSG)))
}

fn check_range(min: u32, max: u32, value: u32, errstr: String) -> Error {
    if value >= min && value <= max {
        Error::Ok
    } else {
        Error::Err(errstr)
    }
}

fn check_even(value: u32, errstr: String) -> Error {
    if value % 2 == 0 {
        Error::Ok
    } else {
        Error::Err(errstr)
    }
}

fn check_length_name(name: &str, min: u32, max: u32, errstr: String) -> Error {
    check_range(min, max, name.len() as u32, errstr)
}

fn check_start_w_cap_letter(name: &str, errstr: String) -> Error {
    if name.chars().next().unwrap().is_uppercase() {
        Error::Ok
    } else {
        Error::Err(errstr)
    
    }
}

fn check_two_lowercase_a(name: &str, errstr: String) -> Error {
    let mut count = 0;
    for c in name.chars() {
        if c == 'a' {
            count += 1;
        }
    }
    if count == 2 {
        Error::Ok
    } else {
        Error::Err(errstr)
    }
}

fn check_ends_w_dot(name: &str, errstr: String) -> Error {
    if name.chars().last().unwrap() == '.' {
        Error::Ok
    } else {
        Error::Err(errstr)
    }
}

fn check_count_vowels(name: &str, errstr: String) -> Error {
    let mut count = 0;
    for c in name.chars() {
        if c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' {
            count += 1;
        }
    }
    check_even(count, errstr)
}
