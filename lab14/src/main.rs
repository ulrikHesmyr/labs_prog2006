use std::collections::HashSet;

struct Island {
    x: usize,
    y: usize,
}

const FILE: &str = "data.txt";
const MAP_EXPANSION : usize = 999999; //Expansion of 1000000 in x and y direction

fn main() {
    let waterworld : Vec<String> = read_from_file();

    let mut islands : Vec<Island> = Vec::new();
    
    //Extracting data of the islands from the waterworld
    let mut columns_with_islands : Vec<usize> = Vec::new();
    let mut extra_row_counter: usize = 0;

    //Iterating through the waterworld data to find the islands
    for (y, row) in waterworld.iter().enumerate() {
        
        //Checking whether the row contains any islands
        let mut no_islands_row = true;
        for (x, ch) in row.chars().enumerate() {

            //We register an Island in addtion to registering whether this column and row contains an island to later expand the map in both x and y direction 
            if ch == '#' {
                no_islands_row = false;
                columns_with_islands.push(x);
                islands.push(Island{x, y: (y+extra_row_counter)})
            }
        }

        //If there was no islands for the row, we increase the counter due to map expansion in the y direction
        if no_islands_row {
            extra_row_counter += MAP_EXPANSION; //Expansion in y direction
        }
    }

    println!("Sum of shortest paths: {}", get_total_length_shortest_paths(columns_with_islands, islands, waterworld[0].len()));

    	
}




fn get_columns_without_islands(columns_with_islands : &Vec<usize>, amount_of_columns: usize) -> Vec<usize>{

    //Creates a hashset of the columns with islands to use the .contain() method below
	let columns_with_islands_set: HashSet<_> = columns_with_islands.iter().cloned().collect();

    //Filters out the columns with islands, so we remain with the columns without islands
	let columns_without_islands: Vec<usize> = (0..amount_of_columns).filter(|n| !columns_with_islands_set.contains(&n)).collect::<Vec<_>>();
	columns_without_islands
}

fn length_shortest_path(second_island: &Island, first_island: &Island, columns_without_islands: &Vec<usize>) -> i64{
	
    //Calculating extra distance in x direction due to expansion of the map in the x direction
	let mut extra_distance_x = 0;
	for column_index in columns_without_islands{
        if !(column_index < &second_island.x && column_index < &first_island.x) && !(column_index > &second_island.x && column_index > &first_island.x){
            extra_distance_x += MAP_EXPANSION; //Expansion in x direction
        }
    }

    //Uses the coordinates to calculate the shortest path between two islands
    let x1 : i64 = first_island.x as i64;
    let x2 : i64 = second_island.x as i64;

    let y1 : i64 = first_island.y as i64;
    let y2 : i64 = second_island.y as i64;

	let steps_x = (x2-x1).abs() + extra_distance_x as i64;
	let steps_y = (y2-y1).abs();
    
	return steps_x + steps_y
}	

fn read_from_file() -> Vec<String> {
    //Reads from a waterworld data file and returns a vector of strings where each row/line is a string
    let file = std::fs::read_to_string(FILE).expect("Unable to read file");
    let lines: Vec<String> = file.lines().map(|s| s.to_string()).collect();
    lines
}

fn get_total_length_shortest_paths(columns_with_islands : Vec<usize>, islands: Vec<Island>, length_row: usize) -> i64 {
    let columns_without_islands : Vec<usize> = get_columns_without_islands(&columns_with_islands, length_row);

    //Finding the total length of all shortest paths
    let mut sum_shortest_path: i64 = 0;

    //Finding the length of the shortest path for all permutations of the islands
    for (i, island_i) in islands.iter().enumerate() {

        //Using .skip(i+1) to skip the n first elements to calculate the shortest path between two islands only once
        for island_j in islands.iter().skip(i+1) {

            //Calculates the shortest path between two islands and adds it to the total length
            let length = length_shortest_path(&island_i, &island_j, &columns_without_islands);
            sum_shortest_path += length;
        }
    }
    sum_shortest_path
}