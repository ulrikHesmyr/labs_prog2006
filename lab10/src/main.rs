/*Async has reduced CPU and memory overhead compared to std::thread when
 working with I/O-bound tasks, such as a server in our case.
 An async runtime uses a small amount of (expensive) threads to handle a large amount of (cheap) tasks.
*/
use axum::{
    extract::rejection::JsonRejection, http::StatusCode, routing::{get, post}, Json, Router, extract::Path
};


#[derive(serde::Deserialize)]
struct InputGreeting {
    name: String,
    #[serde(alias = "input")]
    greet: String,
}

#[derive(serde::Serialize)]
struct Greeting {
    name: String,
    greet: String,
}

#[tokio::main]
async fn main() { 
    // build our application with a single route
    let app = Router::new()
    .route("/hello", get(hello_handler))
    .route("/greet/:name", get(greet_handler))
    .route("/greetme", post(greetme_handler))
    .fallback(|| async {
        (StatusCode::NOT_FOUND, "404 page not found! Nothing to see here!")
    });

    
    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, app).await.unwrap();
}


async fn hello_handler() -> &'static str {
    "Hello, World!"
}

async fn greet_handler(Path(name): Path<String>) -> String { //Extracts the parameter from the URI
    let greeting = Greeting {
        name,
        greet: "Hello".to_owned(),
    };

    match serde_json::to_string(&greeting) {
        Ok(greeting) => greeting,
        Err(_) => (StatusCode::BAD_REQUEST).to_string(),
    }
}

async fn greetme_handler(input: Result<Json<InputGreeting>, JsonRejection>) -> Result<Json<Greeting>, StatusCode> {
    let input = match input {
        Ok(input) => Ok(Json(Greeting {
            name: input.name.clone(),
            greet: input.greet.clone(),
        })),
        Err(_) => Err(StatusCode::BAD_REQUEST)
    };
    input
}


