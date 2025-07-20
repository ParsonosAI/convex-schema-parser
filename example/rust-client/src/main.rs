use convex::ConvexClient;

// We register the generated module.
mod papi;

#[tokio::main]
async fn main() {
    let client = ConvexClient::new("http://localhost:8080")
        .await
        .expect("Failed to create ConvexClient");
    let mut api = papi::Api::new(client);

    match api.list_messages().list().await {
        Ok(messages) => println!("Messages: {:?}", messages),
        Err(e) => {
            println!("An error occurred: {}", e);
            println!("Please ensure the convex backend is running and the client is generated.");
        }
    }

    match api
        .send_message()
        .send("author-rust", "my blazingly short rust text")
        .await
    {
        Ok(_) => println!("Sent a message!"),
        Err(e) => {
            println!("An error occurred: {}", e);
            println!("Please ensure the convex backend is running and the client is generated.");
        }
    }
}
