import papi
from convex import ConvexClient


def main():
    client = ConvexClient("http://localhost:8080")
    api = papi.API(client)
    try:
        messages = api.list_messages.list()
        print("Messages:", messages)

        api.send_message.send(author="user1", body="Hello from Python client 1!")
        print("Sent a message!")
    except Exception as e:
        print(f"An error occurred: {e}")
        print(
            "Please ensure the convex backend is running and the client is generated."
        )


if __name__ == "__main__":
    main()
