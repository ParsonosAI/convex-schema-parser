# Example Project for convex-schema-parser

This directory contains an example project that demonstrates how to use the `convex-schema-parser` to generate type-safe clients for Python and Rust from a Convex project.

## Structure

- `convex-example/`: A simple Convex project with a schema, a query, and a mutation.
- `python-client-1/`: A Python client application.
- `python-client-2/`: A second Python client application.
- `rust-client/`: A Rust client application.
- `convex-parser.yaml`: The configuration file for the `convex-schema-parser` dev command.

## How to Run

1.  **Install Dependencies**
    *   Install the Convex CLI and dependencies for the example Convex project.
    *   Install Python dependencies:
      * `reqwest`
      * `convex` && `python-dotenv`
      * `pydantic`
    *   Install Rust dependencies (`cargo build` in the `rust-client` directory).
    *   Make sure you have the `convex-schema-parser` executable available in your path.
      * With `cabal` do: 
```sh
cabal install convex-schema-parser --overwrite-policy=always
```
If you issue the above command outside of this repository you will get the latest release, if you do that in the root of this repository, you will get a build using your currently checked out `HEAD`.

2.  **You can run a local convex deployment to see everything in action.**

3.  **Generate/Update the Client Libraries**

    ```bash
    convex-schema-parser dev --config example/convex-parser.yaml # Adjust the path if you are in a different directory.
    # By default `convex-schema-parser` searches for `convex-parser.yaml` in the current directory.
    ```

    This command will watch for changes in the `convex-example` project and automatically regenerate the `papi.py` and `papi.rs` files in the respective client directories.

4.  **Run the Clients**

    Once the client libraries are generated/updated and your convex backend is running, you can run the client applications:

    *   **Python:**
        ```bash
        python example/python-client-1/main.py
        python example/python-client-2/main.py
        ```

    *   **Rust:**
        ```bash
        cd example/rust-client
        cargo run
        ```

5. **Go ahead and do some changes to the convex backend project and see the functions and types update!**
