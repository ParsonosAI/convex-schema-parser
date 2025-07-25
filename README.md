# Convex Schema Parser & Client Generator

`convex-schema-parser` is a simple command-line tool designed to parse your Convex project's schema and function definitions, generating strongly-typed API clients for both Rust and Python.

It offers two primary modes of operation:

1. A one-shot `generate` command for manual client generation.

2. A persistent `dev` mode that watches your Convex project for changes and automatically regenerates your clients, providing a seamless development experience.

3. You can checkout a [short overview video](https://youtu.be/4kzkhacJYvk).

> [!IMPORTANT]
> At the bottom you will find a USAGE section and in `examples` a complete simple example configuration with multiple projects.

## Installation

The easiest way to use `convex-schema-parser` is currently through the Cabal package manager.

> [!NOTE]
> We are on hackage now! So you can simply run `cabal update` (important, this package is a recent addition to Hackage) and then `cabal install convex-schema-parser` to install the tool quickly without cloning.

We provide prebuilt binaries for `linux` & `macOS` that you can download and run directly, but `macOS` users have to allow the binary to run first since we do not sign it (yet).
A `npm` package `@parsonosai/convex-schema-parser` is also on its way and supported as soon as we get code-signing ready, we currently use a placeholder.

Installing `cabal` & `ghc` is best done using [`ghcup`](https://www.haskell.org/ghcup/). As soon as it is installed:

```bash
ghcup install ghc 9.10.1
ghcup install cabal 3.10.3.0
```

You can then build/run the tool from source.

```bash
cabal update
cabal install convex-schema-parser # If you have $HOME/.cabal/bin in your PATH.
cabal run convex-schema-parser -- --help # If you do not want to install it globally and just run it.
```

## Prerequisites

> [!NOTE]
> Everything here is also explained when you issue `conves-schema-parser init`.

Before using the tool, please ensure your environment meets the following requirements:

### 1. `pnpm` or `npm` Installation

The tool shells out to your package manager to generate TypeScript declaration files (`.d.ts`). You must have `pnpm` or `npm` installed and available in your system's PATH.

### 2. `package.json` Script

Your Convex project's `package.json` must contain a script named `declarations`. This script is responsible for running all the necessary steps to generate the `.d.ts` files for the parser to read. This often involves cleaning old artifacts, running the TypeScript compiler, and copying over pre-generated files.
Everything here will also be explained when you issue `convex-schema-parser init`, this command will also create a template `convex-parser.yaml` file.

**Example `package.json`:**

```json
{
  "scripts": {
    "declarations:clean": "rm -rf tmp",
    "declarations:build": "tsc -p tsconfig.declarations.json",
    "declarations:copy-generated": "cp -r convex/_generated tmp/declarations/",
    "declarations": "npm run declarations:clean && npm run declarations:build && npm run declarations:copy-generated",
    "test": "echo \"Error: no test specified\" && exit 1"
  }
}
```

### 3. tsconfig.declarations.json

The tool assumes a specific `tsconfig.json` file exists to guide the declaration generation process. This file should be configured to only emit declaration files into a designated output directory (e.g., `tmp/declarations`) from your source `convex/` directory.

Example `tsconfig.declarations.json`:

```json
{
  "compilerOptions": {
    // --- Output Configuration ---
    "outDir": "./tmp/declarations", // Output all files to a temporary directory
    "rootDir": "./convex",          // The root of the source files we care about

    // --- Generation Settings ---
    "declaration": true,            // Generate .d.ts files
    "emitDeclarationOnly": true,    // Don't generate any .js files
    "skipLibCheck": true,           // Speeds up compilation by not checking library files

    // --- Module Settings (to match Convex) ---
    "module": "commonjs",
    "target": "es2020"
  },
  // Tell tsc where to find the files to compile
  "include": ["convex/**/*.ts"]
}
```

# Usage

The tool is run from the command line and has two main subcommands: generate and dev.

## `generate` Command (One-Shot)

This command parses your project once, generates the specified client, and then exits. It's useful for CI/CD pipelines or manual updates.

```bash
convex-schema-parser generate --schema <path> --declarations <path> --target <lang> [-o <output_file>]

# Or if you are not using the installed binary but via cabal (same for the other commands):
cabal run convex-schema-parser -- generate --schema <path> --declarations <path> --target <lang> [-o <output_file>]
```

### Arguments:

* `--schema`: (Required) Path to your main `schema.ts` file.
 
* `--declarations`: (Required) Path to the root directory where `tsc` generated the `.d.ts` files (the `outDir` from your `tsconfig`).
 
* `--target`: (Required) The target language. Can be `Python` or `Rust`.
 
* `-o, --output`: (Optional) The file to write the generated code to. If omitted, the code will be printed to standard output.

### Example:

```bash
cabal run convex-schema-parser -- generate \
  --schema /path/to/my-project/convex/schema.ts \
  --declarations /path/to/my-project/tmp/declarations \
  --target Rust \
  --output /path/to/my-rust-app/src/convex_api.rs
```

## `dev` Command (Watch Mode)

This is the recommended mode for local development. It starts a persistent process that watches your `convex/` directory for any file changes. When a change is detected, it automatically runs the `pnpm declarations` (or `npm run declarations`) script and regenerates all clients defined in your configuration file.

```bash
convex-schema-parser dev [--config <path>]
```

### Arguments:

* `--config`: (Optional) The path to your YAML configuration file. Defaults to `convex-parser.yaml` in the current working directory.

# Configuration (`convex-parser.yaml`)

The `dev` mode is driven by a YAML configuration file. This file allows you to define multiple generation targets, enabling you to generate clients for different languages and output them to multiple locations simultaneously.

### Example `convex-parser.yaml`:

```yaml
# Configuration for the Convex Client Generator.

# (Required) The absolute path to the root of your Convex project.
# This is the directory that contains the `convex/` folder and `package.json`.
project_path: /path/to/your/convex/project

# (Required) The absolute path to the generated TypeScript declarations, relative to `project_path`.
declarations_dir: /path/to/your/tmp/declarations

# (Optional) The path where validation sandbox projects will be created.
# Defaults to `~/.config/convex-schema-parser` if omitted.
validation_path: "~/.config/convex-schema-parser"

# (Required) A list of generation targets. You can have one or more.
targets:
  # Example 1: Generate a Rust client for a backend service.
  - lang: Rust
    # A list of one or more output files for this target.
    output:
      - ../my-rust-app/src/convex_api.rs
      - ../my-other-app/src/convex_api.rs

  # Example 2: Generate a Python client for data scripts.
  - lang: Python
    output:
      - ../scripts/lib/convex_client.py
```

### Configuration Schema:

* `project_path`: The absolute path to your Convex project root directory. This directory should contain the `convex/` folder and a `package.json` file.
* `declarations_dir`: The absolute path to the directory where your TypeScript declaration files (`.d.ts`) are generated. This should be the output directory specified in your `tsconfig.declarations.json`.
* `validation_path`: (Optional) The path where validation sandbox projects will be created. If omitted, defaults to `~/.config/convex-schema-parser`.
* `targets`: A opt-level key holding a list `[]` of target configurations.
* `lang`: The target language for the client. **Must** be `Python` or `Rust`.
* `output`: A list `[]` of file paths where the generated client code will be written. Each target can have multiple output paths.

# API Usage Examples

Once you have generated your client code, you can use it in your projects.

## Python Client Example

The generated Python client uses nested classes to mirror your Convex project's file structure.

### Queries, Mutations and Actions

```python
import os
from convex import ConvexClient
# Import the generated API module (e.g., convex_api.py)
import convex_api

# 1. Instantiate the official ConvexClient with your deployment URL.
deployment_url = os.environ.get("CONVEX_URL")
client = ConvexClient(deployment_url)

# 2. Instantiate your generated API, wrapping the client.
auth_key = get_auth_key()  # Replace with your method to get the auth/api/jwt key if required
client.set_auth(auth_key)

api = convex_api.API(client)

# 3. Call functions using the nested structure.
# This corresponds to the function `getProject` in `convex/functions/projects.ts`.
# The generated API reraises any exceptions from the Convex client, so you can handle them as needed.
# Additionally, we use `pydantic` for type validation, so we raise these exceptions as well.
try:
    project_id = convex_api.Id("prj_...")
    # NOTE that we force named arguments so you do not accidentally refactor stuff in your convex db
    # and forget to adjust your other projects. See the annotation for the rust example below for
    # the reason.
    project = api.functions.projects.get_project(project_id=project_id)
    if project:
        print(f"Successfully fetched project: {project.project_name}")
    else:
        print("Project not found.")
except Exception as e:
    print(f"An error occurred: {e}")
```

### Subscriptions

```python
from convex import ConvexError

# ... (assuming `api` is already instantiated and authenticated)

try:
    # 1. Call the generated `subscribe_*` method. This returns a generator instantly.
    tenant_id = convex_api.Id("tnt_...")
    project_subscription = api.functions.queries.subscribe_fetch_projects(tenant_id=tenant_id)

    print("Subscribed to projects. Waiting for updates... (Press Ctrl+C to stop)")

    # 2. The `for` loop starts the subscription and blocks until the first value
    #    is received. The loop body will run again for each subsequent update.
    for updated_projects in project_subscription:
        # 3. `updated_projects` is already a fully validated Pydantic model
        #    (e.g., list[FetchProjectsReturnObject]).
        print(f"Received update with {len(updated_projects)} projects:")
        for project in updated_projects:
            print(f"  - ID: {project._id}, Name: {project.project_name}")

except ConvexError as e:
    print(f"Subscription failed with an error: {e}")
except KeyboardInterrupt:
    print("\nSubscription stopped by user.")
```


## Rust Client Example

The generated Rust client uses a method-based API which works with Rust's ownership and borrowing rules.

### Queries, Mutations and Actions

```rust
// Assuming the generated module is named `convex_api`.
use convex_api::{Api, Id, types::ProjectsDoc};
use convex::ConvexClient;
use anyhow::Result;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // 1. Instantiate and authenticate the official ConvexClient.
    let convex_url = std::env::var("CONVEX_URL")?;
    let auth_key = get_auth_key(); // Replace with your method to get the auth/api/jwt key if required
    
    let mut convex_client = ConvexClient::new(&convex_url).await?;
    convex_client.set_auth(&auth_key);

    // 2. Instantiate your generated API, wrapping the client.
    let mut api = Api::new(convex_client);

    // 3. Call functions using the nested, method-chaining API.
    let project_id = Id::<ProjectsDoc>::new("prj_...".to_string());
    
    // This corresponds to the function `getProject` in `convex/functions/projects.ts`.
    // NOTE the use of an argument type:
    //    We cannot rely on convex to output deterministic positional args for the type generated for convex
    //    functions. And in case you reorder arguments of the same type (like having two or more strings with
    //    semantic differences) this will simulate "named args".
    //    Especially for generated code, this is safer and keeps the calling site consistent regardless
    //    of refactors in the convex backend.
    match api.functions().projects().get_project(convex_api::types::FunctionsQueriesGetProjectArg { project_id }).await {
        Ok(Some(project)) => {
            println!("Successfully fetched project: {}", project.project_name.unwrap_or_default());
        }
        Ok(None) => {
            println!("Project not found.");
        }
        Err(e) => {
            eprintln!("An error occurred: {}", e);
        }
    }

    Ok(())
}
```

### Subscriptions

```rust
use futures_util::stream::StreamExt;

// ... (assuming `api` is already instantiated and authenticated)

async fn run_subscription() -> anyhow::Result<()> {
    // 1. Call the generated `subscribe_*` method.
    let tenant_id = convex_api::Id::<convex_api::types::TenantsDoc>::new("tnt_...".to_string());
    let mut project_subscription = api.functions().queries().subscribe_fetch_projects(convex_api::types::FunctionsQueriesFetchProjectsArgObject { tenant_id }).await?;

    println!("Subscribed to projects. Waiting for updates... (Press Ctrl+C to stop)");

    // 2. The `while let` loop asynchronously polls the stream for new items.
    while let Some(result) = project_subscription.next().await {
        // 3. Each `result` is a `Result<T, ApiError>`, where T is your strongly-typed
        //    return value (e.g., Vec<FetchProjectsReturnObject>).
        match result {
            Ok(updated_projects) => {
                println!("Received update with {} projects:", updated_projects.len());
                for project in updated_projects {
                    println!("  - ID: {}, Name: {}", project._id, project.project_name.unwrap_or_default());
                }
            }
            Err(e) => {
                eprintln!("Received an error in the subscription stream: {}", e);
            }
        }
    }

    Ok(())
}
```
