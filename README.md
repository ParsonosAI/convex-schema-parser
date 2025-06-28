# Convex Schema Parser & Client Generator

`convex-schema-parser` is a simple command-line tool designed to parse your Convex project's schema and function definitions, generating strongly-typed API clients for both Rust and Python.

It offers two primary modes of operation:

1. A one-shot `generate` command for manual client generation.

2. A persistent `dev` mode that watches your Convex project for changes and automatically regenerates your clients, providing a seamless development experience.

## Prerequisites

Before using the tool, please ensure your environment meets the following requirements:

### 1. `pnpm` Installation

The tool shells out to your package manager to generate TypeScript declaration files (`.d.ts`). You must have `pnpm` installed and available in your system's PATH.

### 2. `package.json` Script

Your Convex project's `package.json` must contain a script named `declarations`. This script is responsible for running all the necessary steps to generate the `.d.ts` files for the parser to read. This often involves cleaning old artifacts, running the TypeScript compiler, and copying over pre-generated files.

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
convex-schema-parser dev --path <path> [--config <path>]
```

### Arguments:

* `--path`: (Required) The path to the root of your Convex project directory (the one containing `package.json` and the `convex/` folder).

* `--config`: (Optional) The path to your YAML configuration file. Defaults to `convex-parser.yaml` in the current working directory.

# Configuration (`convex-parser.yaml`)

The `dev` mode is driven by a YAML configuration file. This file allows you to define multiple generation targets, enabling you to generate clients for different languages and output them to multiple locations simultaneously.

### Example `convex-parser.yaml`:

```yaml
# A list of generation targets. You can have one or more.
targets:

  # Target 1: Generate a Rust client for a backend service.
  - lang: Rust
    # A list of output files. The generated code will be written to all of them.
    output:
      - /path/to/my-rust-backend/src/convex_api.rs

  # Target 2: Generate a Python client for something else.
  - lang: Python
    output:
      - /path/to/my-python-scripts/lib/convex_client.py
      # You can specify multiple output paths for the same target.
      - /path/to/another-project/shared/convex_client.py
```

### Configuration Schema:

* `targets`: A opt-level key holding a list `[]` of target configurations.
* `lang`: The target language for the client. **Must** be `Python` or `Rust`.
* `output`: A list `[]` of file paths where the generated client code will be written. Each target can have multiple output paths.
