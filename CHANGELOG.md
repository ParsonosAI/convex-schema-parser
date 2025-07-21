# Revision history for convex-schema-parser

## 0.1.6.0 -- 2025-07-21

* **Codegen Improvements Rust:**
    * `convex-schema-parser` now properly handles union types with special chars.

## 0.1.5.0 -- 2025-07-12

*   **Backend Improvements:**
    *   **Python:**
        *   The generated Python code now correctly orders class and constant definitions based on their dependencies, resolving issues with out-of-order declarations.
        *   Added support for `ConvexInt64` with Pydantic, ensuring proper validation and serialization.
    *   **Rust:**
        *   Generated `TryFrom` implementations for anonymous enums, improving type safety and ergonomics.

## 0.1.4.0 -- 2025-07-09

*   **Fixes:**
    *   Properly unify nested types during the unification pass.

## 0.1.3.0 -- 2025-07-08

*   **Schema Parsing:**
    *   More robust implementation of the schema parser provides better error handling and more accurate parsing.
    *   Correctly handles foreign imported types.
    *   Support for `i64`, `f64`, `ArrayBuffer`, and `v.bytes()` has been added.

*   **Backend Improvements:**
    *   **Rust:**
        *   Custom `serde` implementation for `convex::Value`.
        *   Validator for generated types.
        *   Typed subscription streams for Convex subscriptions.
    *   **Python:**
        *   Validator for generated types.
        *   Pydantic annotations to handle fields prefixed with `_`.
        *   Typed subscription generators for Convex subscriptions.

*   **Developer Experience:**
    *   New `init` command to guide users through the setup process.
    *   `optparse-applicative` for a better CLI experience.
    *   New "dev mode" to automatically regenerate code on file changes.
    *   `justfile` to provide a simple way to run common commands.

*   **Fixes:**
    *   **Rust Backend:**
        *   Correctly use the inner optional value when generating functions.
        *   Fixed a bug where `innerValueConversion` was not being properly handled for functions.
        *   Custom structs now know how to encode themselves to `convex::Value`.
    *   **Python Backend:**
        *   Removed redundant imports from the generated Python code.
        *   Properly indent code with helpers.
        *   Reverted to a sync client.
    *   **General:**
        *   Normalized generated strings for easier testing.
        *   Namespaced functions to avoid conflicts in the Python and Rust backends.
        *   Fixed a bug where empty function arguments were not being properly handled in the action parser.
        *   Properly unify nested types during the unification pass.

*   **Miscellaneous:**
    *   Updated README and LICENSE.
    *   Added a CI/CD pipeline for releases.
    *   Support for GHC 9.6.7.

## 0.1.0 -- 2025-06-30

* First version
