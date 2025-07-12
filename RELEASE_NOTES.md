# Release Notes: convex-schema-parser 0.1.5.0

This release introduces several key improvements to the Python and Rust backends, enhancing type safety, correctness, and overall developer experience.

## Backend Improvements

*   **Python:**
    *   **Correct Definition Ordering:** The Python backend now analyzes the dependency graph of generated Pydantic models and constants, ensuring that all definitions are topologically sorted. This resolves previous issues where code would fail to compile due to out-of-order declarations, especially in schemas with complex, nested object types.
    *   **Pydantic `ConvexInt64` Support:** We've added seamless integration for `ConvexInt64` within Pydantic models. This ensures that `ConvexInt64` types from your schema are correctly validated and serialized, maintaining data integrity between your Convex backend and Python client.

*   **Rust:**
    *   **`TryFrom` for Anonymous Enums:** The Rust backend now automatically generates `TryFrom` implementations for anonymous enums (string literal unions). This provides a safe and idiomatic way to convert raw strings into their corresponding enum types, with robust error handling for invalid values.

## Example: Python Definition Ordering

Previously, a schema with nested objects could produce invalid Python code:

```python
# Invalid Code: AssetValidatorObjectLink_metadataObject is used before it's defined
class AssetValidatorObject(BaseModel):
    link_metadata: AssetValidatorObjectLink_metadataObject = Field(...)
    # ...

class AssetValidatorObjectLink_metadataObject(BaseModel):
    # ...
```

With this release, the generator correctly orders the definitions:

```python
# Correctly Ordered Code
class AssetValidatorObjectLink_metadataObject(BaseModel):
    # ...

class AssetValidatorObject(BaseModel):
    link_metadata: AssetValidatorObjectLink_metadataObject = Field(...)
    # ...
```