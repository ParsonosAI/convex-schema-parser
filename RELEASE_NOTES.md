# Release Notes: convex-schema-parser 0.1.6.0

This release improves code-generation for the Rust backend in regards to union types which contain fields with special names:

```ts
export const instruction_mime_type = v.union(
        v.literal("application/pdf"),
        v.literal("text/html"),
        v.literal("text/plain")
);
```

Previously the generated enum for Rust would create fields like:

```rs
pub enum InstructionMimeType {
    #[default]
    #[serde(rename = "application/pdf")]
    Application/Pdf,
    #[serde(rename = "text/html")]
    Text/Html,
    #[serde(rename = "text/plain")]
    Text/Plain,
}
```

The above does not compile. Now we have:

```rs
pub enum InstructionMimeType {
    #[default]
    #[serde(rename = "application/pdf")]
    ApplicationPdf,
    #[serde(rename = "text/html")]
    TextHtml,
    #[serde(rename = "text/plain")]
    TextPlain,
}
```
