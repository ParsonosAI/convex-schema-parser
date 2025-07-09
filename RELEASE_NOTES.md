# Release Notes: convex-schema-parser 0.1.4.0

This release introduces a fix for a bug in the unification pass of the schema parser.

## Fixes

*   **Schema Parsing:**
    *   The schema parser now properly unifies nested types during the unification pass.

## Example

Previously, if you had a table `users` and a query that returned a user object, the generated code would not recognize the return type as a `UsersDoc`.

**Schema (`convex/schema.ts`):**
```typescript
import { defineSchema, defineTable } from "convex/server";
import { v } from "convex/values";

export default defineSchema({
  users: defineTable({
    name: v.string(),
    email: v.string()
  })
});
```

**Action (`convex/myActions.ts`):**
```typescript
import { query } from "./_generated/server";

export const getUser = query({
  handler: async (ctx) => {
    // ... implementation returning a user document
  },
});
```

The parser would generate a generic object type for the return value of `getUser`.

With this release, the parser now correctly unifies the return type with the `users` table schema, resulting in a `UsersDoc` type in the generated client. So no need to manually cast or construct return types to their Doc equivalents anymore.
