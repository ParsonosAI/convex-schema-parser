# Release Notes: convex-schema-parser 0.1.7.0

## Codegeneration

We changed the codegeneration to increase DX when actively developing your Convex backend.

### Reasoning

We parse functions and arguments from the typed `tsc` output for convex.
While this gives us a guarantee to receive all values (even implicit ones like system fields), we do not have a guarantee that the typed handlers for queries, mutations or actions align with the order of parameters you defined in your `.ts` functions.
Due to the type-magic, the arguments might be reordered unpredictably.
This was a blessing in disguise.
Thinking further, we realized another issue related to the nature of ambiguous types. Say we have a handler like this:

```ts
export const my_handler = action({
  args: {
    tenant_id: v.id("tenants"),
    product_id: v.id("products"),
    stripe_subscription_id: v.string(),
    stripe_customer_id: v.string(),
    ends_on: v.number() // epoch millis
  },
  // ...
}
```

The type for the above function is:

```ts
const my_handler: RegisteredAction<"public", {
    stripe_customer_id: string;
    tenant_id: Id<"tenants">;
    stripe_subscription_id: string;
    product_id: Id<"products">;
    ends_on: number;
}, Promise<void>>
```

Previously the generated handlers, especially for **Rust**, would just use the order of parameters given by the type. So: `my_handler(stripe_customer_id, tenant_id, stripe_subscription_id, ...)`.
If we go ahead and add another field, especially a `string`, we might see a type like this:

```ts
const my_handler: RegisteredAction<"public", {
    stripe_customer_id: string;
    tenant_id: Id<"tenants">;
    x_another_string: string;
    stripe_subscription_id: string;
    product_id: Id<"products">;
    another_another_string: string;
    ends_on: number;
}, Promise<void>>
```

The generated code would previously just use this and in your client application, using the generated code, you would know to update the calling site of this function.
Your compiler/linter/type-checker would not tell you that `stripe_subscription_id` moved one spot to the right, since the types still match.
We have all been there, it's way past responsible dev time, we have to ship a feature, we refactor multiple functions.
This just might slip through the cracks of attention and suddendly we have a bug, which in this case might be a wrongly submitted `stripe_subscription_id` which only shows itself the next time one of your customers subscription has to be worked with.
Minutes/Days/Weeks?
Testing might catch things like this, but real integration tests are hard no matter the amounts of tools we have and having to fear and keep the question in mind: "Did anything get reordered due to my change" is just stupid.

### Python

We enforce using functions with named parameters, this completely eliminates the above problem. `stripe_subscription_id` stays `stripe_subscription_id`, no matter the order.

### Rust

We generate `<FunctionPrefix>ArgObject`s, which are passed to functions that receive parameters. This effectively simulates named args and solves the problem.

> [!NOTE]
> When testing, it seemed like the Convex Team already does something to help with this by interleaving args of different types where possible. It might also have been coincidence but I will still give them credit, since I am more than satisfied with the DX of Convex. It would not surprise me if they already had this in mind.
