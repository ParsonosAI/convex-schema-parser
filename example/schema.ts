import { defineSchema, defineTable } from "convex/server";
import { v } from "convex/values";

export default defineSchema({
  users: defineTable({
    name: v.string(),
    email: v.string(),
    posts: v.array(v.id("posts")),
    profile: v.object({
      bio: v.string(),
      pictureUrl: v.string(),
    }),
  }),

  posts: defineTable({
    title: v.string(),
    authorId: v.id("users"),
    published: v.boolean(),
  }),
})
