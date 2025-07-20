import { mutation } from "./_generated/server";
import { v } from "convex/values";

export const send = mutation({
  args: {
    author: v.string(),
    body: v.string(),
  },
  handler: async (ctx, { author, body }) => {
    await ctx.db.insert("messages", { author, body });
  },
});
