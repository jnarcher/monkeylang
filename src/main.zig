const std = @import("std");
const repl = @import("./repl.zig");

pub fn main() !void {
    try repl.start(std.heap.page_allocator);
}
