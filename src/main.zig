const std = @import("std");
const lexer = @import("./lexer.zig");

pub fn main() !void {
    const l = lexer.Lexer.new();
    const tkn = l.nextToken();

    std.debug.print("{s}", .{tkn.literal});
}

test {
    std.testing.refAllDecls(@This());
}
