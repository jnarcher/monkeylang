const std = @import("std");
const lexer = @import("lexer.zig");
const Token = @import("token.zig").Token;
const io = std.io;
const print = std.debug.print;

const PROMPT = ">> ";

pub fn start(allocator: std.mem.Allocator) !void {
    const in = std.io.getStdIn().reader();
    const out = std.io.getStdOut().writer();

    try out.print("Monkeylang REPL ('exit' or 'CTRL-C' to quit)\n", .{});

    while (true) {
        try out.print(PROMPT, .{});
        const s = try in.readUntilDelimiterOrEofAlloc(allocator, '\n', 255) orelse continue;

        if (std.mem.eql(u8, s, "exit")) {
            try out.print("Quitting...\n", .{});
            return;
        }

        var lexr = lexer.init(s, allocator);
        var tkn = try lexr.nextToken();
        while (tkn != Token.eof) : (tkn = try lexr.nextToken()) {
            var buf: [255]u8 = undefined;
            try out.print("{s}\n", .{try tkn.toString(buf[0..])});
        }
    }
}
