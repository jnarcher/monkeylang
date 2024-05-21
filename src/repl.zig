const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const io = std.io;
const print = std.debug.print;

const PROMPT = ">> ";

pub fn start(allocator: std.mem.Allocator) !void {
    const in = std.io.getStdIn().reader();
    const out = std.io.getStdOut().writer();

    try out.print("Monkeylang REPL ('CTRL-C' to quit)\n", .{});

    while (true) {
        try out.print(PROMPT, .{});
        const s = try in.readUntilDelimiterOrEofAlloc(
            allocator,
            '\n',
            1 << 32,
        ) orelse continue;

        var lex = Lexer.init(s);
        var parser = try Parser.init(&lex, allocator);

        const program = try parser.parseProgram() orelse {
            try out.print("undefined", .{});
            continue;
        };

        if (parser.errors.items.len != 0) {
            for (try parser.errors.toOwnedSlice()) |err| {
                try out.print("parser error: \t{s}\n", .{err});
            }
            continue;
        }

        try out.print("{s}\n", .{try program.string(allocator)});
    }
}
