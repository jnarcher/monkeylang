const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const io = std.io;
const print = std.debug.print;

const PROMPT = ">> ";

pub fn start() !void {
    const in = std.io.getStdIn().reader();
    const out = std.io.getStdOut().writer();

    try out.print("Monkeylang REPL ('exit' or 'CTRL-C' to quit)\n", .{});

    while (true) {
        try out.print(PROMPT, .{});
        const s = try in.readUntilDelimiterOrEofAlloc(
            std.heap.page_allocator,
            '\n',
            1 << 32,
        ) orelse continue;

        if (std.mem.eql(u8, s, "exit")) {
            try out.print("Quitting...\n", .{});
            return;
        }

        var lex = Lexer.init(s);
        var tok = lex.nextToken();
        while (tok != Token.eof) : (tok = lex.nextToken()) {
            try out.print("{s}\n", .{tok.debugString()});
        }
    }
}
