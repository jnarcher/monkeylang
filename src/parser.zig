const std = @import("std");
const ast = @import("ast.zig");
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: *Lexer,

    curToken: Token,
    peekToken: ?Token,

    pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) Parser {
        const currToken = lexer.nextToken();
        const peekToken = lexer.nextToken();
        return Parser{
            .allocator = allocator,
            .lexer = lexer,
            .curToken = currToken,
            .peekToken = peekToken,
        };
    }

    pub fn parseProgram(self: *Parser) !?ast.Program {
        var program = ast.Program{
            .statements = std.ArrayList(ast.Statement).init(self.allocator),
        };

        while (self.curToken != Token.eof) : (self.nextToken()) {
            const stmt = self.parseStatement() orelse continue;
            try program.statements.append(stmt);
        }

        return program;
    }

    fn parseStatement(self: *Parser) ?ast.Statement {
        return switch (self.curToken) {
            .let => .{ .let = self.parseLetStatement() orelse return null },
            else => null,
        };
    }

    fn parseLetStatement(self: *Parser) ?ast.LetStatement {
        var stmt = ast.LetStatement{
            .ident = undefined,
            .value = undefined,
        };

        if (self.peekToken.? != Token.ident) return null;
        self.nextToken();

        stmt.ident = self.curToken.ident;

        if (self.peekToken.? != Token.assign) return null;
        self.nextToken();

        // TODO: skipping the expressions for now
        while (self.curToken == Token.semicolon) : (self.nextToken()) {}

        return stmt;
    }

    fn nextToken(self: *Parser) void {
        self.curToken = self.peekToken.?;
        self.peekToken = if (self.curToken == Token.eof) null else self.lexer.nextToken();
    }
};

// TESTS BEGIN HERE
//
const testing = std.testing;

fn testLetStatement(stmt: ast.Statement, ident_name: []const u8) !void {
    try testing.expectEqualStrings(stmt.let.ident, ident_name);
    // TODO: check for correct value
}

test "let statements" {
    std.debug.print("\n", .{});
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(input);
    var parser = Parser.init(&lex, alloc);

    const program = try parser.parseProgram() orelse {
        std.log.warn("parseProgram() returned a null value.\n", .{});
        return error.NullProgram;
    };

    testing.expectEqual(3, program.statements.items.len) catch |err| {
        std.log.warn(
            "program.statements does not contain 3 statements. got={d}",
            .{program.statements.items.len},
        );
        return err;
    };

    const tests = [_][]const u8{
        "x",
        "y",
        "foobar",
    };

    for (tests, 0..) |t, i|
        try testLetStatement(program.statements.items[i], t);
}
