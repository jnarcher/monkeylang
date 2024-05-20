const std = @import("std");
const ast = @import("ast.zig");
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: *Lexer,

    errors: std.ArrayList([]const u8),
    currToken: Token,
    peekToken: ?Token,

    pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) Parser {
        const currToken = lexer.nextToken();
        const peekToken = lexer.nextToken();
        return Parser{
            .allocator = allocator,
            .lexer = lexer,
            .errors = std.ArrayList([]const u8).init(allocator),
            .currToken = currToken,
            .peekToken = peekToken,
        };
    }

    pub fn parseProgram(self: *Parser) !?ast.Program {
        var program = ast.Program{
            .statements = std.ArrayList(ast.Statement).init(self.allocator),
        };

        while (self.currToken != .eof) : (self.nextToken()) {
            const stmt = self.parseStatement() orelse continue;
            try program.statements.append(stmt);
        }

        return program;
    }

    pub fn peekError(self: *Parser, token: Token) !void {
        const err = try std.fmt.allocPrint(
            self.allocator,
            "expected next token to be {s}, got {s}",
            .{ token.debugString(), self.peekToken.?.debugString() },
        );
        try self.errors.append(err);
    }

    fn parseStatement(self: *Parser) ?ast.Statement {
        return switch (self.currToken) {
            .let => .{ .let = self.parseLetStatement() orelse return null },
            else => null,
        };
    }

    fn parseLetStatement(self: *Parser) ?ast.LetStatement {
        var stmt = ast.LetStatement{
            .ident = undefined,
            .value = undefined,
        };

        if (self.peekToken.? != .ident) {
            self.peekError(.{ .ident = "any" }) catch {
                std.log.err("Unable to create error...", .{});
            };
            return null;
        }
        self.nextToken();

        stmt.ident = self.currToken.ident;

        if (self.peekToken.? != .assign) {
            self.peekError(.assign) catch {
                std.log.err("Unable to create error...", .{});
            };
            return null;
        }
        self.nextToken();

        // TODO: skipping the expressions for now
        while (self.currToken == .semicolon) : (self.nextToken()) {}

        return stmt;
    }

    fn nextToken(self: *Parser) void {
        self.currToken = self.peekToken.?;
        self.peekToken = if (self.currToken == .eof) null else self.lexer.nextToken();
    }
};

// TESTS BEGIN HERE
//
const testing = std.testing;

fn testLetStatement(stmt: ast.Statement, ident_name: []const u8) !void {
    try testing.expectEqualStrings(stmt.let.ident, ident_name);
    // TODO: check for correct value
}

fn checkParserErrors(parser: Parser) !void {
    if (parser.errors.items.len == 0) return;

    std.log.warn("parser has {d} error(s)\n", .{parser.errors.items.len});

    for (parser.errors.items) |err| {
        std.log.err("parser error: {s}\n", .{err});
    }
    return error.ParserError;
}

test "let statements" {
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

    try checkParserErrors(parser);

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

test "return statements" {
    // const input =
    //     \\return 5;
    //     \\return 10;
    //     \\return add(15);
    // ;
}
