const std = @import("std");
const ast = @import("ast.zig");
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;

pub const PrefixParseFn = *const fn (ptr: *Parser) ?*ast.Expression;
pub const InfixParseFn = *const fn (ptr: *Parser, expr: *ast.Expression) ?*ast.Expression;

pub const Precedence = enum(u8) {
    lowest,
    equals, // ==
    less_grater, // > or <
    sum, // +
    product, // *
    prefix, // -X or !X
    call, // fn(X)
};

pub const Parser = struct {
    alloc: std.mem.Allocator,
    lexer: *Lexer,

    errors: std.ArrayList([]const u8),
    currToken: Token,
    peekToken: ?Token,

    prefixParseFns: std.StringHashMap(PrefixParseFn),
    infixParseFns: std.StringHashMap(InfixParseFn),

    pub fn init(lexer: *Lexer, alloc: std.mem.Allocator) !Parser {
        const currToken = lexer.nextToken();
        const peekToken = lexer.nextToken();
        var parser = Parser{
            .alloc = alloc,
            .lexer = lexer,
            .errors = std.ArrayList([]const u8).init(alloc),
            .currToken = currToken,
            .peekToken = peekToken,
            .prefixParseFns = std.StringHashMap(PrefixParseFn).init(alloc),
            .infixParseFns = std.StringHashMap(InfixParseFn).init(alloc),
        };
        try parser.registerPrefix(.{ .ident = "any" }, parseIdentifier);
        try parser.registerPrefix(.{ .int = "any" }, parseIntLiteral);
        try parser.registerPrefix(.bang, parsePrefixExpression);
        try parser.registerPrefix(.minus, parsePrefixExpression);
        return parser;
    }

    pub fn parseProgram(self: *Parser) !?ast.Program {
        var program = ast.Program{
            .statements = std.ArrayList(ast.Statement).init(self.alloc),
        };

        while (self.currToken != .eof) : (self.nextToken()) {
            const stmt = self.parseStatement() orelse continue;
            try program.statements.append(stmt);
        }

        return program;
    }

    fn peekError(self: *Parser, token: Token) !void {
        const err = try std.fmt.allocPrint(
            self.alloc,
            "expected next token to be {s}, got {s}",
            .{ token.debugString(), self.peekToken.?.debugString() },
        );
        try self.errors.append(err);
    }

    fn nextToken(self: *Parser) void {
        self.currToken = self.peekToken.?;
        self.peekToken = if (self.currToken == .eof) null else self.lexer.nextToken();
    }

    fn parseStatement(self: *Parser) ?ast.Statement {
        return switch (self.currToken) {
            .let => .{
                .let = self.parseLetStatement() orelse return null,
            },
            ._return => .{
                ._return = self.parseReturnStatement() orelse return null,
            },
            else => .{
                .expression = self.parseExpressionStatement() orelse return null,
            },
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

        stmt.ident = ast.Identifier{ .name = self.currToken.ident };

        if (self.peekToken.? != .assign) {
            self.peekError(.assign) catch {
                std.log.err("Unable to create error...", .{});
            };
            return null;
        }
        self.nextToken();

        // TODO: skipping the expressions for now
        while (self.currToken != .semicolon) : (self.nextToken()) {}

        return stmt;
    }

    fn parseReturnStatement(self: *Parser) ?ast.ReturnStatment {
        const stmt = ast.ReturnStatment{ .value = undefined };

        self.nextToken();

        // TODO: skipping value for now

        while (self.currToken != .semicolon) : (self.nextToken()) {}

        return stmt;
    }

    fn parseExpressionStatement(self: *Parser) ?ast.ExpressionStatement {
        const stmt = ast.ExpressionStatement{
            .expression = self.parseExpression(.lowest).?,
        };

        if (self.peekToken.? == .semicolon) self.nextToken();

        return stmt;
    }

    fn parseExpression(self: *Parser, _: Precedence) ?*ast.Expression {
        const prefix = self.getPrefixFn(self.currToken).?;
        const leftExp = prefix(self);
        return leftExp;
    }

    fn parseIdentifier(self: *Parser) ?*ast.Expression {
        const expr = self.alloc.create(ast.Expression) catch return null;
        expr.* = .{ .ident = ast.Identifier{ .name = self.currToken.ident } };
        return expr;
    }

    fn parseIntLiteral(self: *Parser) ?*ast.Expression {
        const expr = self.alloc.create(ast.Expression) catch return null;
        expr.* = .{
            .int = .{
                .value = std.fmt.parseInt(i64, self.currToken.int, 10) catch blk: {
                    self.errors.append("unable to parse integer") catch {};
                    break :blk 0;
                },
            },
        };
        return expr;
    }

    fn parsePrefixExpression(self: *Parser) ?*ast.Expression {
        var prefix = ast.Prefix{
            .operator = self.currToken.literal(),
            .right = undefined,
        };

        self.nextToken();

        var right = self.alloc.create(ast.Expression) catch return null;
        right = self.parseExpression(.prefix).?;

        prefix.right = right;
        const expr = self.alloc.create(ast.Expression) catch return null;
        expr.* = ast.Expression{ .prefix = prefix };
        return expr;
    }

    fn registerPrefix(self: *Parser, token: Token, func: PrefixParseFn) !void {
        return self.prefixParseFns.put(@tagName(token), func);
    }

    fn getPrefixFn(self: *Parser, token: Token) ?PrefixParseFn {
        return self.prefixParseFns.get(@tagName(token));
    }

    fn registerInfix(self: *Parser, token: Token, func: InfixParseFn) !void {
        return self.infixParseFns.put(@tagName(token), func);
    }

    fn getInfixFn(self: *Parser, token: Token) ?InfixParseFn {
        return self.infixParseFns.get(@tagName(token));
    }
};

// TESTS BEGIN HERE

const testing = std.testing;

fn testLetStatement(stmt: ast.Statement, ident_name: []const u8) !void {
    try testing.expectEqualStrings(stmt.let.ident.name, ident_name);
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
    var parser = try Parser.init(&lex, alloc);

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
    const input =
        \\return 5;
        \\return 10;
        \\return add(15);
    ;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(input);
    var parser = try Parser.init(&lex, alloc);

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

    for (program.statements.items) |stmt| {
        return switch (stmt) {
            ._return => {},
            else => error.IncorrectStatementType,
        };
    }
}

test "identifier expression" {
    const input =
        \\foobar;
    ;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(input);
    var parser = try Parser.init(&lex, alloc);

    const program = try parser.parseProgram() orelse {
        std.log.warn("parseProgram() returned a null value.\n", .{});
        return error.NullProgram;
    };

    try checkParserErrors(parser);

    testing.expectEqual(1, program.statements.items.len) catch |err| {
        std.log.warn(
            "program.statements does not contain 1 statement. got={d}",
            .{program.statements.items.len},
        );
        return err;
    };

    const name = program
        .statements
        .items[0]
        .expression
        .expression
        .ident
        .name;

    try testing.expectEqualStrings("foobar", name);
}

fn testIntLiteral(expr: ast.Expression, expected: i64) !void {
    try testing.expectEqual(expected, expr.int.value);
}

test "int literal expression" {
    const input =
        \\5;
    ;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(input);
    var parser = try Parser.init(&lex, alloc);

    const program = try parser.parseProgram() orelse {
        std.log.warn("parseProgram() returned a null value.\n", .{});
        return error.NullProgram;
    };

    try checkParserErrors(parser);

    testing.expectEqual(1, program.statements.items.len) catch |err| {
        std.log.warn(
            "program.statements does not contain 1 statement. got={d}",
            .{program.statements.items.len},
        );
        return err;
    };

    try testIntLiteral(program.statements.items[0].expression.expression.*, 5);
}

test "prefix operators" {
    const PrefixTest = struct {
        input: []const u8,
        operator: []const u8,
        value: i64,
    };

    const prefix_tests = [_]PrefixTest{
        .{ .input = "!5;", .operator = "!", .value = 5 },
        .{ .input = "-15;", .operator = "-", .value = 15 },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    for (prefix_tests) |t| {
        var lex = Lexer.init(t.input);
        var parser = try Parser.init(&lex, alloc);

        const program = try parser.parseProgram() orelse {
            std.log.warn("parseProgram() returned a null value.\n", .{});
            return error.NullProgram;
        };

        try checkParserErrors(parser);

        testing.expectEqual(1, program.statements.items.len) catch |err| {
            std.log.warn(
                "program.statements does not contain 1 statement. got={d}",
                .{program.statements.items.len},
            );
            return err;
        };

        const prefix = program
            .statements
            .items[0]
            .expression
            .expression
            .prefix;

        try testing.expectEqualStrings(t.operator, prefix.operator);
        try testIntLiteral(prefix.right.*, t.value);
    }
}
