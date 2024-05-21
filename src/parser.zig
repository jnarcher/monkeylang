const std = @import("std");
const ast = @import("ast.zig");
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;

pub const PrefixParseFn = *const fn (ptr: *Parser) ?*ast.Expression;
pub const InfixParseFn = *const fn (ptr: *Parser, expr: *ast.Expression) ?*ast.Expression;

pub const Precedence = enum(u8) {
    lowest,
    equality, //     ==, !=, <=, >=
    less_greater, // > or <
    sum, //          +
    product, //      *
    prefix, //       -X or !X
    call, //         fn(X)

    pub fn tokenPrecedence(token: Token) Precedence {
        return switch (token) {
            .eq, .not_eq, .le, .ge => .equality,
            .lt, .gt => .less_greater,
            .plus, .minus => .sum,
            .slash, .asterisk => .product,
            else => .lowest,
        };
    }

    pub fn compare(a: Precedence, b: Precedence) bool {
        return @intFromEnum(a) < @intFromEnum(b);
    }

    pub fn compareSelf(self: Precedence, other: Precedence) bool {
        return compare(self, other);
    }
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

        try parser.registerInfix(.plus, parseInfixExpression);
        try parser.registerInfix(.minus, parseInfixExpression);
        try parser.registerInfix(.slash, parseInfixExpression);
        try parser.registerInfix(.asterisk, parseInfixExpression);
        try parser.registerInfix(.eq, parseInfixExpression);
        try parser.registerInfix(.not_eq, parseInfixExpression);
        try parser.registerInfix(.le, parseInfixExpression);
        try parser.registerInfix(.ge, parseInfixExpression);
        try parser.registerInfix(.lt, parseInfixExpression);
        try parser.registerInfix(.gt, parseInfixExpression);

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

    fn peekPrecedence(self: Parser) Precedence {
        return Precedence.tokenPrecedence(self.peekToken.?);
    }

    fn currPrecedence(self: Parser) Precedence {
        return Precedence.tokenPrecedence(self.currToken);
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
            .expression = self.parseExpression(.lowest) orelse return null,
        };

        if (self.peekToken.? == .semicolon) self.nextToken();

        return stmt;
    }

    fn parseExpression(self: *Parser, precedence: Precedence) ?*ast.Expression {
        const prefix = self.getPrefixFn(self.currToken) orelse {
            self.noPrefixParseFnError(self.currToken) catch {};
            return null;
        };

        var leftExp = prefix(self) orelse return null;

        while (self.peekToken.? != .semicolon and
            precedence.compareSelf(self.peekPrecedence()))
        {
            const infix = self.getInfixFn(self.peekToken.?) orelse {
                return leftExp;
            };

            self.nextToken();
            leftExp = infix(self, leftExp) orelse return leftExp;
        }

        return leftExp;
    }

    fn parseIdentifier(self: *Parser) ?*ast.Expression {
        const exp = self.alloc.create(ast.Expression) catch return null;
        exp.* = .{ .ident = ast.Identifier{ .name = self.currToken.ident } };
        return exp;
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
            .operator = self.currToken,
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

    fn noPrefixParseFnError(self: *Parser, token: Token) !void {
        try self.errors.append(
            try std.fmt.allocPrint(
                self.alloc,
                "no prefix parse function for ' {s} ' found",
                .{
                    token.literal(),
                },
            ),
        );
    }

    fn parseInfixExpression(self: *Parser, left: *ast.Expression) ?*ast.Expression {
        var infix = ast.Infix{
            .left = left,
            .operator = self.currToken,
            .right = undefined,
        };

        const precedence = self.currPrecedence();
        self.nextToken();
        infix.right = self.parseExpression(precedence) orelse return null;

        const exp = self.alloc.create(ast.Expression) catch return null;
        exp.* = ast.Expression{ .infix = infix };
        return exp;
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

fn testIntLiteral(exp: *ast.Expression, expected: i64) !void {
    try testing.expectEqual(expected, exp.int.value);
}

fn testIdentifier(exp: *ast.Expression, expected: []const u8) !void {
    try testing.expectEqualSlices(expected, exp.ident.name);
}

fn testLiteralExpression(exp: *ast.Expression, expected: anytype) !void {
    switch (@TypeOf(expected)) {
        comptime_int, i64 => try testIntLiteral(exp, expected),
        []const u8 => try testIdentifier(exp, expected),
        else => std.log.warn("type of exp not handled. got={s}", .{@typeName(@TypeOf(expected))}),
    }
}

fn testInfixExpression(exp: *ast.Expression, left: anytype, operator: Token, right: anytype) !void {
    const infix = exp.infix;
    try testLiteralExpression(infix.left, left);
    try testing.expectEqual(operator, infix.operator);
    try testLiteralExpression(infix.right, right);
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

    const exp = program
        .statements
        .items[0]
        .expression
        .expression;

    try testLiteralExpression(exp, 5);
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

        try testing.expectEqualStrings(t.operator, prefix.operator.literal());
        try testIntLiteral(prefix.right, t.value);
    }
}

test "infix operators" {
    const InfixTest = struct {
        input: []const u8,
        left: i64,
        operator: Token,
        right: i64,
    };

    const infix_tests = [_]InfixTest{
        .{ .input = "5 + 5;", .left = 5, .operator = .plus, .right = 5 },
        .{ .input = "5 - 5;", .left = 5, .operator = .minus, .right = 5 },
        .{ .input = "5 * 5;", .left = 5, .operator = .asterisk, .right = 5 },
        .{ .input = "5 / 5;", .left = 5, .operator = .slash, .right = 5 },
        .{ .input = "5 > 5;", .left = 5, .operator = .gt, .right = 5 },
        .{ .input = "5 < 5;", .left = 5, .operator = .lt, .right = 5 },
        .{ .input = "5 == 5;", .left = 5, .operator = .eq, .right = 5 },
        .{ .input = "5 != 5;", .left = 5, .operator = .not_eq, .right = 5 },
        .{ .input = "5 <= 5;", .left = 5, .operator = .le, .right = 5 },
        .{ .input = "5 >= 5;", .left = 5, .operator = .ge, .right = 5 },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    for (infix_tests) |t| {
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

        const exp = program
            .statements
            .items[0]
            .expression
            .expression;

        try testInfixExpression(exp, t.left, t.operator, t.right);
    }
}

test "operator precedence" {
    const PrecedenceTest = struct {
        input: []const u8,
        expected: []const u8,
    };

    const precedence_tests = [_]PrecedenceTest{
        .{ .input = "-a * b", .expected = "((-a) * b)" },
        .{ .input = "!-a", .expected = "(!(-a))" },
        .{ .input = "a + b + c", .expected = "((a + b) + c)" },
        .{ .input = "a * b * c", .expected = "((a * b) * c)" },
        .{ .input = "a * b / c", .expected = "((a * b) / c)" },
        .{ .input = "a + b / c", .expected = "(a + (b / c))" },
        .{ .input = "a + b * c + d / e - f", .expected = "(((a + (b * c)) + (d / e)) - f)" },
        .{ .input = "3 + 4; -5 * 5", .expected = "(3 + 4)((-5) * 5)" },
        .{ .input = "5 > 4 == 3 < 4", .expected = "((5 > 4) == (3 < 4))" },
        .{ .input = "5 < 4 != 3 > 4", .expected = "((5 < 4) != (3 > 4))" },
        .{ .input = "3 + 4 * 5 == 3 * 1 + 4 * 5", .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    for (precedence_tests) |t| {
        var lex = Lexer.init(t.input);
        var parser = try Parser.init(&lex, alloc);

        const program = try parser.parseProgram() orelse {
            std.log.warn("parseProgram() returned a null value.\n", .{});
            return error.NullProgram;
        };
        try checkParserErrors(parser);

        try testing.expectEqualStrings(t.expected, try program.string(alloc));
    }
}
