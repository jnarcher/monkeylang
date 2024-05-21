const std = @import("std");
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");

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
    try testing.expectEqualStrings(expected, exp.ident.name);
}

fn testBooleanLiteral(exp: *ast.Expression, expected: bool) !void {
    try testing.expectEqual(expected, exp.boolean.value);
}

fn testLiteralExpression(exp: *ast.Expression, expected: anytype) !void {
    switch (@typeInfo(@TypeOf(expected))) {
        .Int, .ComptimeInt => try testIntLiteral(exp, expected),
        .Array, .Pointer => try testIdentifier(exp, expected),
        .Bool => try testBooleanLiteral(exp, expected),
        else => {
            std.log.warn("type of exp not handled. got={}\n", .{@TypeOf(expected)});
            std.log.warn("type info: {}\n", .{@typeInfo(@TypeOf(expected))});
        },
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

        const exp = program
            .statements
            .items[0]
            .expression
            .expression;

        try testing.expectEqualStrings(t.operator, exp.prefix.operator.literal());
        try testLiteralExpression(exp.prefix.right, t.value);
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
        .{
            .input = "-a * b",
            .expected = "((-a) * b)",
        },
        .{
            .input = "!-a",
            .expected = "(!(-a))",
        },
        .{
            .input = "a + b + c",
            .expected = "((a + b) + c)",
        },
        .{
            .input = "a * b * c",
            .expected = "((a * b) * c)",
        },
        .{
            .input = "a * b / c",
            .expected = "((a * b) / c)",
        },
        .{
            .input = "a + b / c",
            .expected = "(a + (b / c))",
        },
        .{
            .input = "a + b * c + d / e - f",
            .expected = "(((a + (b * c)) + (d / e)) - f)",
        },
        .{
            .input = "3 + 4; -5 * 5",
            .expected = "(3 + 4)((-5) * 5)",
        },
        .{
            .input = "5 > 4 == 3 < 4",
            .expected = "((5 > 4) == (3 < 4))",
        },
        .{
            .input = "5 < 4 != 3 > 4",
            .expected = "((5 < 4) != (3 > 4))",
        },
        .{
            .input = "3 + 4 * 5 == 3 * 1 + 4 * 5",
            .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        .{
            .input = "3 > 5 == false",
            .expected = "((3 > 5) == false)",
        },
        .{
            .input = "3 < 5 == true",
            .expected = "((3 < 5) == true)",
        },
        .{
            .input = "a + add(b * c) + d",
            .expected = "((a + add((b * c))) + d)",
        },
        .{
            .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        },
        .{
            .input = "add(a + b + c * d / f + g)",
            .expected = "add((((a + b) + ((c * d) / f)) + g))",
        },
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

test "grouped operator precedence" {
    const PrecedenceTest = struct {
        input: []const u8,
        expected: []const u8,
    };

    const precedence_tests = [_]PrecedenceTest{
        .{
            .input = "1 + (2 + 3) + 4",
            .expected = "((1 + (2 + 3)) + 4)",
        },
        .{
            .input = "(5 + 5) * 2",
            .expected = "((5 + 5) * 2)",
        },
        .{
            .input = "-(5 + 5)",
            .expected = "(-(5 + 5))",
        },
        .{
            .input = "!(true == true)",
            .expected = "(!(true == true))",
        },
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

test "if expression" {
    const input = "if (x < y) { x }";

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

    const _if = switch (program.statements.items[0].expression.expression.*) {
        ._if => |v| v,
        else => return error.NotIfExpression,
    };

    try testInfixExpression(_if.condition, "x", .lt, "y");

    try testing.expectEqual(1, _if.consequence.statements.items.len);

    const consequence = switch (_if.consequence.statements.items[0]) {
        .expression => |v| v,
        else => return error.NotExpressionStatement,
    };

    try testIdentifier(consequence.expression, "x");

    if (_if.alternative) |_| return error.UnexpectedElseBlock;
}

test "if-else expression" {
    const input = "if (left < right) { left } else { right }";

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

    const _if = switch (program.statements.items[0].expression.expression.*) {
        ._if => |v| v,
        else => return error.NotIfExpression,
    };

    try testInfixExpression(_if.condition, "left", .lt, "right");

    try testing.expectEqual(1, _if.consequence.statements.items.len);

    const consequence = switch (_if.consequence.statements.items[0]) {
        .expression => |v| v,
        else => return error.ConsequenceNotExpressionStatement,
    };

    try testIdentifier(consequence.expression, "left");

    const alt_block = _if.alternative orelse return error.AlternativeNull;

    const alternative = switch (alt_block.statements.items[0]) {
        .expression => |v| v,
        else => return error.AlternativeNotExpressionStatement,
    };

    try testIdentifier(alternative.expression, "right");
}

test "function literal expression" {
    const input = "fn(x, y) { x + y; }";

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

    const function = switch (program.statements.items[0].expression.expression.*) {
        .function => |v| v,
        else => return error.NotIfExpression,
    };

    var param1 = ast.Expression{ .ident = function.params.items[0] };
    var param2 = ast.Expression{ .ident = function.params.items[1] };

    try testIdentifier(&param1, "x");
    try testIdentifier(&param2, "y");

    try testing.expectEqual(1, function.body.statements.items.len);

    const body = switch (function.body.statements.items[0]) {
        .expression => |v| v,
        else => return error.BodyNotExpressionStatement,
    };

    try testInfixExpression(body.expression, "x", .plus, "y");
}

test "function parameter parsing" {
    const Param = struct {
        name: []const u8,
    };

    const Test = struct {
        input: []const u8,
        expectedParams: []Param,
    };

    var params1 = [_]Param{};
    var params2 = [_]Param{
        .{ .name = "x" },
    };
    var params3 = [_]Param{
        .{ .name = "x" },
        .{ .name = "y" },
        .{ .name = "z" },
    };

    const tests = [_]Test{
        .{ .input = "fn() {};", .expectedParams = &params1 },
        .{
            .input = "fn(x) {};",
            .expectedParams = &params2,
        },
        .{
            .input = "fn(x, y, z) {};",
            .expectedParams = &params3,
        },
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    for (tests) |t| {
        var lex = Lexer.init(t.input);
        var parser = try Parser.init(&lex, alloc);

        const program = try parser.parseProgram() orelse {
            std.log.warn("parseProgram() returned a null value.\n", .{});
            return error.NullProgram;
        };
        try checkParserErrors(parser);

        const function = switch (program.statements.items[0].expression.expression.*) {
            .function => |v| v,
            else => return error.NotIfExpression,
        };

        try testing.expectEqual(t.expectedParams.len, function.params.items.len);

        for (function.params.items, t.expectedParams) |param, expected| {
            var exp = ast.Expression{
                .ident = ast.Identifier{
                    .name = param.name,
                },
            };
            try testIdentifier(&exp, expected.name);
        }
    }
}

test "function call parsing" {
    const input = "add(1, 2 * 3, 4 + 5);";

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var lex = Lexer.init(input);
    var parser = try Parser.init(&lex, alloc);

    const program = try parser.parseProgram() orelse {
        std.log.err("program is null", .{});
        return error.NullProgram;
    };
    try checkParserErrors(parser);

    try testing.expectEqual(1, program.statements.items.len);

    const stmt = program
        .statements
        .items[0];

    const call_exp = switch (stmt.expression.expression.*) {
        .call => |v| v,
        else => return error.NoCallExpression,
    };

    try testIdentifier(call_exp.function, "add");

    try testing.expectEqual(3, call_exp.arguments.items.len);

    try testLiteralExpression(call_exp.arguments.items[0], 1);
    try testInfixExpression(call_exp.arguments.items[1], 2, .asterisk, 3);
    try testInfixExpression(call_exp.arguments.items[2], 4, .plus, 5);
}
