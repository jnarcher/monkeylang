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
            .lparen => .call,
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
        try parser.registerPrefix(.true, parseBoolLiteral);
        try parser.registerPrefix(.false, parseBoolLiteral);
        try parser.registerPrefix(.lparen, parseGroupedExpression);
        try parser.registerPrefix(._if, parseIfExpression);
        try parser.registerPrefix(.function, parseFunctionLiteral);

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
        try parser.registerInfix(.lparen, parseCallExpression);

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

    fn parseBlockStatement(self: *Parser) ?ast.BlockStatement {
        var block = ast.BlockStatement{
            .statements = std.ArrayList(ast.Statement).init(self.alloc),
        };

        self.nextToken();

        while (self.currToken != .rsquirly and self.currToken != .eof) : (self.nextToken()) {
            const stmt = self.parseStatement() orelse continue;
            block.statements.append(stmt) catch {};
        }
        return block;
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
        const exp = self.newExpression() catch return null;
        exp.* = .{ .ident = ast.Identifier{ .name = self.currToken.ident } };
        return exp;
    }

    fn parseIntLiteral(self: *Parser) ?*ast.Expression {
        const expr = self.newExpression() catch return null;
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

    fn parseBoolLiteral(self: *Parser) ?*ast.Expression {
        const expr = self.newExpression() catch return null;
        expr.* = .{
            .boolean = .{
                .value = self.currToken == Token.true,
            },
        };
        return expr;
    }

    fn parseGroupedExpression(self: *Parser) ?*ast.Expression {
        self.nextToken();
        const exp = self.parseExpression(Precedence.lowest);
        if (self.peekToken.? != Token.rparen) return null;
        self.nextToken();
        return exp;
    }

    fn parsePrefixExpression(self: *Parser) ?*ast.Expression {
        var prefix = ast.Prefix{
            .operator = self.currToken,
            .right = undefined,
        };

        self.nextToken();

        var right = self.newExpression() catch return null;
        right = self.parseExpression(.prefix).?;

        prefix.right = right;
        const expr = self.newExpression() catch return null;
        expr.* = ast.Expression{ .prefix = prefix };
        return expr;
    }

    fn parseFunctionLiteral(self: *Parser) ?*ast.Expression {
        var lit = ast.FunctionLiteral{
            .params = undefined,
            .body = undefined,
        };

        if (self.peekToken.? != .lparen) {
            self.peekError(.lparen) catch {};
            return null;
        }
        self.nextToken();

        // std.debug.print("{s}\n", .{self.currToken.debugString()});

        const params = self.parseFunctionParameters() catch {
            self.errors.append("error parsing function parameters") catch {};
            return null;
        };
        lit.params = params orelse {
            self.errors.append("parsing function params returned null") catch {};
            return null;
        };

        if (self.peekToken.? != .lsquirly) {
            self.peekError(.lsquirly) catch {};
            return null;
        }
        self.nextToken();

        lit.body = self.parseBlockStatement() orelse return null;

        const exp = self.newExpression() catch return null;
        exp.* = ast.Expression{ .function = lit };
        return exp;
    }

    fn parseFunctionParameters(self: *Parser) !?std.ArrayList(ast.Identifier) {
        var idents = std.ArrayList(ast.Identifier).init(self.alloc);

        if (self.peekToken.? == .rparen) {
            self.nextToken();
            return idents;
        }

        self.nextToken();

        try idents.append(ast.Identifier{ .name = self.currToken.ident });

        while (self.peekToken.? == .comma) {
            self.nextToken();
            self.nextToken();
            try idents.append(ast.Identifier{ .name = self.currToken.ident });
        }

        if (self.peekToken.? != .rparen) {
            self.peekError(.rparen) catch {};
            return null;
        }
        self.nextToken();

        return idents;
    }

    fn parseIfExpression(self: *Parser) ?*ast.Expression {
        if (self.peekToken.? != .lparen) {
            self.peekError(.lparen) catch {};
            return null;
        }
        self.nextToken();

        self.nextToken();
        var _if = ast.IfExpression{
            .condition = self.parseExpression(.lowest) orelse return null,
            .consequence = undefined,
            .alternative = null,
        };

        if (self.peekToken.? != .rparen) {
            self.peekError(.rparen) catch {};
            return null;
        }
        self.nextToken();

        if (self.peekToken.? != .lsquirly) {
            self.peekError(.lsquirly) catch {};
            return null;
        }
        self.nextToken();

        _if.consequence = self.parseBlockStatement() orelse {
            self.errors.append("if statment consequence block is null") catch {};
            return null;
        };

        if (self.peekToken.? == ._else) {
            self.nextToken();

            if (self.peekToken.? != .lsquirly) {
                self.peekError(.lsquirly) catch {};
                return null;
            }
            self.nextToken();

            _if.alternative = self.parseBlockStatement();
        }

        const exp = self.newExpression() catch return null;
        exp.* = ast.Expression{ ._if = _if };
        return exp;
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

    fn parseCallExpression(self: *Parser, func_exp: *ast.Expression) ?*ast.Expression {
        const exp = self.newExpression() catch return null;
        exp.* = ast.Expression{
            .call = .{
                .function = func_exp,
                .arguments = self.parseCallArguments() orelse return null,
            },
        };
        return exp;
    }

    fn parseCallArguments(self: *Parser) ?std.ArrayList(*ast.Expression) {
        var args = std.ArrayList(*ast.Expression).init(self.alloc);

        if (self.peekToken.? == .rparen) {
            self.nextToken();
            return args;
        }

        self.nextToken();
        var exp = self.parseExpression(.lowest) orelse return null;
        args.append(exp) catch return null;

        while (self.peekToken.? == .comma) {
            self.nextToken();
            self.nextToken();
            exp = self.parseExpression(.lowest) orelse return null;
            args.append(exp) catch return null;
        }

        if (self.peekToken.? != .rparen) {
            self.peekError(.rparen) catch {};
            return null;
        }
        self.nextToken();

        return args;
    }

    fn newExpression(self: *Parser) !*ast.Expression {
        return self.alloc.create(ast.Expression);
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
