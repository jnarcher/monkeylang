const std = @import("std");
const token = @import("./token.zig");

pub const Lexer = struct {
    literal: []const u8,

    pub fn new() Lexer {
        return Lexer{ .literal = ";" };
    }
    pub fn nextToken(self: Lexer) token.Token {
        return token.Token{ .filename = "test", .col = 0, .row = 0, .type = token.TokenType.SEMICOLON, .literal = self.literal };
    }
};

test "nextToken" {
    const TokenTest = struct {
        expectedType: token.TokenType,
        expectedLiteral: []const u8,
    };
    // const input = "=+(){},;";

    const tests = [_]TokenTest{
        .{ .expectedType = token.TokenType.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = token.TokenType.PLUS, .expectedLiteral = "+" },
        .{ .expectedType = token.TokenType.LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = token.TokenType.RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = token.TokenType.LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = token.TokenType.RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = token.TokenType.COMMA, .expectedLiteral = "," },
        .{ .expectedType = token.TokenType.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenType.EOF, .expectedLiteral = "" },
    };

    const l = Lexer.new();

    inline for (tests, 0..) |t, i| {
        const tok = l.nextToken();

        std.testing.expectEqual(tok.type, t.expectedType) catch {
            std.debug.print("tests[{d}] - type wrong. expected=`{s}`, got=`{s}`\n", .{ i, t.expectedType.toString(), tok.type.toString() });
        };

        std.testing.expect(std.mem.eql(u8, tok.literal, t.expectedLiteral)) catch {
            std.debug.print("tests[{d}] - literal wrong. expected=`{s}`, got=`{s}`\n\n", .{ i, t.expectedLiteral, tok.literal });
        };
    }
}
