const std = @import("std");
const token = @import("../token/token.zig");
const testing = std.testing;

const Lexer = struct {
    pub fn new() Lexer {
        return Lexer{};
    }
    pub fn nextToken() token.Token {
        return Lexer{};
    }
};

test "nextToken" {
    const input = "=+(){},;";

    const tests = [_]struct {
        expectedType: token.TokenType,
        expectedLiteral: *const []u8,
    }{
        .{ token.ASSIGN, "=" },
        .{ token.PLUS, "+" },
        .{ token.LPAREM, "(" },
        .{ token.RPAREN, ")" },
        .{ token.LBRACE, "{" },
        .{ token.RBRACE, "}" },
        .{ token.COMMA, "," },
        .{ token.SEMICOLON, ";" },
        .{ token.EOF, "" },
    };

    const l = Lexer.new(input);

    inline for (tests) |t| {
        const tok = l.nextToken();
        testing.expectEqual(tok.type, t.expectedType);
        testing.expectEqual(tok.literal, t.expectedLiteral);
    }
}
