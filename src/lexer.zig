const std = @import("std");
const token = @import("token.zig");

pub const Lexer = struct {
    input: []const u8,
    allocator: std.mem.Allocator,
    position: usize,
    readPosition: usize,
    char: u8,

    pub fn new(input: []const u8, allocator: std.mem.Allocator) Lexer {
        var l = Lexer{
            .input = input,
            .allocator = allocator,
            .position = 0,
            .readPosition = 0,
            .char = 0,
        };
        l.readChar();
        return l;
    }

    pub fn nextToken(self: *Lexer) !token.Token {
        defer self.readChar();

        const literal = try self.allocator.dupe(u8, &[1]u8{self.char});
        return token.Token{
            .type = switch (self.char) {
                '=' => .assign,
                ';' => .semicolon,
                '(' => .lparen,
                ')' => .rparen,
                ',' => .comma,
                '+' => .plus,
                '{' => .lbrace,
                '}' => .rbrace,
                0 => .eof,
                else => .illegal,
            },
            .literal = if (self.char == 0) "" else literal,
        };
    }

    fn readChar(self: *Lexer) void {
        if (self.readPosition >= self.input.len) {
            self.char = 0;
        } else {
            self.char = self.input[self.readPosition];
        }
        self.position = self.readPosition;
        self.readPosition += 1;
    }
};

test "Lexer.nextToken" {
    const TokenTest = struct {
        expectedType: token.Type,
        expectedLiteral: []const u8,
    };

    const input = "=+(){},;#";
    const tests = [_]TokenTest{
        .{ .expectedType = .assign, .expectedLiteral = "=" },
        .{ .expectedType = .plus, .expectedLiteral = "+" },
        .{ .expectedType = .lparen, .expectedLiteral = "(" },
        .{ .expectedType = .rparen, .expectedLiteral = ")" },
        .{ .expectedType = .lbrace, .expectedLiteral = "{" },
        .{ .expectedType = .rbrace, .expectedLiteral = "}" },
        .{ .expectedType = .comma, .expectedLiteral = "," },
        .{ .expectedType = .semicolon, .expectedLiteral = ";" },
        .{ .expectedType = .illegal, .expectedLiteral = "#" },
        .{ .expectedType = .eof, .expectedLiteral = "" },
    };

    var lexr = Lexer.new(input, std.heap.page_allocator);
    inline for (tests) |t| {
        const tkn = try lexr.nextToken();
        try std.testing.expectEqual(t.expectedType, tkn.type);
        try std.testing.expectEqualStrings(t.expectedLiteral, tkn.literal);
    }
}
