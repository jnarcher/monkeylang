const std = @import("std");
const Token = @import("token.zig").Token;

pub const Lexer = struct {
    allocator: std.mem.Allocator,
    input: []const u8,
    position: usize,
    readPosition: usize,
    ch: u8,

    pub fn nextToken(self: *Lexer) !Token {
        self.skipWhitespace();
        const tkn: Token = switch (self.ch) {
            '=' => blk: {
                const peek = self.peekChar();
                if (peek == '=') {
                    self.readChar();
                    break :blk .eq;
                }
                break :blk .assign;
            },
            '!' => blk: {
                const peek = self.peekChar();
                if (peek == '=') {
                    self.readChar();
                    break :blk .not_eq;
                }
                break :blk .bang;
            },
            '<' => blk: {
                const peek = self.peekChar();
                if (peek == '=') {
                    self.readChar();
                    break :blk .le;
                }
                break :blk .lt;
            },
            '>' => blk: {
                const peek = self.peekChar();
                if (peek == '=') {
                    self.readChar();
                    break :blk .ge;
                }
                break :blk .gt;
            },
            '+' => .plus,
            '-' => .minus,
            '*' => .asterisk,
            '/' => .slash,
            ',' => .comma,
            '(' => .lparen,
            ')' => .rparen,
            '{' => .lsquirley,
            '}' => .rsquirley,
            ';' => .semicolon,
            0 => .eof,
            else => blk: {
                // Identifiers and keywords
                if (isLetter(self.ch)) {
                    const word = try self.readMultipleChars(isLetter);
                    return Token.keyword(word) orelse Token{ .ident = word };
                }

                // Numbers
                if (isDigit(self.ch)) {
                    const numStr = try self.readMultipleChars(isDigit);
                    return Token{ .int = numStr };
                }

                // Illegal character
                break :blk Token{
                    .illegal = try self.allocator.dupe(u8, &[1]u8{self.ch}),
                };
            },
        };

        self.readChar();
        return tkn;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (std.ascii.isWhitespace(self.ch)) : (self.readChar()) {}
    }

    fn readChar(self: *Lexer) void {
        if (self.readPosition >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.readPosition];
        }
        self.position = self.readPosition;
        self.readPosition += 1;
    }

    /// Look ahead one character without advancing the lexer position.
    fn peekChar(self: Lexer) u8 {
        return if (self.readPosition >= self.input.len) 0 else self.input[self.readPosition];
    }

    /// Reads character from input while func(self.ch) returns true and returns
    /// the a slice containing the characters read.
    fn readMultipleChars(self: *Lexer, comptime func: fn (ch: u8) bool) ![]const u8 {
        const position = self.position;
        while (func(self.ch)) : (self.readChar()) {}
        return try self.allocator.dupe(u8, self.input[position..self.position]);
    }
};

pub fn init(input: []const u8, allocator: std.mem.Allocator) Lexer {
    var l = Lexer{
        .allocator = allocator,
        .input = input,
        .position = 0,
        .readPosition = 0,
        .ch = 0,
    };
    l.readChar();
    return l;
}

fn isLetter(c: u8) bool {
    return std.ascii.isAlphabetic(c) or 'c' == '_';
}

fn isDigit(c: u8) bool {
    return std.ascii.isDigit(c);
}

test "nextToken" {
    var input: []const u8 = undefined;

    input =
        \\let five = 5;
        \\let ten = 10;
    ;
    var tests_assignment = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .semicolon,
        .let,
        .{ .ident = "ten" },
        .assign,
        .{ .int = "10" },
        .semicolon,
        .eof,
    };
    try runTest(input, tests_assignment[0..], "ASSIGNMENT");

    input =
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\
        \\let result = add(five, ten);
    ;
    var tests_function = [_]Token{
        .let,
        .{ .ident = "add" },
        .assign,
        .function,
        .lparen,
        .{ .ident = "x" },
        .comma,
        .{ .ident = "y" },
        .rparen,
        .lsquirley,
        .{ .ident = "x" },
        .plus,
        .{ .ident = "y" },
        .semicolon,
        .rsquirley,
        .semicolon,
        .let,
        .{ .ident = "result" },
        .assign,
        .{ .ident = "add" },
        .lparen,
        .{ .ident = "five" },
        .comma,
        .{ .ident = "ten" },
        .rparen,
        .semicolon,
        .eof,
    };
    try runTest(input, tests_function[0..], "FUNCTIONS");

    input =
        \\!-/*5;
        \\5 < 10 > 5;
    ;
    var tests_random = [_]Token{
        .bang,
        .minus,
        .slash,
        .asterisk,
        .{ .int = "5" },
        .semicolon,
        .{ .int = "5" },
        .lt,
        .{ .int = "10" },
        .gt,
        .{ .int = "5" },
        .semicolon,
        .eof,
    };
    try runTest(input, tests_random[0..], "RANDOM");

    input =
        \\if (5 < 10) {
        \\  return true;
        \\} else {
        \\  return false;
        \\}
    ;
    var tests_if = [_]Token{
        ._if,
        .lparen,
        .{ .int = "5" },
        .lt,
        .{ .int = "10" },
        .rparen,
        .lsquirley,
        ._return,
        .true,
        .semicolon,
        .rsquirley,
        ._else,
        .lsquirley,
        ._return,
        .false,
        .semicolon,
        .rsquirley,
        .eof,
    };
    try runTest(input, tests_if[0..], "IF STATEMENT");

    input =
        \\10 == 10;
        \\10 != 9;
        \\10 <= 11;
        \\10 >= 3;
    ;
    var tests_double_char = [_]Token{
        .{ .int = "10" },
        .eq,
        .{ .int = "10" },
        .semicolon,
        .{ .int = "10" },
        .not_eq,
        .{ .int = "9" },
        .semicolon,
        .{ .int = "10" },
        .le,
        .{ .int = "11" },
        .semicolon,
        .{ .int = "10" },
        .ge,
        .{ .int = "3" },
        .semicolon,
        .eof,
    };
    try runTest(input, tests_double_char[0..], "DOUBLE CHARACTER TOKENS");
}

fn runTest(input: []const u8, tests: []Token, name: []const u8) !void {
    std.debug.print("\nTESTING: {s}\n", .{name});
    var lexr = init(input, std.heap.page_allocator);
    for (tests, 0..) |t, i| {
        const tkn = try lexr.nextToken();

        std.testing.expect(@intFromEnum(tkn) == @intFromEnum(t)) catch {
            std.debug.print("FAILED TEST ({d}):\n", .{i});
            std.debug.print("\texpected: {s} '{s}'\n", .{ @tagName(t), switch (t) {
                .ident, .int, .illegal => |v| v,
                else => "",
            } });
            std.debug.print("\tactual: {s} '{s}'\n", .{ @tagName(tkn), switch (tkn) {
                .ident, .int, .illegal => |v| v,
                else => "",
            } });
            std.debug.print("LEXER\n\tpos: {d}\n\tread pos: {d}\n\tch: '{c}'\n", .{ lexr.position, lexr.readPosition, lexr.ch });
        };
    }
}
