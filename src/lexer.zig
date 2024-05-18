const std = @import("std");
const Token = @import("token.zig").Token;

pub const Lexer = struct {
    const Self = @This();

    input: []const u8,
    position: usize = 0,
    readPosition: usize = 0,
    ch: u8 = 0,

    pub fn init(input: []const u8) Self {
        var lex = Self{ .input = input };
        lex.readChar();
        return lex;
    }

    pub fn nextToken(self: *Self) Token {
        self.skipWhitespace();
        const tkn: Token = switch (self.ch) {
            0 => .eof,
            '+' => .plus,
            '-' => .minus,
            '*' => .asterisk,
            '/' => .slash,
            ',' => .comma,
            '(' => .lparen,
            ')' => .rparen,
            '{' => .lsquirly,
            '}' => .rsquirly,
            ';' => .semicolon,
            '=' => if (self.readIfMatchPeek('=')) .eq else .assign,
            '!' => if (self.readIfMatchPeek('=')) .not_eq else .bang,
            '<' => if (self.readIfMatchPeek('=')) .le else .lt,
            '>' => if (self.readIfMatchPeek('=')) .ge else .gt,
            'a'...'z', 'A'...'Z', '_' => {
                const word = self.readMany(isLetter);
                return Token.keyword(word) orelse .{ .ident = word };
            },
            '0'...'9' => return .{ .int = self.readMany(isDigit) },
            else => .illegal,
        };

        self.readChar();
        return tkn;
    }

    fn skipWhitespace(self: *Self) void {
        while (std.ascii.isWhitespace(self.ch)) : (self.readChar()) {}
    }

    fn readChar(self: *Self) void {
        if (self.readPosition >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.readPosition];
        }
        self.position = self.readPosition;
        self.readPosition += 1;
    }

    /// Look ahead one character without advancing the lexer position.
    fn peekChar(self: Self) u8 {
        return if (self.readPosition >= self.input.len) 0 else self.input[self.readPosition];
    }

    /// Reads character from input while isValid(self.ch) returns true.
    /// Returns a slice containing the characters read.
    fn readMany(self: *Self, comptime isValid: fn (ch: u8) bool) []const u8 {
        const position = self.position;
        while (isValid(self.ch)) : (self.readChar()) {}
        return self.input[position..self.position];
    }

    /// Read the next character if `c1 == c2`.
    /// Returns whether the characters matched.
    fn readIfMatch(self: *Self, c1: u8, c2: u8) bool {
        const match = c1 == c2;
        if (match) self.readChar();
        return match;
    }

    /// Read the next character if `c` matches the peek character.
    /// Returns whether the characters matched.
    fn readIfMatchPeek(self: *Self, c: u8) bool {
        return self.readIfMatch(self.peekChar(), c);
    }
};

fn isLetter(c: u8) bool {
    return std.ascii.isAlphabetic(c) or 'c' == '_';
}

fn isDigit(c: u8) bool {
    return std.ascii.isDigit(c);
}

// TESTS START HERE

fn runTest(input: []const u8, tests: []Token, name: []const u8) !void {
    std.debug.print("{s}\n", .{name});
    var lex = Lexer.init(input);
    for (tests, 0..) |tst, i| {
        const tok = lex.nextToken();
        std.testing.expectEqualDeep(tst, tok) catch {
            std.debug.print(
                \\TEST FAILED ({d})
                \\INPUT:
                \\{s}
                \\TOKENS:
                \\  expected {s}
                \\  actual   {s}
                \\
            ,
                .{ i, input, tst.debugString(), tok.debugString() },
            );
            return error.FAIL;
        };
    }
}

test "Lexer" {
    std.debug.print("\n", .{});

    var input: []const u8 = undefined;

    input =
        \\let five = 5;
        \\let ten = 10;
    ;
    var assign_tokens = [_]Token{
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
    try runTest(input, assign_tokens[0..], "ASSIGNMENT");

    input =
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\
        \\let result = add(five, ten);
    ;
    var function_tokens = [_]Token{
        .let,
        .{ .ident = "add" },
        .assign,
        .function,
        .lparen,
        .{ .ident = "x" },
        .comma,
        .{ .ident = "y" },
        .rparen,
        .lsquirly,
        .{ .ident = "x" },
        .plus,
        .{ .ident = "y" },
        .semicolon,
        .rsquirly,
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
    try runTest(input, function_tokens[0..], "FUNCTIONS");

    input =
        \\!-/*5~;
        \\5 < 10 > 5;
    ;
    var misc_tokens = [_]Token{
        .bang,
        .minus,
        .slash,
        .asterisk,
        .{ .int = "5" },
        .illegal,
        .semicolon,
        .{ .int = "5" },
        .lt,
        .{ .int = "10" },
        .gt,
        .{ .int = "5" },
        .semicolon,
        .eof,
    };
    try runTest(input, misc_tokens[0..], "MISC");

    input =
        \\if (5 < 10) {
        \\  return true;
        \\} else {
        \\  return false;
        \\}
    ;
    var if_tokens = [_]Token{
        ._if,
        .lparen,
        .{ .int = "5" },
        .lt,
        .{ .int = "10" },
        .rparen,
        .lsquirly,
        ._return,
        .true,
        .semicolon,
        .rsquirly,
        ._else,
        .lsquirly,
        ._return,
        .false,
        .semicolon,
        .rsquirly,
        .eof,
    };
    try runTest(input, if_tokens[0..], "IF STATEMENT");

    input =
        \\10 == 10;
        \\10 != 9;
        \\10 <= 11;
        \\10 >= 3;
    ;
    var double_tokens = [_]Token{
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
    try runTest(input, double_tokens[0..], "DOUBLE CHARACTER");
}
