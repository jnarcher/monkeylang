const std = @import("std");
const token = @import("token.zig");

pub const Lexer = struct {
    input: []const u8,
    allocator: std.mem.Allocator,
    position: usize,
    readPosition: usize,
    ch: u8,

    pub fn new(input: []const u8, allocator: std.mem.Allocator) Lexer {
        var l = Lexer{
            .input = input,
            .allocator = allocator,
            .position = 0,
            .readPosition = 0,
            .ch = 0,
        };
        l.readChar();
        return l;
    }

    pub fn nextToken(self: *Lexer) !token.Token {
        self.skipWhitespace();

        const tkn: token.Token = switch (self.ch) {
            '=' => blk: {
                const peek = self.peekChar();
                if (peek == '=') {
                    self.readChar();
                    break :blk .{
                        .type = .equal,
                        .literal = "==",
                    };
                } else {
                    break :blk .{
                        .type = .assign,
                        .literal = "=",
                    };
                }
            },
            '!' => blk: {
                const peek = self.peekChar();
                if (peek == '=') {
                    self.readChar();
                    break :blk .{
                        .type = .not_equal,
                        .literal = "!=",
                    };
                } else {
                    break :blk .{
                        .type = .bang,
                        .literal = "!",
                    };
                }
            },
            '+' => .{ .type = .plus, .literal = "+" },
            '-' => .{ .type = .minus, .literal = "-" },
            '*' => .{ .type = .asterisk, .literal = "*" },
            '/' => .{ .type = .slash, .literal = "/" },
            '<' => .{ .type = .less_than, .literal = "<" },
            '>' => .{ .type = .greater_than, .literal = ">" },
            ',' => .{ .type = .comma, .literal = "," },
            '(' => .{ .type = .paren_l, .literal = "(" },
            ')' => .{ .type = .paren_r, .literal = ")" },
            '{' => .{ .type = .brace_l, .literal = "{" },
            '}' => .{ .type = .brace_r, .literal = "}" },
            ';' => .{ .type = .semicolon, .literal = ";" },
            0 => .{ .type = .eof, .literal = "EOF" },
            else => blk: {
                if (isLetter(self.ch)) { // IDENTIFIERS & KEYWORDS
                    var tkn = token.Token{
                        .type = undefined,
                        .literal = try self.readMultipleChars(isLetter),
                    };
                    tkn.type = token.lookupIdent(tkn.literal);
                    return tkn;
                } else if (isDigit(self.ch)) { // NUMBERS
                    return .{
                        .type = .int,
                        .literal = try self.readMultipleChars(isDigit),
                    };
                }

                break :blk .{
                    .type = .illegal,
                    .literal = try self.allocator.dupe(u8, &[1]u8{self.ch}),
                };
            },
        };

        self.readChar();
        return tkn;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') : (self.readChar()) {}
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

fn isLetter(ch: u8) bool {
    return ('a' <= ch and ch <= 'z') or ('A' <= ch and ch <= 'Z') or (ch == '_');
}

fn isDigit(ch: u8) bool {
    return '0' <= ch and ch <= '9';
}

test "nextToken" {
    const TokenTest = struct {
        expectedType: token.Type,
        expectedLiteral: []const u8,
    };

    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\  return true;
        \\} else {
        \\  return false;
        \\}
        \\
        \\10 == 10;
        \\10 != 9;
    ;

    const tests = [_]TokenTest{
        // let five = 5;
        .{ .expectedType = .let, .expectedLiteral = "let" },
        .{ .expectedType = .identifier, .expectedLiteral = "five" },
        .{ .expectedType = .assign, .expectedLiteral = "=" },
        .{ .expectedType = .int, .expectedLiteral = "5" },
        .{ .expectedType = .semicolon, .expectedLiteral = ";" },
        // let ten = 10;
        .{ .expectedType = .let, .expectedLiteral = "let" },
        .{ .expectedType = .identifier, .expectedLiteral = "ten" },
        .{ .expectedType = .assign, .expectedLiteral = "=" },
        .{ .expectedType = .int, .expectedLiteral = "10" },
        .{ .expectedType = .semicolon, .expectedLiteral = ";" },
        // let add = fn(x, y) { x + y; };
        .{ .expectedType = .let, .expectedLiteral = "let" },
        .{ .expectedType = .identifier, .expectedLiteral = "add" },
        .{ .expectedType = .assign, .expectedLiteral = "=" },
        .{ .expectedType = .function, .expectedLiteral = "fn" },
        .{ .expectedType = .paren_l, .expectedLiteral = "(" },
        .{ .expectedType = .identifier, .expectedLiteral = "x" },
        .{ .expectedType = .comma, .expectedLiteral = "," },
        .{ .expectedType = .identifier, .expectedLiteral = "y" },
        .{ .expectedType = .paren_r, .expectedLiteral = ")" },
        .{ .expectedType = .brace_l, .expectedLiteral = "{" },
        .{ .expectedType = .identifier, .expectedLiteral = "x" },
        .{ .expectedType = .plus, .expectedLiteral = "+" },
        .{ .expectedType = .identifier, .expectedLiteral = "y" },
        .{ .expectedType = .semicolon, .expectedLiteral = ";" },
        .{ .expectedType = .brace_r, .expectedLiteral = "}" },
        .{ .expectedType = .semicolon, .expectedLiteral = ";" },
        // let result = add(five, ten);
        .{ .expectedType = .let, .expectedLiteral = "let" },
        .{ .expectedType = .identifier, .expectedLiteral = "result" },
        .{ .expectedType = .assign, .expectedLiteral = "=" },
        .{ .expectedType = .identifier, .expectedLiteral = "add" },
        .{ .expectedType = .paren_l, .expectedLiteral = "(" },
        .{ .expectedType = .identifier, .expectedLiteral = "five" },
        .{ .expectedType = .comma, .expectedLiteral = "," },
        .{ .expectedType = .identifier, .expectedLiteral = "ten" },
        .{ .expectedType = .paren_r, .expectedLiteral = ")" },
        .{ .expectedType = .semicolon, .expectedLiteral = ";" },
        // !-/*5;
        .{ .expectedType = .bang, .expectedLiteral = "!" },
        .{ .expectedType = .minus, .expectedLiteral = "-" },
        .{ .expectedType = .slash, .expectedLiteral = "/" },
        .{ .expectedType = .asterisk, .expectedLiteral = "*" },
        .{ .expectedType = .int, .expectedLiteral = "5" },
        .{ .expectedType = .semicolon, .expectedLiteral = ";" },
        // 5 < 10 > 5;
        .{ .expectedType = .int, .expectedLiteral = "5" },
        .{ .expectedType = .less_than, .expectedLiteral = "<" },
        .{ .expectedType = .int, .expectedLiteral = "10" },
        .{ .expectedType = .greater_than, .expectedLiteral = ">" },
        .{ .expectedType = .int, .expectedLiteral = "5" },
        .{ .expectedType = .semicolon, .expectedLiteral = ";" },
        // if (5 < 10) {
        //   return true;
        // } else {
        //   return false;
        // }
        .{ .expectedType = ._if, .expectedLiteral = "if" },
        .{ .expectedType = .paren_l, .expectedLiteral = "(" },
        .{ .expectedType = .int, .expectedLiteral = "5" },
        .{ .expectedType = .less_than, .expectedLiteral = "<" },
        .{ .expectedType = .int, .expectedLiteral = "10" },
        .{ .expectedType = .paren_r, .expectedLiteral = ")" },
        .{ .expectedType = .brace_l, .expectedLiteral = "{" },
        .{ .expectedType = ._return, .expectedLiteral = "return" },
        .{ .expectedType = .true, .expectedLiteral = "true" },
        .{ .expectedType = .semicolon, .expectedLiteral = ";" },
        .{ .expectedType = .brace_r, .expectedLiteral = "}" },
        .{ .expectedType = ._else, .expectedLiteral = "else" },
        .{ .expectedType = .brace_l, .expectedLiteral = "{" },
        .{ .expectedType = ._return, .expectedLiteral = "return" },
        .{ .expectedType = .false, .expectedLiteral = "false" },
        .{ .expectedType = .semicolon, .expectedLiteral = ";" },
        .{ .expectedType = .brace_r, .expectedLiteral = "}" },
        // 10 == 10;
        .{ .expectedType = .int, .expectedLiteral = "10" },
        .{ .expectedType = .equal, .expectedLiteral = "==" },
        .{ .expectedType = .int, .expectedLiteral = "10" },
        .{ .expectedType = .semicolon, .expectedLiteral = ";" },
        // 10 != 9;
        .{ .expectedType = .int, .expectedLiteral = "10" },
        .{ .expectedType = .not_equal, .expectedLiteral = "!=" },
        .{ .expectedType = .int, .expectedLiteral = "9" },
        .{ .expectedType = .semicolon, .expectedLiteral = ";" },
        // end
        .{ .expectedType = .eof, .expectedLiteral = "EOF" },
    };

    std.debug.print("\n", .{});
    var lexr = Lexer.new(input, std.heap.page_allocator);
    std.debug.print("INPUT ========================================\n{s}\n========================================\n", .{lexr.input});
    std.debug.print(
        "LEXER:\n\tposition - {d}\n\tread position - {d}\n\tchar - '{c}'\n",
        .{ lexr.position, lexr.readPosition, lexr.ch },
    );
    inline for (tests) |t| {
        std.debug.print("\nNEXT TOKEN\n--------------------\n", .{});
        const tkn = try lexr.nextToken();
        std.debug.print("TOKEN ({s}): '{s}'\n", .{ tkn.type.toString(), tkn.literal });
        std.debug.print(
            "LEXER:\n\tposition - {d}\n\tread position - {d}\n\tchar - '{c}'\n",
            .{ lexr.position, lexr.readPosition, lexr.ch },
        );
        try std.testing.expectEqual(t.expectedType, tkn.type);
        try std.testing.expectEqualStrings(t.expectedLiteral, tkn.literal);
    }
}
