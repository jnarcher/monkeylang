const std = @import("std");

pub const Type = enum {
    illegal,
    eof,
    // Identifiers + literals
    identifier, // add, foobar, x, y, ...
    int, // 213423423
    // Operators
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    less_than,
    greater_than,
    equal,
    not_equal,
    // Delimiters
    comma,
    semicolon,
    // Surround + scope
    paren_l,
    paren_r,
    brace_l,
    brace_r,
    // Keywords
    function,
    let,
    true,
    false,
    _if,
    _else,
    _return,

    pub fn toString(self: Type) []const u8 {
        return switch (self) {
            .illegal => "ILLEGAL",
            .eof => "EOF",
            .identifier => "IDENTIFIER",
            .int => "INT",
            .assign => "ASSIGN",
            .plus => "PLUS",
            .minus => "MINUS",
            .equal => "EQUAL",
            .not_equal => "NOT EQUAL",
            .bang => "BANG",
            .asterisk => "ASTERISK",
            .slash => "SLASH",
            .less_than => "LESS THAN",
            .greater_than => "GREATER THAN",
            .comma => "COMMA",
            .semicolon => "SEMICOLON",
            .paren_l => "PAREN LEFT",
            .paren_r => "PAREN RIGHT",
            .brace_l => "BRACE LEFT",
            .brace_r => "BRACE RIGHT",
            .function => "FUNCTION",
            .let => "LET",
            .true => "TRUE",
            .false => "FALSE",
            ._if => "IF",
            ._else => "ELSE",
            ._return => "RETURN",
        };
    }
};

pub const Token = struct {
    type: Type,
    literal: []const u8,

    pub fn toString(self: Token) ![]const u8 {
        return try std.fmt.allocPrint(
            std.heap.page_allocator,
            "Token ({s}): `{s}`",
            .{ self.type.toString(), self.literal },
        );
    }
};

pub fn lookupIdent(ident: []const u8) Type {
    return if (std.mem.eql(u8, ident, "fn"))
        .function
    else if (std.mem.eql(u8, ident, "let"))
        .let
    else if (std.mem.eql(u8, ident, "true"))
        .true
    else if (std.mem.eql(u8, ident, "false"))
        .false
    else if (std.mem.eql(u8, ident, "if"))
        ._if
    else if (std.mem.eql(u8, ident, "else"))
        ._else
    else if (std.mem.eql(u8, ident, "return"))
        ._return
    else
        .identifier;
}
