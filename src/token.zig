const std = @import("std");

pub const Type = enum {
    illegal,
    eof,
    // Identifiers + literals
    ident, // add, foobar, x, y, ...
    int, // 213423423
    // Operators
    assign,
    plus,
    // Delimiters
    comma,
    semicolon,
    // Surround + scope
    lparen,
    rparen,
    lbrace,
    rbrace,
    // Keywords
    function,
    let,

    pub fn toString(self: Type) []const u8 {
        return switch (self) {
            .illegal => "ILLEGAL",
            .eof => "EOF",
            .ident => "IDENT",
            .int => "INT",
            .assign => "=",
            .plus => "+",
            .comma => ",",
            .semicolon => ";",
            .lparen => "(",
            .rparen => ")",
            .lbrace => "{",
            .rbrace => "}",
            .function => "FUNCTION",
            .let => "LET",
        };
    }
};

pub const Token = struct {
    type: Type,
    literal: []const u8,
};
