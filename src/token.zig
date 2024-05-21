const std = @import("std");
const allocPrint = std.fmt.allocPrint;

pub const Token = union(enum) {
    const Self = @This();

    ident: []const u8,
    int: []const u8,

    illegal,
    eof,

    // Operations
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    lt, // <  less than
    gt, // >  greater than
    le, // <= less than or equal to
    ge, // >= greater than or equal to
    eq,
    not_eq,

    // Delimiters
    comma,
    semicolon,
    lparen,
    rparen,
    lsquirly,
    rsquirly,

    // Keywords
    function,
    let,
    true,
    false,
    _if,
    _else,
    _return,

    pub fn keyword(ident: []const u8) ?Self {
        const map = std.ComptimeStringMap(Self, .{
            .{ "fn", .function },
            .{ "let", .let },
            .{ "true", .true },
            .{ "false", .false },
            .{ "if", ._if },
            .{ "else", ._else },
            .{ "return", ._return },
        });
        return map.get(ident);
    }

    pub fn literal(self: Self) []const u8 {
        return switch (self) {
            .ident, .int => |v| v,
            .illegal => "ILLEGAL",
            .eof => "EOF",
            .assign => "=",
            .plus => "+",
            .minus => "-",
            .bang => "!",
            .asterisk => "*",
            .slash => "/",
            .lt => "<",
            .gt => ">",
            .le => "<=",
            .ge => ">=",
            .eq => "==",
            .not_eq => "!=",
            .comma => ",",
            .semicolon => ";",
            .lparen => "(",
            .rparen => ")",
            .lsquirly => "{",
            .rsquirly => "}",
            .function => "fn",
            .let => "let",
            .true => "true",
            .false => "false",
            ._if => "if",
            ._else => "else",
            ._return => "return",
        };
    }

    pub fn debugString(self: Self) []const u8 {
        const alloc = std.heap.page_allocator;
        return switch (self) {
            .ident => |v| allocPrint(alloc, "Token(IDENT): '{s}'", .{v}) catch "",
            .int => |v| allocPrint(alloc, "Token(INT): '{s}'", .{v}) catch "",
            else => allocPrint(alloc, "Token({s})", .{self.literal()}) catch "",
        };
    }
};
