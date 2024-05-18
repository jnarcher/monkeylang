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

    pub fn debugString(self: Self) []const u8 {
        const alloc = std.heap.page_allocator;
        return switch (self) {
            ._if => allocPrint(alloc, "Token (if)", .{}) catch "",
            ._else => allocPrint(alloc, "Token (else)", .{}) catch "",
            ._return => allocPrint(alloc, "Token (return)", .{}) catch "",
            .ident, .int => |v| allocPrint(alloc, "Token ({s}) : '{s}'", .{ @tagName(self), v }) catch "",
            else => allocPrint(alloc, "Token ({s})", .{@tagName(self)}) catch "",
        };
    }
};
