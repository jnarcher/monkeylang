const std = @import("std");

pub const Token = union(enum) {
    ident: []const u8,
    int: []const u8,
    illegal: []const u8,

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
    lsquirley,
    rsquirley,

    // Keywords
    function,
    let,
    true,
    false,
    _if,
    _else,
    _return,

    pub fn keyword(ident: []const u8) ?Token {
        const map = std.ComptimeStringMap(Token, .{
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

    pub fn toString(self: Token, buf: []u8) ![]const u8 {
        return switch (self) {
            ._if => try std.fmt.bufPrint(buf, "Token (if)", .{}),
            ._else => try std.fmt.bufPrint(buf, "Token (else)", .{}),
            ._return => try std.fmt.bufPrint(buf, "Token (return)", .{}),
            .ident, .int, .illegal => |v| try std.fmt.bufPrint(
                buf,
                "Token ({s}) : '{s}'",
                .{ @tagName(self), v },
            ),
            else => try std.fmt.bufPrint(buf, "Token ({s})", .{@tagName(self)}),
        };
    }
};
