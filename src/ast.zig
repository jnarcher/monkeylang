const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;

pub const Program = struct {
    statements: std.ArrayList(Statement),
};

pub const Statement = union(enum) {
    let: LetStatement,
    _return: ReturnStatment,
    other,
};

pub const LetStatement = struct {
    ident: []const u8,
    value: Expression,
};

pub const ReturnStatment = struct {
    value: Expression,
};

pub const Expression = struct {};
