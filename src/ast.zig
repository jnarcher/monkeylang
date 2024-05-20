const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;

// TODO: what is this for?
pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,
};

pub const Statement = union(enum) {
    let: LetStatement,
    other,

    pub fn tokenLiteral(self: Statement) []const u8 {
        // TODO
        _ = self;
        return "";
    }

    pub fn node(self: Statement) void {
        // TODO
        _ = self;
    }
};

pub const LetStatement = struct {
    ident: []const u8,
    value: Expression,
};

pub const Expression = struct {
    pub fn tokenLiteral(self: Expression) []const u8 {
        // TODO
        _ = self;
        return "";
    }

    pub fn node(self: Expression) void {
        // TODO
        _ = self;
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),
};
