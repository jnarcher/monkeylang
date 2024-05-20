const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn string(self: Program) []const u8 {
        var out: []const u8 = undefined;
        return for (self.statements.items) |stmt| {
            out = out ++ stmt.string();
        } else out[0..];
    }
};

pub const Statement = union(enum) {
    let: LetStatement,
    _return: ReturnStatment,
    expression: ExpressionStatement,

    pub fn string(self: Statement) []const u8 {
        return switch (self) {
            inline else => |s| s.string(),
        };
    }
};

pub const LetStatement = struct {
    ident: []const u8,
    value: Expression,

    pub fn string(self: LetStatement) []const u8 {
        return "let " ++ self.ident ++ " = " ++ self.value.string() ++ ";";
    }
};

pub const ReturnStatment = struct {
    value: Expression,

    pub fn string(self: LetStatement) []const u8 {
        return "return " ++ self.value.string() ++ ";";
    }
};

pub const ExpressionStatement = struct {
    expression: Expression,

    pub fn string(self: ExpressionStatement) []const u8 {
        return self.expression.string();
    }
};

pub const Expression = struct {
    pub fn string(self: Expression) []const u8 {
        return self.ident;
    }
};
