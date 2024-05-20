const std = @import("std");
const allocPrint = std.fmt.allocPrint;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn string(self: Program, alloc: std.mem.Allocator) ![]const u8 {
        var lines = std.ArrayList(u8).init(alloc);
        defer lines.deinit();
        for (self.statements.items) |stmt| {
            try lines.appendSlice(try stmt.string(alloc));
        }
        return try lines.toOwnedSlice();
    }
};

pub const Statement = union(enum) {
    let: LetStatement,
    _return: ReturnStatment,
    expression: ExpressionStatement,

    pub fn string(self: Statement, alloc: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            inline else => |s| try s.string(alloc),
        };
    }
};

pub const LetStatement = struct {
    ident: Identifier,
    value: Expression,

    pub fn string(self: LetStatement, alloc: std.mem.Allocator) ![]const u8 {
        return allocPrint(alloc, "let {s} = {s};", .{
            try self.ident.string(alloc),
            try self.value.string(alloc),
        });
    }
};

pub const ReturnStatment = struct {
    value: Expression,

    pub fn string(self: ReturnStatment, alloc: std.mem.Allocator) ![]const u8 {
        return allocPrint(alloc, "return {s};", .{try self.value.string(alloc)});
    }
};

pub const ExpressionStatement = struct {
    expression: Expression,

    pub fn string(self: ExpressionStatement, alloc: std.mem.Allocator) ![]const u8 {
        return self.expression.string(alloc);
    }
};

pub const Expression = union(enum) {
    ident: Identifier,
    int: IntLiteral,

    pub fn string(self: Expression, alloc: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            inline else => |e| e.string(alloc),
        };
    }
};

pub const Identifier = struct {
    name: []const u8,

    pub fn string(self: Identifier, _: std.mem.Allocator) ![]const u8 {
        return self.name;
    }
};

pub const IntLiteral = struct {
    value: i64,

    pub fn string(self: IntLiteral, alloc: std.mem.Allocator) ![]const u8 {
        return try allocPrint(alloc, "{}", .{self.value});
    }
};

// TESTS BEGIN HERE

const testing = std.testing;

test "print program" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const stmts = [_]Statement{
        Statement{
            .let = LetStatement{
                .ident = Identifier{ .name = "myVar" },
                .value = Expression{
                    .ident = Identifier{ .name = "anotherVar" },
                },
            },
        },
    };

    var program = Program{
        .statements = std.ArrayList(Statement).init(alloc),
    };
    try program.statements.appendSlice(&stmts);

    try testing.expectEqualStrings("let myVar = anotherVar;", try program.string(alloc));
}
