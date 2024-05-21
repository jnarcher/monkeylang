const std = @import("std");
const allocPrint = std.fmt.allocPrint;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn string(self: Program, alloc: std.mem.Allocator) ![]const u8 {
        var lines = std.ArrayList(u8).init(alloc);
        defer lines.deinit();
        for (self.statements.items) |stmt| {
            try lines.appendSlice(try stmt.string(alloc));
        }
        return lines.toOwnedSlice();
    }
};

pub const Statement = union(enum) {
    let: LetStatement,
    _return: ReturnStatment,
    block: BlockStatement,
    expression: ExpressionStatement,

    pub fn string(self: Statement, alloc: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            inline else => |s| try s.string(alloc),
        };
    }
};

pub const LetStatement = struct {
    ident: Identifier,
    value: *Expression,

    pub fn string(self: LetStatement, alloc: std.mem.Allocator) ![]const u8 {
        return allocPrint(alloc, "let {s} = {s};", .{
            try self.ident.string(alloc),
            try self.value.string(alloc),
        });
    }
};

pub const ReturnStatment = struct {
    value: ?*Expression,

    pub fn string(self: ReturnStatment, alloc: std.mem.Allocator) ![]const u8 {
        if (self.value) |v| {
            return allocPrint(alloc, "return {s};", .{try v.string(alloc)});
        }
        return "return;";
    }
};

pub const BlockStatement = struct {
    statements: std.ArrayList(Statement),

    pub fn string(self: BlockStatement, alloc: std.mem.Allocator) std.fmt.AllocPrintError![]const u8 {
        var block = std.ArrayList(u8).init(alloc);
        defer block.deinit();

        for (self.statements.items) |s| {
            try block.appendSlice(try s.string(alloc));
        }

        return block.toOwnedSlice();
    }
};

pub const ExpressionStatement = struct {
    expression: *Expression,

    pub fn string(self: ExpressionStatement, alloc: std.mem.Allocator) ![]const u8 {
        return self.expression.string(alloc);
    }
};

pub const Expression = union(enum) {
    ident: Identifier,
    int: IntLiteral,
    prefix: Prefix,
    infix: Infix,
    boolean: Boolean,
    _if: IfExpression,
    function: FunctionLiteral,
    call: CallExpression,

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

pub const Boolean = struct {
    value: bool,

    pub fn string(self: Boolean, _: std.mem.Allocator) ![]const u8 {
        return if (self.value) "true" else "false";
    }
};

pub const Prefix = struct {
    operator: Token,
    right: *Expression,

    pub fn string(self: Prefix, alloc: std.mem.Allocator) std.fmt.AllocPrintError![]const u8 {
        return allocPrint(alloc, "({s}{s})", .{
            self.operator.literal(),
            try self.right.string(alloc),
        });
    }
};

pub const Infix = struct {
    left: *Expression,
    operator: Token,
    right: *Expression,

    pub fn string(self: Infix, alloc: std.mem.Allocator) std.fmt.AllocPrintError![]const u8 {
        return allocPrint(alloc, "({s} {s} {s})", .{
            try self.left.string(alloc),
            self.operator.literal(),
            try self.right.string(alloc),
        });
    }
};

pub const IfExpression = struct {
    condition: *Expression,
    consequence: BlockStatement,
    alternative: ?BlockStatement,

    pub fn string(self: IfExpression, alloc: std.mem.Allocator) std.fmt.AllocPrintError![]const u8 {
        var out = std.ArrayList(u8).init(alloc);
        defer out.deinit();

        try out.appendSlice(try allocPrint(
            alloc,
            "if ({s}) {{ {s} }}",
            .{
                try self.condition.string(alloc),
                try self.consequence.string(alloc),
            },
        ));

        if (self.alternative) |alt| {
            try out.appendSlice(try allocPrint(
                alloc,
                " else {{ {s} }}",
                .{try alt.string(alloc)},
            ));
        }

        return out.toOwnedSlice();
    }
};

pub const FunctionLiteral = struct {
    params: std.ArrayList(Identifier),
    body: BlockStatement,

    pub fn string(self: FunctionLiteral, alloc: std.mem.Allocator) std.fmt.AllocPrintError![]const u8 {
        var params_str = std.ArrayList(u8).init(alloc);
        defer params_str.deinit();
        for (self.params.items, 0..) |param, i| {
            try params_str.appendSlice(try param.string(alloc));
            if (i < self.params.items.len - 1) {
                try params_str.appendSlice(", ");
            }
        }

        var out = std.ArrayList(u8).init(alloc);
        defer out.deinit();
        try out.appendSlice(try allocPrint(
            alloc,
            "fn({s}) {{ {s} }}",
            .{
                try params_str.toOwnedSlice(),
                try self.body.string(alloc),
            },
        ));

        return out.toOwnedSlice();
    }
};

pub const CallExpression = struct {
    function: *Expression,
    arguments: std.ArrayList(*Expression),

    pub fn string(self: CallExpression, alloc: std.mem.Allocator) std.fmt.AllocPrintError![]const u8 {
        var args_str = std.ArrayList(u8).init(alloc);
        defer args_str.deinit();

        for (self.arguments.items, 0..) |arg, i| {
            try args_str.appendSlice(try arg.string(alloc));
            if (i < self.arguments.items.len - 1) {
                try args_str.appendSlice(", ");
            }
        }

        var out = std.ArrayList(u8).init(alloc);
        defer out.deinit();
        try out.appendSlice(try allocPrint(
            alloc,
            "{s}({s})",
            .{
                try self.function.string(alloc),
                try args_str.toOwnedSlice(),
            },
        ));

        return out.toOwnedSlice();
    }
};

// TESTS BEGIN HERE

const testing = std.testing;

test "print program" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var exp = Expression{
        .ident = Identifier{ .name = "anotherVar" },
    };

    const stmts = [_]Statement{
        Statement{
            .let = LetStatement{
                .ident = Identifier{ .name = "myVar" },
                .value = &exp,
            },
        },
    };

    var program = Program{
        .statements = std.ArrayList(Statement).init(alloc),
    };
    try program.statements.appendSlice(&stmts);

    try testing.expectEqualStrings("let myVar = anotherVar;", try program.string(alloc));
}
