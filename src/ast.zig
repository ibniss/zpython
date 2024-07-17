const std = @import("std");
const t = @import("./token.zig");
const Token = t.Token;

pub const Module = struct {
    body: std.ArrayList(Stmt),
    arena: *std.heap.ArenaAllocator,

    pub fn init(arena: *std.heap.ArenaAllocator) Module {
        return .{
            .body = std.ArrayList(Stmt).init(arena.allocator()),
            .arena = arena,
        };
    }

    pub fn format(self: Module, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Module(\n  body=[\n", .{});

        for (self.body.items) |stmt| {
            try writer.print("    {s},\n", .{stmt});
        }

        try writer.print("  ]\n)", .{});
    }

    pub fn stringify(self: Module, writer: anytype) void {
        for (self.body.items, 0..) |stmt, i| {
            stmt.stringify(writer);
            if (i != self.body.items.len - 1) {
                _ = writer.write("\n") catch unreachable;
            }
        }
    }
};

pub const Expr = union(enum) {
    name: Name,
    constant: Constant,
    unary: UnaryOp,
    binary: BinaryOp,
    compare: Compare,

    pub fn format(self: Expr, comptime buf: []const u8, fmt: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            inline else => |s| try s.format(buf, fmt, writer),
        }
    }

    pub fn stringify(self: Expr, writer: anytype) void {
        switch (self) {
            inline else => |s| s.stringify(writer),
        }
    }
};
pub const Stmt = union(enum) {
    assign: Assign,
    ret: Return,

    // expression statemens - standalone expression
    expr: struct {
        const Self = @This();
        value: *const Expr,

        pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.print("Expr(value={s})", .{self.value});
        }

        pub fn stringify(self: Self, writer: anytype) void {
            self.value.stringify(writer);
        }
    },

    // comptime formatter which dispatches to the formatter for the correct union member
    pub fn format(self: Stmt, comptime buf: []const u8, fmt: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            inline else => |s| try s.format(buf, fmt, writer),
        }
    }

    pub fn stringify(self: Stmt, writer: anytype) void {
        switch (self) {
            inline else => |s| s.stringify(writer),
        }
    }
};

pub const Assign = struct {
    // TODO: targets* in Python
    target: *const Expr,
    value: *const Expr,

    pub fn format(self: Assign, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Assign(target={s}, value={?})", .{ self.target, self.value });
    }

    pub fn stringify(self: Assign, writer: anytype) void {
        self.target.stringify(writer);
        _ = writer.write(" = ") catch unreachable;
        self.value.stringify(writer);
    }
};

pub const Return = struct {
    // RETURN token
    token: Token,
    value: ?*const Expr,

    pub fn format(self: Return, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Return(value={any})", .{self.value});
    }

    pub fn stringify(self: Return, writer: anytype) void {
        _ = writer.write("return") catch unreachable;

        if (self.value) |v| {
            _ = writer.write(" ") catch unreachable;
            v.stringify(writer);
        }
    }
};

pub const Name = struct {
    // NAME token
    token: Token,
    // the actual name referenced
    value: []const u8,

    pub fn format(self: Name, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Name(value=\"{s}\")", .{self.value});
    }

    pub fn stringify(self: Name, writer: anytype) void {
        _ = writer.write(self.value) catch unreachable;
    }
};

pub const Constant = struct {
    token: Token,
    // TODO: can be number, string, etc
    value: []const u8,

    pub fn format(self: Constant, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Constant(value=\"{s}\")", .{self.value});
    }

    pub fn stringify(self: Constant, writer: anytype) void {
        _ = writer.write(self.value) catch unreachable;
    }
};

pub const UnaryOp = struct {
    token: Token,
    op: UnaryOperator,
    operand: *const Expr,

    pub fn format(self: UnaryOp, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("UnaryOp(op={s}, operand={s})", .{ self.op, self.operand });
    }

    pub fn stringify(self: UnaryOp, writer: anytype) void {
        _ = writer.write("(") catch unreachable;
        self.op.stringify(writer);
        self.operand.stringify(writer);
        _ = writer.write(")") catch unreachable;
    }
};

pub const BinaryOp = struct {
    token: Token,
    left: *const Expr,
    op: BinaryOperator,
    right: *const Expr,

    pub fn format(self: BinaryOp, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("BinOp(left={s}, op={s}, right={s})", .{ self.left, self.op, self.right });
    }

    pub fn stringify(self: BinaryOp, writer: anytype) void {
        _ = writer.write("(") catch unreachable;
        self.left.stringify(writer);
        _ = writer.write(" ") catch unreachable;
        self.op.stringify(writer);
        _ = writer.write(" ") catch unreachable;
        self.right.stringify(writer);
        _ = writer.write(")") catch unreachable;
    }
};

pub const Compare = struct {
    token: Token,
    left: *const Expr,
    ops: []const ComparisonOperator,
    comparators: []const *const Expr,

    pub fn format(self: Compare, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        // TODO print properly
        try writer.print("Compare(left={s}, ops={s}, comparators={s})", .{ self.left, self.ops, self.comparators });
    }

    pub fn stringify(self: Compare, writer: anytype) void {
        _ = writer.write("(") catch unreachable;
        self.left.stringify(writer);
        _ = writer.write(" ") catch unreachable;
        self.ops.stringify(writer);
        _ = writer.write(" ") catch unreachable;
        self.comparators.stringify(writer);
        _ = writer.write(")") catch unreachable;
    }
};

pub const ComparisonOperator = union(enum) {
    Eq: struct { token: Token },
    NotEq: struct { token: Token },
    Lt: struct { token: Token },
    LtE: struct { token: Token },
    Gt: struct { token: Token },
    GtE: struct { token: Token },

    pub fn fromToken(token: Token) ?ComparisonOperator {
        return switch (token.type) {
            .EQEQUAL => .{ .Eq = .{ .token = token } },
            .NOTEQUAL => .{ .NotEq = .{ .token = token } },
            .LESS => .{ .Lt = .{ .token = token } },
            .GREATER => .{ .Gt = .{ .token = token } },
            // TODO add all
            else => null,
        };
    }

    pub fn format(self: ComparisonOperator, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s}()", .{@tagName(self)});
    }

    pub fn stringify(self: ComparisonOperator, writer: anytype) void {
        _ = switch (self) {
            inline else => |s| writer.write(s.token.literal) catch unreachable,
        };
    }
};

pub const BinaryOperator = union(enum) {
    Add: struct { token: Token },
    Sub: struct { token: Token },
    Mult: struct { token: Token },
    Div: struct { token: Token },
    FloorDiv: struct { token: Token },
    Mod: struct { token: Token },

    pub fn fromToken(token: Token) ?BinaryOperator {
        return switch (token.type) {
            .PLUS => .{ .Add = .{ .token = token } },
            .MINUS => .{ .Sub = .{ .token = token } },
            .STAR => .{ .Mult = .{ .token = token } },
            .SLASH => .{ .Div = .{ .token = token } },
            .DOUBLESLASH => .{ .FloorDiv = .{ .token = token } },
            .PERCENT => .{ .Mod = .{ .token = token } },
            else => null,
        };
    }

    pub fn format(self: BinaryOperator, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s}()", .{@tagName(self)});
    }

    pub fn stringify(self: BinaryOperator, writer: anytype) void {
        _ = switch (self) {
            inline else => |s| writer.write(s.token.literal) catch unreachable,
        };
    }
};

pub const UnaryOperator = union(enum) {
    USub: struct { token: Token },
    Invert: struct { token: Token },

    pub fn fromToken(token: Token) UnaryOperator {
        return switch (token.type) {
            .MINUS => .{ .USub = .{ .token = token } },
            .TILDE => .{ .Invert = .{ .token = token } },
            else => std.debug.panic("Invalid unary operator token {s}\n", .{token}),
        };
    }

    pub fn format(self: UnaryOperator, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s}()", .{@tagName(self)});
    }

    pub fn stringify(self: UnaryOperator, writer: anytype) void {
        _ = switch (self) {
            inline else => |to| writer.write(to.token.literal) catch unreachable,
        };
    }
};
