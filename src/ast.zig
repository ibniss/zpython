const std = @import("std");
const t = @import("./token.zig");
const Token = t.Token;

pub const Module = struct {
    body: std.ArrayList(Stmt),

    pub fn init(alloc: std.mem.Allocator) Module {
        return .{ .body = std.ArrayList(Stmt).init(alloc) };
    }

    pub fn deinit(self: *const Module) void {
        self.body.deinit();
    }

    pub fn format(self: Module, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Module(\n  body=[\n", .{});

        for (self.body.items) |stmt| {
            try writer.print("    {s},\n", .{stmt});
        }

        try writer.print("  ]\n)", .{});
    }
};

pub const Expr = union(enum) {
    name: Name,
    constant: Constant,
    unary: UnaryOp,
    binary: BinaryOp,

    pub fn format(self: Expr, comptime buf: []const u8, fmt: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .constant => |s| try s.format(buf, fmt, writer),
            inline else => |s| try s.format(buf, fmt, writer),
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
    },

    // comptime formatter which dispatches to the formatter for the correct union member
    pub fn format(self: Stmt, comptime buf: []const u8, fmt: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            inline else => |s| try s.format(buf, fmt, writer),
        }
    }
};

pub const Assign = struct {
    // TODO: targets* in Python
    target: *const Expr,
    value: ?*const Expr, // TODO: nonnull

    pub fn format(self: Assign, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Assign(target={s}, value={?})", .{ self.target, self.value });
    }
};

pub const Return = struct {
    // RETURN token
    token: Token,
    value: ?*const Expr,

    pub fn format(self: Return, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Return(value={any})", .{self.value});
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
};

pub const Constant = struct {
    token: Token,
    // TODO: can be number, string, etc
    value: []const u8,

    pub fn format(self: Constant, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Constant(value=\"{s}\")", .{self.value});
    }
};

pub const UnaryOp = struct {
    token: Token,
    op: UnaryOperator,
    operand: *const Expr,

    pub fn format(self: UnaryOp, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("UnaryOp(op={s}, operand={s})", .{ self.op, self.operand });
    }
};

pub const BinaryOp = struct {
    token: Token,
    left: *const Expr,
    op: BinaryOperator,
    right: *const Expr,

    pub fn format(self: BinaryOp, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        // try writer.print("BinOp(, op={s}, right={s})", .{ self.op, self.right });
        try writer.print("BinOp(left={s}, op={s}, right={s})", .{ self.left, self.op, self.right });
    }
};

pub const BinaryOperator = union(enum) {
    Add: struct { token: Token },
    Sub: struct { token: Token },
    Mult: struct { token: Token },
    Div: struct { token: Token },
    FloorDiv: struct { token: Token },
    Mod: struct { token: Token },

    pub fn fromToken(token: Token) BinaryOperator {
        return switch (token.type) {
            .PLUS => .{ .Add = .{ .token = token } },
            .MINUS => .{ .Sub = .{ .token = token } },
            .STAR => .{ .Mult = .{ .token = token } },
            .SLASH => .{ .Div = .{ .token = token } },
            .DOUBLESLASH => .{ .FloorDiv = .{ .token = token } },
            .PERCENT => .{ .Mod = .{ .token = token } },
            else => std.debug.panic("Invalid binary operator token {s}\n", .{token}),
        };
    }

    pub fn format(self: BinaryOperator, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s}()", .{@tagName(self)});
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
};
