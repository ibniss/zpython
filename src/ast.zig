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

    pub fn format(self: Expr, comptime buf: []const u8, fmt: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
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

pub const UnaryOperator = union(enum) {
    usub: USub,
    invert: Invert,

    pub fn fromToken(token: Token) UnaryOperator {
        return switch (token.type) {
            .MINUS => .{ .usub = .{ .token = token } },
            .TILDE => .{ .invert = .{ .token = token } },
            else => std.debug.panic("Invalid unary operator token {s}\n", .{token}),
        };
    }

    pub fn format(self: UnaryOperator, comptime buf: []const u8, fmt: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            inline else => |s| try s.format(buf, fmt, writer),
        }
    }
};

pub const USub = struct {
    token: Token,

    pub fn format(self: USub, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        try writer.print("USub()", .{});
    }
};

pub const Invert = struct {
    token: Token,

    pub fn format(self: Invert, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = self;
        try writer.print("Invert()", .{});
    }
};
