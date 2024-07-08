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
        try writer.print("Module(body=[\n", .{});

        for (self.body.items) |stmt| {
            try writer.print("{s},\n", .{stmt});
        }

        try writer.print("])", .{});
    }
};

pub const Expr = union(enum) { name: Name };
pub const Stmt = union(enum) {
    assign: Assign,
    ret: Return,

    // comptime formatter which dispatches to the formatter for the correct union member
    pub fn format(self: Stmt, comptime buf: []const u8, fmt: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            inline else => |s| try s.format(buf, fmt, writer),
        }
    }
};

pub const Assign = struct {
    // TODO: targets* in Python
    target: *Name,
    value: *Expr,

    pub fn format(self: Assign, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Assign(target={any}, value={any})\n", .{ self.target, self.value });
    }
};

pub const Return = struct {
    // RETURN token
    token: Token,
    value: ?*Expr,

    pub fn format(self: Return, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Return(value={any})\n", .{self.value});
    }
};

pub const Name = struct {
    // NAME token
    token: Token,
    // the actual name referenced
    value: []const u8,
};
