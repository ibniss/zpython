const std = @import("std");
const t = @import("./token.zig");
const Token = t.Token;

const IndentWriter = struct {
    writer: std.io.AnyWriter,
    indent_size: usize,
    level: usize = 0,
    needs_indent: bool = true,

    pub fn init(writer: std.io.AnyWriter, ind: usize) IndentWriter {
        return .{
            .writer = writer,
            .indent_size = ind,
        };
    }

    pub fn indent(self: *IndentWriter) void {
        self.level += 1;
    }

    pub fn dedent(self: *IndentWriter) void {
        if (self.level > 0) {
            self.level -= 1;
        }
    }

    fn writeIndent(self: *IndentWriter) anyerror!void {
        if (self.needs_indent) {
            self.needs_indent = false;

            for (0..self.indent_size * self.level) |_| {
                _ = try self.writer.write(" ");
            }
        }
    }

    pub fn write(self: *IndentWriter, bytes: []const u8) anyerror!usize {
        // write one by one, ensuring after newline we indent the next char
        for (bytes) |byte| {
            try self.writeIndent();
            if (byte == '\n') {
                self.needs_indent = true;
            }
            try self.writer.writeByte(byte);
        }
        return bytes.len;
    }
    pub fn print(self: *IndentWriter, comptime format: []const u8, args: anytype) anyerror!void {
        // for print, always indent the line
        self.needs_indent = true;
        try self.writeIndent();
        // if print included newline, next print/write should indent
        if (std.mem.indexOf(u8, format, "\n")) |_| {
            self.needs_indent = true;
        }
        return self.writer.print(format, args);
    }
};

pub fn dumpAlloc(obj: anytype, allocator: std.mem.Allocator, indent: usize) ![]const u8 {
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();
    try dump(obj, buf.writer().any(), indent);
    return buf.toOwnedSlice();
}

pub fn dump(obj: anytype, writer: std.io.AnyWriter, indent: usize) !void {
    var id_writer = IndentWriter.init(writer, indent);
    try obj.dump(&id_writer);
}

fn dumpOption(obj: anytype, writer: *IndentWriter) !void {
    if (obj) |v| {
        try v.dump(writer);
    } else {
        _ = try writer.write("None");
    }
}

fn dumpList(list: anytype, writer: *IndentWriter) anyerror!void {
    for (list.items, 0..) |arg, idx| {
        if (idx != 0) {
            _ = try writer.write(",\n");
        }

        // depending on whether the child type is optional or not, handle printing optionals
        const ListT = @TypeOf(list.items);
        const ListTypeInfo = @typeInfo(ListT);
        // @compileLog(.{ ListT, ListTypeInfo });
        switch (ListTypeInfo) {
            // fixed size array
            .Array => |Typ| {
                const ListChildTypeInfo = @typeInfo(Typ.child);
                if (ListChildTypeInfo == .Optional) {
                    try dumpOption(arg, writer);
                } else {
                    try arg.dump(writer);
                }
            },
            // could be a slice
            .Pointer => |Point| {
                if (Point.size == .Slice) {
                    const ChildTypeInfo = @typeInfo(Point.child);

                    if (ChildTypeInfo == .Optional) {
                        try dumpOption(arg, writer);
                    } else {
                        try arg.dump(writer);
                    }
                } else {
                    unreachable;
                }
            },
            else => unreachable,
        }
    }
}

pub const Module = struct {
    body: std.ArrayList(Stmt),
    arena: std.heap.ArenaAllocator,

    const Self = @This();

    pub fn init(arena: *std.heap.ArenaAllocator) Module {
        return .{
            .body = std.ArrayList(Stmt).init(arena.allocator()),
            .arena = arena.*,
        };
    }

    pub fn deinit(self: *const Self) void {
        self.arena.deinit();
    }

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write("Module(\n");
        writer.indent();
        _ = try writer.write("body=[\n");
        writer.indent();
        for (self.body.items) |stmt| {
            try stmt.dump(writer);
            _ = try writer.write(",\n");
        }
        writer.dedent();
        _ = try writer.write("]\n");
        writer.dedent();
        _ = try writer.write(")");
    }

    pub fn stringify(self: Self, writer: anytype) void {
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
    if_exp: IfExp,
    call: Call,

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) anyerror!void {
        switch (self) {
            inline else => |s| try s.dump(writer),
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
    if_stmt: IfStmt,
    func_def: FunctionDef,

    // expression statemens - standalone expression
    expr: struct {
        const ExprSelf = @This();
        value: *const Expr,

        // Dump the ast representation using the writer
        pub fn dump(self: ExprSelf, writer: *IndentWriter) !void {
            _ = try writer.write("Expr(\n");
            writer.indent();
            _ = try writer.write("value=");
            try self.value.dump(writer);
            _ = try writer.write("\n");
            writer.dedent();
            _ = try writer.write(")");
        }

        pub fn stringify(self: ExprSelf, writer: anytype) void {
            self.value.stringify(writer);
        }
    },

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        switch (self) {
            inline else => |s| try s.dump(writer),
        }
    }

    pub fn stringify(self: Stmt, writer: anytype) void {
        switch (self) {
            inline else => |s| s.stringify(writer),
        }
    }
};

pub const Arg = struct {
    /// Argument name
    arg: []const u8,
    // TODO: annotation
    // TODO: type comment

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        try writer.print("arg(arg=\"{s}\")", .{self.arg});
    }
};

pub const Arguments = struct {
    // TODO
    //
    // posonlyargs: std.ArrayList(Arg),
    args: std.ArrayList(Arg),
    vararg: ?Arg,
    kwonlyargs: std.ArrayList(Arg),
    kw_defaults: std.ArrayList(?Constant),
    kwarg: ?Arg,
    defaults: std.ArrayList(Constant),

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write("arguments(\n");
        writer.indent();
        // TODO posonlyargs=[\n", .{});
        // for (self.posonlyargs.items, 0..) |arg, idx| {
        //     if (idx != 0) {
        //         _ = try writer.write(",\n", .{});
        //     }
        //
        //     _ = try writer.write("{s}", .{arg});
        // }
        //
        // _ = try writer.write("],\n", .{});
        _ = try writer.write("args=[\n");
        writer.indent();
        try dumpList(self.args, writer);
        _ = try writer.write("\n");
        writer.dedent();
        _ = try writer.write("],\n");
        _ = try writer.write("vararg=");
        try dumpOption(self.vararg, writer);
        _ = try writer.write(",\n");
        _ = try writer.write("kwonlyargs=[\n");
        writer.indent();
        try dumpList(self.kwonlyargs, writer);
        writer.dedent();
        _ = try writer.write("],\n");
        _ = try writer.write("kw_defaults=[\n");
        writer.indent();
        try dumpList(self.kw_defaults, writer);
        writer.dedent();
        _ = try writer.write("],\n");
        _ = try writer.write("kwarg=");
        try dumpOption(self.kwarg, writer);
        _ = try writer.write(",\n");
        _ = try writer.write("defaults=[\n");
        writer.indent();
        try dumpList(self.defaults, writer);
        writer.dedent();
        _ = try writer.write("],\n");
        writer.dedent();
        _ = try writer.write("]\n");
        writer.dedent();
        _ = try writer.write(")");
    }
};

pub const FunctionDef = struct {
    name: []const u8,
    args: Arguments,
    body: std.ArrayList(Stmt),

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write("FunctionDef(\n");
        writer.indent();
        _ = try writer.write("name=");
        _ = try writer.write(self.name);
        _ = try writer.write(",\n");
        _ = try writer.write("args=");
        try self.args.dump(writer);
        _ = try writer.write(",\n");
        _ = try writer.write("body=[\n");
        writer.indent();
        try dumpList(self.body, writer);
        writer.dedent();
        _ = try writer.write("\n");
        _ = try writer.write("]\n");
        writer.dedent();
        _ = try writer.write(")");
    }

    pub fn stringify(self: FunctionDef, writer: anytype) void {
        _ = self;
        _ = writer.write("def") catch unreachable;
    }
};

pub const IfStmt = struct {
    test_expr: *const Expr,
    body: std.ArrayList(Stmt),
    or_else: std.ArrayList(Stmt),

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write("If(\n");
        writer.indent();
        _ = try writer.write("test=");
        try self.test_expr.dump(writer);
        _ = try writer.write(",\n");
        _ = try writer.write("body=[\n");
        writer.indent();
        try dumpList(self.body, writer);
        writer.dedent();
        _ = try writer.write("\n");
        _ = try writer.write("],\n");
        _ = try writer.write("orelse=[\n");
        writer.indent();
        try dumpList(self.or_else, writer);
        writer.dedent();
        _ = try writer.write("\n");
        _ = try writer.write("]\n");
        writer.dedent();
        _ = try writer.write(")");
    }

    pub fn stringify(self: IfStmt, writer: anytype) void {
        _ = self;
        _ = writer.write("if") catch unreachable;
    }
};

pub const Assign = struct {
    // TODO: targets* in Python
    target: *const Expr,
    value: *const Expr,

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write("Assign(\n");
        writer.indent();
        _ = try writer.write("target=");
        try self.target.dump(writer);
        _ = try writer.write(",\n");
        _ = try writer.write("value=");
        try self.value.dump(writer);
        writer.dedent();
        _ = try writer.write("\n");
        _ = try writer.write(")");
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

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write("Return(value=");
        try dumpOption(self.value, writer);
        _ = try writer.write(")");
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

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write("Name(value=");
        _ = try writer.write(self.value);
        _ = try writer.write(")");
    }

    pub fn stringify(self: Name, writer: anytype) void {
        _ = writer.write(self.value) catch unreachable;
    }
};

pub const Constant = struct {
    token: Token,
    // TODO: can be number, string, etc
    value: []const u8,

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write("Constant(value=");
        _ = try writer.write(self.value);
        _ = try writer.write(")");
    }

    pub fn stringify(self: Constant, writer: anytype) void {
        _ = writer.write(self.value) catch unreachable;
    }
};

pub const UnaryOp = struct {
    token: Token,
    op: UnaryOperator,
    operand: *const Expr,

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write("UnaryOp(\n");
        writer.indent();
        try self.op.dump(writer);
        _ = try writer.write(",\n");
        _ = try writer.write("operand=");
        try self.operand.dump(writer);
        _ = try writer.write("\n");
        writer.dedent();
        _ = try writer.write(")");
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

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write("BinOp(\n");
        writer.indent();
        _ = try writer.write("left=");
        try self.left.dump(writer);
        _ = try writer.write(",\n");
        _ = try writer.write("op=");
        try self.op.dump(writer);
        _ = try writer.write(",\n");
        _ = try writer.write("right=");
        try self.right.dump(writer);
        _ = try writer.write("\n");
        writer.dedent();
        _ = try writer.write(")");
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
    ops: std.ArrayList(ComparisonOperator),
    comparators: std.ArrayList(*const Expr),

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write("Compare(\n");
        writer.indent();
        _ = try writer.write("left=");
        try self.left.dump(writer);
        _ = try writer.write(",\n");
        _ = try writer.write("ops=[\n");
        writer.indent();
        try dumpList(self.ops, writer);
        writer.dedent();
        _ = try writer.write("\n");
        _ = try writer.write("],\n");
        _ = try writer.write("comparators=[\n");
        writer.indent();
        try dumpList(self.comparators, writer);
        _ = try writer.write("\n");
        writer.dedent();
        _ = try writer.write("]\n");
        writer.dedent();
        _ = try writer.write(")");
    }

    pub fn stringify(self: Compare, writer: anytype) void {
        _ = writer.write("(") catch unreachable;
        self.left.stringify(writer);

        for (self.ops.items, self.comparators.items) |op, comp| {
            _ = writer.write(" ") catch unreachable;
            op.stringify(writer);
            _ = writer.write(" ") catch unreachable;
            comp.stringify(writer);
        }
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

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        try writer.print("{s}()", .{@tagName(self)});
    }

    pub fn stringify(self: ComparisonOperator, writer: anytype) void {
        _ = switch (self) {
            inline else => |s| {
                std.debug.print("{s}\n", .{s.token});
                _ = writer.write(s.token.literal) catch unreachable;
            },
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

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write(@tagName(self));
        _ = try writer.write("()");
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

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write(@tagName(self));
        _ = try writer.write("()");
    }

    pub fn stringify(self: UnaryOperator, writer: anytype) void {
        _ = switch (self) {
            inline else => |to| writer.write(to.token.literal) catch unreachable,
        };
    }
};

pub const Keyword = struct {
    /// identifier is NULL for **kwargs
    identifier: ?[]const u8,
    value: *const Expr,

    const Self = @This();

    pub fn dump(self: Self, writer: *IndentWriter) !void {
        if (self.identifier) |ident| {
            _ = try writer.write(ident);
            _ = try writer.write("=");
        }
        try self.value.dump(writer);
    }
};

pub const Call = struct {
    func: *const Expr,
    args: std.ArrayList(*const Expr),
    keywords: std.ArrayList(Keyword),

    const Self = @This();

    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write("Call(\n");
        writer.indent();
        _ = try writer.write("func=");
        try self.func.dump(writer);
        _ = try writer.write(",\n");
        _ = try writer.write("args=[\n");
        try dumpList(self.args, writer);
        _ = try writer.write("],\n");
        _ = try writer.write("keywords=[\n");
        try dumpList(self.keywords, writer);
        _ = try writer.write("],\n");
        try self.func.dump(writer);
        writer.dedent();
        _ = try writer.write("\n)");
    }

    pub fn stringify(self: Self, writer: anytype) void {
        self.func.stringify(writer);
        _ = writer.write("(") catch unreachable;
        // TODO: ...
        _ = writer.write(")") catch unreachable;
    }
};

pub const IfExp = struct {
    test_exp: *const Expr,
    body: *const Expr,
    or_else: *const Expr,

    const Self = @This();

    // Dump the ast representation using the writer
    pub fn dump(self: Self, writer: *IndentWriter) !void {
        _ = try writer.write("IfExp(\n");
        writer.indent();
        _ = try writer.write("test=");
        try self.test_exp.dump(writer);
        _ = try writer.write(",\n");
        _ = try writer.write("body=");
        try self.body.dump(writer);
        _ = try writer.write(",\n");
        _ = try writer.write("orelse=");
        try self.or_else.dump(writer);
        _ = try writer.write("\n");
        writer.dedent();
        _ = try writer.write(")");
    }

    pub fn stringify(self: IfExp, writer: anytype) void {
        self.body.stringify(writer);
        _ = writer.write(" if ") catch unreachable;
        self.test_exp.stringify(writer);
        _ = writer.write(" else ") catch unreachable;
        self.or_else.stringify(writer);
    }
};
