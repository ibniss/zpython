const assert = @import("./assert.zig").assert;
const std = @import("std");
const _t = @import("./token.zig");
const Token = _t.Token;
const TokenType = _t.TokenType;
const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;

const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: *Lexer,
    curToken: ?Token,
    peekToken: ?Token,
    errors: std.ArrayList([]const u8),

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) !Parser {
        const curr = try lexer.nextToken();
        const next = try lexer.nextToken();

        return .{ .allocator = allocator, .lexer = lexer, .curToken = curr, .peekToken = next, .errors = std.ArrayList([]const u8).init(allocator) };
    }

    pub fn deinit(self: *Parser) void {
        self.errors.deinit();
    }

    pub fn peekError(self: *Parser, typ: TokenType) void {
        // const msg = std.fmt.allocPrint(self.allocator, "expected next token to be {any}, got {any} instead\n", .{ typ, self.peekToken.?.type }) catch @panic("Fail to generate error message");
        // self.errors.append(msg) catch @panic("Fail to append error message");
        _ = self;
        _ = typ;
    }

    pub fn nextToken(self: *Parser) void {
        std.debug.print("Calling next, current: {any}\n", .{self.curToken});
        self.curToken = self.peekToken;
        // unwrap so we don't have to try every parser step, error only can happen due to memory stuff
        self.peekToken = self.lexer.nextToken() catch @panic("Fail to get next parser token");
    }

    fn curTokenIs(self: *Parser, typ: TokenType) bool {
        return self.curToken != null and self.curToken.?.type == typ;
    }

    fn peekTokenIs(self: *Parser, typ: TokenType) bool {
        return self.peekToken != null and self.peekToken.?.type == typ;
    }

    fn expectPeek(self: *Parser, typ: TokenType) bool {
        if (self.peekTokenIs(typ)) {
            self.nextToken();
            return true;
        }

        self.peekError(typ);
        return false;
    }

    fn parseReturn(self: *Parser) ast.Return {
        const returnToken = self.curToken.?;
        self.nextToken();

        // TODO: parse expression rather than skipping
        var i: usize = 0;
        while (!self.curTokenIs(.NEWLINE) and !self.curTokenIs(.EOF)) {
            self.nextToken();
            i += 1;
            if (i > 10) {
                @panic("loop:");
            }
        }

        return .{ .token = returnToken, .value = null };
    }

    fn parseAssign(self: *Parser) ?ast.Assign {
        const name = ast.Name{ .token = self.curToken.?, .value = self.curToken.?.literal };

        // ensure next token is a assign, move forward
        if (!self.expectPeek(.EQUAL)) {
            return null;
        }

        // TODO: eval expr
        while (!self.curTokenIs(.NEWLINE)) {
            self.nextToken();
        }

        return .{ .target = name, .value = null };
    }

    fn parseStatement(self: *Parser) ?ast.Stmt {
        return switch (self.curToken.?.type) {
            .RETURN => .{ .ret = self.parseReturn() },
            else => els: {
                // TODO: Might have to change lexer to output a newline as a token, easier to parse?
                // no keywords were matched so check if next token is an equal
                if (self.curTokenIs(.NAME) and self.peekTokenIs(.EQUAL)) {
                    if (self.parseAssign()) |ass| {
                        break :els .{ .assign = ass };
                    }
                }

                break :els null;
            },
        };
    }

    pub fn parseProgram(self: *Parser) !ast.Module {
        var module = ast.Module.init(self.allocator);

        while (self.curToken != null and !self.curTokenIs(.EOF)) {
            std.debug.print("Calling parse statement, current: {any}\n", .{self.curToken});
            const stmt = self.parseStatement();
            if (stmt) |s| {
                try module.body.append(s);
            }
            self.nextToken();
        }

        return module;
    }
};

const t = std.testing;
const Snap = @import("./snaptest.zig").Snap;
const snap = Snap.snap;

// util to perform snapshot testing on a given input
fn checkParserOutput(input: []const u8, want: Snap) !void {
    var lexer = try Lexer.init(t.allocator, input);
    defer lexer.deinit();
    var parser = try Parser.init(t.allocator, &lexer);
    defer parser.deinit();

    const module = try parser.parseProgram();
    defer module.deinit();

    if (parser.errors.items.len > 0) {
        std.debug.print("Encountered {} parser errors:\n", .{parser.errors.items.len});
        for (parser.errors.items) |err| {
            std.debug.print("parser error: {s}\n", .{err});
        }
        std.debug.print("Parser output: {s}\n", .{module});
        // fail here
        try t.expect(parser.errors.items.len == 0);
    }

    const result = try std.fmt.allocPrint(t.allocator, "{s}", .{module});
    defer t.allocator.free(result);
    try want.diff(result);
}

test "can parse" {
    const input =
        \\x = 5
        \\y = 10
        \\foobar = 838383
        \\return 5
    ;

    try checkParserOutput(input, snap(@src(),
        \\Module(
        \\  body=[
        \\    Assign(target=Name(value="x"), value=null),
        \\    Assign(target=Name(value="y"), value=null),
        \\    Assign(target=Name(value="foobar"), value=null),
        \\    Return(value=null),
        \\  ]
        \\)
    ));
}
