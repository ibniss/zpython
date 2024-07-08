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

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) !Parser {
        const curr = try lexer.nextToken();
        const next = try lexer.nextToken();

        return .{ .allocator = allocator, .lexer = lexer, .curToken = curr, .peekToken = next };
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

        return false;
    }

    fn parseReturn(self: *Parser) ast.Return {
        const returnToken = self.curToken.?;
        self.nextToken();

        // TODO: parse expression rather than skipping
        self.nextToken();

        return .{ .token = returnToken, .value = null };
    }
    //
    // fn parseAssign(self: *Parser) ast.Assign {
    //    return
    // }

    fn parseStatement(self: *Parser) ?ast.Stmt {
        return switch (self.curToken.?.type) {
            .RETURN => .{ .ret = self.parseReturn() },
            else => els: {
                // // no keywords were matched so check if next token is an equal
                // if (self.peekTokenIs(.EQUAL)) {
                //     break :els self.parseAssign();
                // }

                break :els null;
            },
        };
    }

    pub fn parseProgram(self: *Parser) !*const ast.Module {
        var module = ast.Module.init(self.allocator);

        while (self.curToken != null and !self.curTokenIs(.EOF)) {
            std.debug.print("Calling parse statement, current: {any}\n", .{self.curToken});
            const stmt = self.parseStatement();
            if (stmt) |s| {
                try module.body.append(s);
            }
            self.nextToken();
        }

        return &module;
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

    const module = try parser.parseProgram();
    defer module.deinit();

    // build a string by printing into the buffer
    var strings = std.ArrayList([]u8).init(t.allocator);
    defer strings.deinit();
    var stringsLen: usize = 0;
    for (module.body.items) |stmt| {
        const printed = try std.fmt.allocPrint(t.allocator, "{s}\n", .{stmt});

        try strings.append(printed);
        stringsLen += printed.len;
    }
    const outArr = try t.allocator.alloc(u8, stringsLen);
    defer t.allocator.free(outArr);
    var i: usize = 0;
    for (strings.items) |str| {
        // we can only free the string after we use it here and copy the underlying bytes into the output string
        defer t.allocator.free(str);
        @memcpy(outArr[i .. i + str.len], str);
        i += str.len;
    }

    try want.diff(outArr);
}

test "can parse" {
    const input =
        \\return 5
    ;

    try checkParserOutput(input, snap(@src(),
        \\Return(value=null)
        \\
        \\
    ));
}
