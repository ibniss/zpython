const assert = @import("./assert.zig").assert;
const std = @import("std");
const _t = @import("./token.zig");
const Token = _t.Token;
const TokenType = _t.TokenType;
const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: *Lexer,
    cur_token: Token,

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) !Parser {
        const curr = try lexer.nextToken();

        const parser: Parser = .{
            .allocator = allocator,
            .lexer = lexer,
            .cur_token = curr.?,
        };

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        _ = self;
    }

    pub fn raiseError(self: *Parser, expected: TokenType, received: TokenType) void {
        _ = self;
        std.debug.panic("expected token to be {any}, got {any} instead\n", .{ expected, received });
    }

    pub fn nextToken(self: *Parser) void {
        // TODO: skip over comments/NL?
        std.debug.print("current {any}\n", .{self.cur_token});
        self.cur_token = (self.lexer.nextToken() catch unreachable).?;
    }

    fn check(self: *Parser, typ: TokenType) bool {
        return self.cur_token.type == typ;
    }

    fn match(self: *Parser, typ: TokenType) bool {
        const res = self.check(typ);
        if (res and typ != .ENDMARKER) {
            self.nextToken();
        }
        return res;
    }

    fn expect(self: *Parser, typ: TokenType) void {
        const res = self.match(typ);

        if (!res) {
            self.raiseError(typ, self.cur_token.type);
        }
    }

    //     fn parseReturn(self: *Parser) ast.Return {
    //         const return_token = self.cur_token.?;
    //         self.nextToken();
    //
    //         // TODO: parse expression rather than skipping
    //         var i: usize = 0;
    //         while (!self.curTokenIs(.NEWLINE) and !self.curTokenIs(.ENDMARKER)) {
    //             self.nextToken();
    //             i += 1;
    //             if (i > 10) {
    //                 @panic("loop:");
    //             }
    //         }
    //
    //         return .{ .token = return_token, .value = null };
    //     }
    //
    fn parseAssign(self: *Parser) ?ast.Assign {
        if (!self.check(.NAME)) {
            return null;
        }

        const name = ast.Name{ .token = self.cur_token, .value = self.cur_token.literal };
        self.nextToken();

        // ensure next token is a assign, move forward
        self.expect(.EQUAL);

        // TODO: eval expr
        while (!(self.check(.NEWLINE) or self.check(.ENDMARKER))) {
            std.debug.print("skipping current {any}, {any}, {any}\n", .{ self.cur_token, self.check(.NEWLINE), self.check(.ENDMARKER) });
            self.nextToken();
        }

        return .{ .target = name, .value = null };
    }

    // fn parseExpression(self: *Parser) ?ast.Expr {
    //
    //
    // }
    //
    // fn parseExpressionStatement(self: *Parser) ?ast.Stmt {
    //     const expression = self.parseExpression();
    //
    //     if (self.peekTokenIs(.NEWLINE)) {
    //         self.nextToken();
    //     }
    //
    //     return;
    // }

    fn parseStatement(self: *Parser) ?ast.Stmt {
        return switch (self.cur_token.type) {
            // .RETURN => .{ .ret = self.parseReturn() },
            else => els: {
                // TODO: match equal should be checked after other simple statements (del, global, etc)
                // no keywords were matched so check if next token is an equal
                if (self.parseAssign()) |ass| {
                    break :els .{ .assign = ass };
                }

                break :els null;
                // break :els self.parseExpressionStatement();
            },
        };
    }

    pub fn parseProgram(self: *Parser) !ast.Module {
        var module = ast.Module.init(self.allocator);

        while (!self.check(.ENDMARKER)) {
            std.debug.print("loop, current {any}\n", .{self.cur_token});
            const stmt = self.parseStatement();
            if (stmt) |s| {
                try module.body.append(s);
            }
            // TODO: handle input not having a newline before the end? or just append in lexer
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

    //if (parser.errors.items.len > 0) {
    //    std.debug.print("Encountered {} parser errors:\n", .{parser.errors.items.len});
    //    for (parser.errors.items) |err| {
    //        std.debug.print("parser error: {s}\n", .{err});
    //    }
    //    std.debug.print("Parser output: {s}\n", .{module});
    //    // fail here
    //    try t.expect(parser.errors.items.len == 0);
    //}

    const result = try std.fmt.allocPrint(t.allocator, "{s}", .{module});
    defer t.allocator.free(result);
    try want.diff(result);
}

test "can parse statements" {
    const input =
        \\x = 5
        \\y = 10
        \\foobar = 838383
        \\
    ;

    try checkParserOutput(input, snap(@src(),
        \\Module(
        \\  body=[
        \\    Assign(target=Name(value="x"), value=null),
        \\    Assign(target=Name(value="y"), value=null),
        \\    Assign(target=Name(value="foobar"), value=null),
        \\  ]
        \\)
    ));
}

// test "can parse expressions" {
//     const input =
//         \\foobar
//     ;
//
//     try checkParserOutput(input, snap(@src(),
//         \\
//     ));
// }
