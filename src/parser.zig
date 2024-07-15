const assert = @import("./assert.zig").assert;
const std = @import("std");
const _t = @import("./token.zig");
const Token = _t.Token;
const TokenType = _t.TokenType;
const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;
const _p = @import("./parselets.zig");
const token_infos = _p.token_infos;
const TokenInfo = _p.TokenInfo;

pub const Parser = struct {
    arena: std.heap.ArenaAllocator,
    lexer: *Lexer,
    cur_token: Token,

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) !Parser {
        const curr = try lexer.nextToken();

        const parser: Parser = .{
            // use an arena to clear at once
            .arena = std.heap.ArenaAllocator.init(allocator),
            .lexer = lexer,
            .cur_token = curr.?,
        };

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.arena.deinit();
    }

    pub fn raiseError(self: *Parser, expected: TokenType, received: TokenType) void {
        _ = self;
        std.debug.panic("expected token to be {any}, got {any} instead\n", .{ expected, received });
    }

    pub fn nextToken(self: *Parser) void {
        // TODO: skip over comments/NL?
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

        return .{ .target = name, .value = self.parseExpression(0) };
    }

    fn getTokenInfo(self: *Parser, token: Token) TokenInfo {
        // SKIP FOR NAME - check if there's a direct match on the token type name
        if (!self.check(.NAME)) {
            if (token_infos.get(@tagName(token.type))) |info| {
                return info;
            }
        }

        // then match if there might be a match on the literal
        if (token_infos.get(token.literal)) |info| {
            return info;
        }

        // otherwise it's a generic NAME
        if (token.type != .NAME) {
            std.debug.panic("Failed to find token info for {any}\n", .{token});
        }
        return token_infos.get(@tagName(TokenType.NAME)).?;
    }

    fn curTokenInfo(self: *Parser) TokenInfo {
        return self.getTokenInfo(self.cur_token);
    }

    /// rbp - right binding power, starting precedence - should start at 0
    pub fn parseExpression(self: *Parser, rbp: u8) ?*const ast.Expr {
        // TODO: handle newlines better
        if (self.check(.NEWLINE) or self.check(.INDENT) or self.check(.DEDENT)) {
            return null;
        }

        const first_token = self.cur_token;

        self.nextToken();

        const first_token_info = self.getTokenInfo(first_token);
        const firstPrefixParser = first_token_info.prefix orelse return null;

        // assign initial 'left' value from prefix parselet of first token
        const left = firstPrefixParser(&first_token_info, self, first_token);
        const leftExpr = self.arena.allocator().create(ast.Expr) catch unreachable;
        leftExpr.* = left;

        // keep track of prev token and info token info
        var prev_token_info = self.curTokenInfo();
        var prev_token = self.cur_token;

        // until we encounter a token more left-binding than current precedence
        while (rbp < prev_token_info.lbp) {
            self.nextToken();

            // at any point if the next token expr does not support being infix, just return
            const infixParser = prev_token_info.infix orelse return leftExpr;

            // wrap current left with the result
            leftExpr.* = infixParser(&prev_token_info, self, leftExpr, prev_token);

            // update token and info
            prev_token = self.cur_token;
            prev_token_info = self.curTokenInfo();
        }

        return leftExpr;
    }

    // fn parseExpressionStatement(self: *Parser) ?ast.Stmt {
    //     const expression = self.parseExpression();
    //
    //     if (self.peekTokenIs(.NEWLINE)) {
    //         self.nextToken();
    //     }
    //
    //     return;
    // }

    fn parseSimpleStatement(self: *Parser) ?ast.Stmt {
        // TODO: check statements like assert, global...

        // Otherwise it's some assignment etc starting with expression
        const maybeName = self.parseExpression(0);

        const name = maybeName orelse return null;

        if (self.match(.EQUAL)) {
            const value = self.parseExpression(0);
            return .{ .assign = .{ .value = value, .target = name } };
        }

        // no other forms matched so it's a standalone expression
        return .{ .expr = .{ .value = name } };
    }

    // Can return multiple as stmts can be semicolon separated
    fn parseStatement(self: *Parser) ?*std.ArrayList(ast.Stmt) {
        const firstStmt = self.parseSimpleStatement() orelse return null;

        var statements = std.ArrayList(ast.Stmt).init(self.arena.allocator());
        statements.append(firstStmt) catch unreachable;

        // handle followup semi-separated statements
        while (true) {
            // no semicolon, just breakout
            if (!self.match(.SEMI)) {
                break;
            }

            // otherwise if there's no newline, consume next statements
            if (self.check(.NEWLINE)) {
                break;
            }

            const matched = self.parseSimpleStatement() orelse break;
            statements.append(matched) catch unreachable;
        }

        // Consume newlines and indent
        _ = self.match(.NEWLINE);
        _ = self.match(.INDENT);
        return &statements;
    }

    pub fn parseProgram(self: *Parser) !ast.Module {
        var module = ast.Module.init(self.arena.allocator());

        while (!self.check(.ENDMARKER)) {
            const statements = self.parseStatement();
            if (statements) |stmts| {
                // defer stmts.deinit();
                try module.body.appendSlice(stmts.items);
            }
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

    const result = try std.fmt.allocPrint(t.allocator, "{s}", .{module});
    defer t.allocator.free(result);
    try want.diff(result);
}

// test "can parse statements" {
//     const input =
//         \\x = 5
//         \\y = 10
//         \\foobar = 838383
//         \\
//     ;
//
//     try checkParserOutput(input, snap(@src(),
//         \\Module(
//         \\  body=[
//         \\    Assign(target=Name(value="x"), value=Constant(value="5")),
//         \\    Assign(target=Name(value="y"), value=Constant(value="10")),
//         \\    Assign(target=Name(value="foobar"), value=Constant(value="838383")),
//         \\  ]
//         \\)
//     ));
// }
//
test "can parse unary expressions" {
    const input =
        \\-5
        \\~9
    ;

    try checkParserOutput(input, snap(@src(),
        \\Module(
        \\  body=[
        \\    Expr(value=UnaryOp(op=USub(), operand=Constant(value="5"))),
        \\    Expr(value=UnaryOp(op=Invert(), operand=Constant(value="9"))),
        \\  ]
        \\)
    ));
}

test "can parse infix expressions" {
    // TODO: something is wrong with advancing tokens
    // TODO: checkout the python copy and trace through it how they handle token advancing
    const input =
        \\5 + 5
        \\5 - 5
        \\5 * 5
        \\5 / 5
        \\5 // 5
        \\5 % 5
    ;

    try checkParserOutput(input, snap(@src(),
        \\Module(
        \\  body=[
        \\    Expr(value=BinOp(left=Constant(value="5"), op=Add(), right=Constant(value="5"))),
        \\    Expr(value=BinOp(left=Constant(value="5"), op=Sub(), right=Constant(value="5"))),
        \\    Expr(value=BinOp(left=Constant(value="5"), op=Mult(), right=Constant(value="5"))),
        \\    Expr(value=BinOp(left=Constant(value="5"), op=Div(), right=Constant(value="5"))),
        \\    Expr(value=BinOp(left=Constant(value="5"), op=FloorDiv(), right=Constant(value="5"))),
        \\    Expr(value=BinOp(left=Constant(value="5"), op=Mod(), right=Constant(value="5"))),
        \\  ]
        \\)
    ));
}
