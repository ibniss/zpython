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
        std.debug.print("hello! token: {any}\n", .{curr});

        const parser: Parser = .{
            // used for AST allocations
            .arena = std.heap.ArenaAllocator.init(allocator),
            .lexer = lexer,
            .cur_token = curr.?,
        };

        return parser;
    }

    pub fn deinit(self: *const Parser) void {
        self.arena.deinit();
    }

    pub fn raiseError(self: *Parser, expected: TokenType, received: TokenType) void {
        _ = self;
        std.debug.panic("expected token to be {any}, got {any} instead\n", .{ expected, received });
    }

    pub fn nextToken(self: *Parser) void {
        // TODO: skip over comments/NL?
        self.cur_token = (self.lexer.nextToken() catch unreachable).?;
        std.debug.print("token: {s}\n", .{self.cur_token});
    }

    /// Can be called with []const u8 or TokenType (must be used explicitly)
    fn check(self: *Parser, typ: anytype) bool {
        const T = @TypeOf(typ);

        if (T == TokenType) {
            return self.cur_token.type == typ;
        }

        // array of u8 ~ string
        if (std.meta.Elem(T) == u8) {
            return self.cur_token.type == .NAME and std.mem.eql(u8, self.cur_token.literal, typ);
        }

        @compileLog(.{T});
        @compileLog(.{@typeInfo(T)});
        @compileLog(.{@typeInfo(@typeInfo(T).Pointer.child)});
        @compileError("Unsupported type, see received type in log above");
    }

    /// Can be called with []const u8 or TokenType (must be used explicitly)
    fn match(self: *Parser, typ: anytype) bool {
        const res = self.check(typ);
        if (res and !(@TypeOf(typ) == TokenType and typ == .ENDMARKER)) {
            self.nextToken();
        }
        return res;
    }

    /// Can be called with []const u8 or TokenType (must be used explicitly)
    pub fn expect(self: *Parser, typ: anytype) void {
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
        if (!self.check(TokenType.NAME)) {
            return null;
        }

        const name = ast.Name{ .token = self.cur_token, .value = self.cur_token.literal };
        self.nextToken();

        // ensure next token is a assign, move forward
        self.expect(TokenType.EQUAL);

        return .{ .target = name, .value = self.parseExpression(0) };
    }

    fn getTokenInfo(self: *Parser, token: Token) TokenInfo {
        _ = self;
        // SKIP FOR NAME - check if there's a direct match on the token type name
        if (token.type != TokenType.NAME) {
            if (token_infos.get(@tagName(token.type))) |info| {
                return info;
            }
        }

        // then match if there might be a match on the literal
        if (token_infos.get(token.literal)) |info| {
            return info;
        }

        // otherwise treat it as a generic NAME
        return token_infos.get(@tagName(TokenType.NAME)).?;
    }

    fn curTokenInfo(self: *Parser) TokenInfo {
        return self.getTokenInfo(self.cur_token);
    }

    /// rbp - right binding power, starting precedence - should start at 0
    pub fn parseExpression(self: *Parser, rbp: u8) ?*const ast.Expr {
        // TODO: handle newlines better
        if (self.check(TokenType.NEWLINE) or self.check(TokenType.INDENT) or self.check(TokenType.DEDENT)) {
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

    fn parseSimpleStatement(self: *Parser) ?ast.Stmt {
        // TODO: check statements like assert, global...

        // Otherwise it's some assignment etc starting with expression
        const maybeName = self.parseExpression(0);
        std.debug.print("returned expr {?}\n", .{maybeName});

        const name = maybeName orelse return null;

        if (self.match(TokenType.EQUAL)) {
            const value = self.parseExpression(0);
            return .{ .assign = .{ .value = value.?, .target = name } };
        }

        // no other forms matched so it's a standalone expression
        return .{ .expr = .{ .value = name } };
    }

    fn handleIf(self: *Parser) ast.Stmt {
        std.debug.print("IN if, curren: {s}\n", .{self.cur_token});
        const expression = self.parseExpression(0) orelse std.debug.panic("Expected expression, found {}\n", .{self.cur_token});
        std.debug.print("after expr in if, expr: {s}, tok: {s}\n", .{ expression, self.cur_token });
        self.expect(TokenType.COLON);
        std.debug.print("expected colon, cur: {s}\n", .{self.cur_token});
        // parse indented block
        const body = self.parseBlock();
        if (true) {
            // std.debug.print("Expr: {any}\n", .{expression});
            // std.debug.print("Body {any}\n", .{body.items[0]});
            // @panic("umm");
        }

        var or_else = std.ArrayList(ast.Stmt).init(self.arena.allocator());

        if (self.match("elif")) {
            or_else.append(self.handleIf()) catch unreachable;
        } else if (self.match("else")) {
            self.expect(TokenType.COLON);
            var else_block = self.parseBlock();
            const else_block_slice = else_block.toOwnedSlice() catch unreachable;
            or_else.appendSlice(else_block_slice) catch unreachable;
        }

        return ast.Stmt{
            .if_stmt = .{
                .test_expr = expression,
                .body = body,
                .or_else = or_else,
            },
        };
    }

    fn parseIfStmt(self: *Parser) ?ast.Stmt {
        if (self.match("if")) {
            return self.handleIf();
        }

        return null;
    }

    ///compound_stmt:
    /// | function_def
    /// | if_stmt
    /// | class_def
    /// | with_stmt
    /// | for_stmt
    /// | try_stmt
    /// | while_stmt
    /// | match_stmt
    fn parseCompoundStatement(self: *Parser) ?ast.Stmt {
        if (self.parseIfStmt()) |if_stmt| {
            return if_stmt;
        }

        return null;
    }

    fn parseBlock(self: *Parser) std.ArrayList(ast.Stmt) {
        std.debug.print("in parse block, token: {s}\n", .{self.cur_token});
        var statements = std.ArrayList(ast.Stmt).init(self.arena.allocator());

        if (self.match(TokenType.NEWLINE)) {
            std.debug.print("Match newline\n", .{});
            self.expect(TokenType.INDENT);

            while (!self.match(TokenType.DEDENT)) {
                const stmt = self.parseStatement();
                // std.debug.print("got stmt {any}\n", .{stmt});
                statements.insertSlice(statements.items.len, stmt.?.items) catch unreachable;
            }

            return statements;
        }

        const simple = self.parseSimpleStatement();
        statements.append(simple.?) catch unreachable;
        return statements;
    }

    // Can return multiple as stmts can be semicolon separated
    fn parseStatement(self: *Parser) ?std.ArrayList(ast.Stmt) {
        var statements = std.ArrayList(ast.Stmt).init(self.arena.allocator());

        if (self.parseCompoundStatement()) |compound| {
            statements.append(compound) catch unreachable;
            return statements;
        }

        const firstStmt = self.parseSimpleStatement() orelse return null;
        statements.append(firstStmt) catch unreachable;

        // handle followup semi-separated statements
        while (true) {
            // no semicolon, just breakout
            if (!self.match(TokenType.SEMI)) {
                break;
            }

            // otherwise if there's no newline, consume next statements
            if (self.check(TokenType.NEWLINE)) {
                break;
            }

            const matched = self.parseSimpleStatement() orelse break;
            statements.append(matched) catch unreachable;
        }

        // Consume newlines and indent
        _ = self.match(TokenType.NEWLINE);
        _ = self.match(TokenType.INDENT);
        return statements;
    }

    /// Returns an owned module
    /// Caller is responsible for freeing the module memory
    pub fn parseProgram(self: *Parser) !ast.Module {
        var module = ast.Module.init(&self.arena);
        std.debug.print("arena {any}\n", .{self.arena});

        while (!self.check(TokenType.ENDMARKER)) {
            var statements = self.parseStatement();
            if (statements) |*stmts| {
                const stmts_slice = try stmts.toOwnedSlice();
                std.debug.print("got stmts {any}\n", .{stmts_slice});
                try module.body.appendSlice(stmts_slice);
            }
        }

        // transfer arena ownership
        module.arena = self.arena;

        return module;
    }
};

const t = std.testing;
const Snap = @import("./snaptest.zig").Snap;
const snap = Snap.snap;

// util to perform snapshot testing on a given input
fn checkParserOutput(module: ast.Module, want: Snap) !void {
    var buf: [2048]u8 = undefined;
    const result = try std.fmt.bufPrint(&buf, "{s}", .{module});
    try want.diff(result);
}

fn checkStringified(module: ast.Module, want: Snap) !void {
    var stringified = std.ArrayList(u8).init(t.allocator);
    defer stringified.deinit();
    module.stringify(stringified.writer());
    const asSlice = try stringified.toOwnedSlice();
    defer t.allocator.free(asSlice);
    try want.diff(asSlice);
}

fn parseModule(input: []const u8) !ast.Module {
    var lexer = try Lexer.init(t.allocator, input);
    defer lexer.deinit();
    var parser = try Parser.init(t.allocator, &lexer);

    const module = try parser.parseProgram();
    return module;
}

test "can parse statements" {
    const input =
        \\x = 5
        \\y = 10
        \\foobar = 838383
        \\
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\Module(
        \\  body=[
        \\Assign(target=Name(value="x"), value=Constant(value="5")),
        \\Assign(target=Name(value="y"), value=Constant(value="10")),
        \\Assign(target=Name(value="foobar"), value=Constant(value="838383")),
        \\  ]
        \\)
    ));
}

test "can parse name and constant expressions" {
    const input =
        \\foobar
        \\True
        \\False
        \\None
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\Module(
        \\  body=[
        \\Expr(value=Name(value="foobar")),
        \\Expr(value=Constant(value="True")),
        \\Expr(value=Constant(value="False")),
        \\Expr(value=Constant(value="None")),
        \\  ]
        \\)
    ));
}

test "can parse unary expressions" {
    const input =
        \\-5
        \\~9
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\Module(
        \\  body=[
        \\Expr(value=UnaryOp(op=USub(), operand=Constant(value="5"))),
        \\Expr(value=UnaryOp(op=Invert(), operand=Constant(value="9"))),
        \\  ]
        \\)
    ));

    try checkStringified(module, snap(@src(),
        \\(-5)
        \\(~9)
    ));
}

test "can parse infix expressions" {
    const input =
        \\5 + 5
        \\5 - 5
        \\5 * 5
        \\5 / 5
        \\5 // 5
        \\5 % 5
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\Module(
        \\  body=[
        \\Expr(value=BinOp(left=Constant(value="5"), op=Add(), right=Constant(value="5"))),
        \\Expr(value=BinOp(left=Constant(value="5"), op=Sub(), right=Constant(value="5"))),
        \\Expr(value=BinOp(left=Constant(value="5"), op=Mult(), right=Constant(value="5"))),
        \\Expr(value=BinOp(left=Constant(value="5"), op=Div(), right=Constant(value="5"))),
        \\Expr(value=BinOp(left=Constant(value="5"), op=FloorDiv(), right=Constant(value="5"))),
        \\Expr(value=BinOp(left=Constant(value="5"), op=Mod(), right=Constant(value="5"))),
        \\  ]
        \\)
    ));

    try checkStringified(module, snap(@src(),
        \\(5 + 5)
        \\(5 - 5)
        \\(5 * 5)
        \\(5 / 5)
        \\(5 // 5)
        \\(5 % 5)
    ));
}

test "can handle infix precedence" {
    const input =
        \\a > 5
        \\b < 4
        \\c == 5
        \\c != 5
        \\-a * b
        \\a+b+c
        \\a+b-c
        \\a*b*c
        \\a*b/c
        \\a+b/c
        \\a+b*c+d/e-f
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\Module(
        \\  body=[
        \\Expr(value=Compare(left=Name(value="a"),ops=[Gt()], comparators=[Constant(value="5")])),
        \\Expr(value=Compare(left=Name(value="b"),ops=[Lt()], comparators=[Constant(value="4")])),
        \\Expr(value=Compare(left=Name(value="c"),ops=[Eq()], comparators=[Constant(value="5")])),
        \\Expr(value=Compare(left=Name(value="c"),ops=[NotEq()], comparators=[Constant(value="5")])),
        \\Expr(value=BinOp(left=UnaryOp(op=USub(), operand=Name(value="a")), op=Mult(), right=Name(value="b"))),
        \\Expr(value=BinOp(left=BinOp(left=Name(value="a"), op=Add(), right=Name(value="b")), op=Add(), right=Name(value="c"))),
        \\Expr(value=BinOp(left=BinOp(left=Name(value="a"), op=Add(), right=Name(value="b")), op=Sub(), right=Name(value="c"))),
        \\Expr(value=BinOp(left=BinOp(left=Name(value="a"), op=Mult(), right=Name(value="b")), op=Mult(), right=Name(value="c"))),
        \\Expr(value=BinOp(left=BinOp(left=Name(value="a"), op=Mult(), right=Name(value="b")), op=Div(), right=Name(value="c"))),
        \\Expr(value=BinOp(left=Name(value="a"), op=Add(), right=BinOp(left=Name(value="b"), op=Div(), right=Name(value="c")))),
        \\Expr(value=BinOp(left=BinOp(left=BinOp(left=Name(value="a"), op=Add(), right=BinOp(left=Name(value="b"), op=Mult(), right=Name(value="c"))), op=Add(), right=BinOp(left=Name(value="d"), op=Div(), right=Name(value="e"))), op=Sub(), right=Name(value="f"))),
        \\  ]
        \\)
    ));

    try checkStringified(module, snap(@src(),
        \\(a > 5)
        \\(b < 4)
        \\(c == 5)
        \\(c != 5)
        \\((-a) * b)
        \\((a + b) + c)
        \\((a + b) - c)
        \\((a * b) * c)
        \\((a * b) / c)
        \\(a + (b / c))
        \\(((a + (b * c)) + (d / e)) - f)
    ));
}

test "can handle chained comparators" {
    const input: []const u8 =
        \\ 1 < 2 > 3
        \\5 > 4 == 3 < 4
        \\5 < 4 != 3 > 4
        \\3 + 4*5 == 3*1 + 4*5
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\Module(
        \\  body=[
        \\Expr(value=Compare(left=Constant(value="1"),ops=[Lt(),Gt()], comparators=[Constant(value="2"),Constant(value="3")])),
        \\Expr(value=Compare(left=Constant(value="5"),ops=[Gt(),Eq(),Lt()], comparators=[Constant(value="4"),Constant(value="3"),Constant(value="4")])),
        \\Expr(value=Compare(left=Constant(value="5"),ops=[Lt(),NotEq(),Gt()], comparators=[Constant(value="4"),Constant(value="3"),Constant(value="4")])),
        \\Expr(value=Compare(left=BinOp(left=Constant(value="3"), op=Add(), right=BinOp(left=Constant(value="4"), op=Mult(), right=Constant(value="5"))),ops=[Eq()], comparators=[BinOp(left=BinOp(left=Constant(value="3"), op=Mult(), right=Constant(value="1")), op=Add(), right=BinOp(left=Constant(value="4"), op=Mult(), right=Constant(value="5")))])),
        \\  ]
        \\)
    ));

    try checkStringified(module, snap(@src(),
        \\(1 < 2 > 3)
        \\(5 > 4 == 3 < 4)
        \\(5 < 4 != 3 > 4)
        \\((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))
    ));
}

test "can handle grouped expressions" {
    const input: []const u8 =
        \\1 + (2 + 3) + 4
        \\(5 + 5) * 2
        \\2 / (5 + 5)
        \\-(5 + 5)
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\Module(
        \\  body=[
        \\Expr(value=BinOp(left=BinOp(left=Constant(value="1"), op=Add(), right=BinOp(left=Constant(value="2"), op=Add(), right=Constant(value="3"))), op=Add(), right=Constant(value="4"))),
        \\Expr(value=BinOp(left=BinOp(left=Constant(value="5"), op=Add(), right=Constant(value="5")), op=Mult(), right=Constant(value="2"))),
        \\Expr(value=BinOp(left=Constant(value="2"), op=Div(), right=BinOp(left=Constant(value="5"), op=Add(), right=Constant(value="5")))),
        \\Expr(value=UnaryOp(op=USub(), operand=BinOp(left=Constant(value="5"), op=Add(), right=Constant(value="5")))),
        \\  ]
        \\)
    ));

    try checkStringified(module, snap(@src(),
        \\((1 + (2 + 3)) + 4)
        \\((5 + 5) * 2)
        \\(2 / (5 + 5))
        \\(-(5 + 5))
    ));
}

test "can handle if statements" {
    const input: []const u8 =
        \\if True:
        \\  False
        \\if a > 5:
        \\  foo = 234
        \\  baz = 123
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\Module(
        \\  body=[
        \\If(test=Constant(value="True"), body=[Expr(value=Constant(value="False"))], orelse=[]),
        \\If(test=Compare(left=Name(value="a"),ops=[Gt()], comparators=[Constant(value="5")]), body=[Assign(target=Name(value="foo"), value=Constant(value="234")),Assign(target=Name(value="baz"), value=Constant(value="123"))], orelse=[]),
        \\  ]
        \\)
    ));
}

test "can handle if-else statements" {
    const input: []const u8 =
        \\if True:
        \\  False
        \\else:
        \\  True
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\Module(
        \\  body=[
        \\If(test=Constant(value="True"), body=[Expr(value=Constant(value="False"))], orelse=[Expr(value=Constant(value="True"))]),
        \\  ]
        \\)
    ));
}

test "can handle if-elif-else statements" {
    const input: []const u8 =
        \\if True:
        \\  1
        \\elif False:
        \\  2
        \\else:
        \\  3
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\Module(
        \\  body=[
        \\If(test=Constant(value="True"), body=[Expr(value=Constant(value="1"))], orelse=[If(test=Constant(value="False"), body=[Expr(value=Constant(value="2"))], orelse=[Expr(value=Constant(value="3"))])]),
        \\  ]
        \\)
    ));
}
