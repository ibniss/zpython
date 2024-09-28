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

const BP_UNTIL_COMMA = 6;

pub const Parser = struct {
    arena: std.heap.ArenaAllocator,
    lexer: *Lexer,
    cur_token: Token,

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) !Parser {
        const curr = try lexer.nextToken();

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

    pub fn raiseError(self: *Parser, expected: anytype, received: TokenType) void {
        _ = self;
        std.debug.panic("expected token to be {any}, got {any} instead\n", .{ expected, received });
    }

    pub fn nextToken(self: *Parser) void {
        // TODO: skip over comments/NL?
        self.cur_token = (self.lexer.nextToken() catch unreachable).?;
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
        @compileError("Unsupported type, see received type in log above");
    }

    /// Can be called with []const u8 or TokenType (must be used explicitly)
    fn match(self: *Parser, typ: anytype) ?Token {
        const res = self.check(typ);
        const tok = self.cur_token;
        if (res and !(@TypeOf(typ) == TokenType and typ == .ENDMARKER)) {
            self.nextToken();
        }
        return if (res) tok else null;
    }

    /// Can be called with []const u8 or TokenType (must be used explicitly)
    pub fn expect(self: *Parser, typ: anytype) Token {
        const res = self.match(typ);

        if (res == null) {
            self.raiseError(typ, self.cur_token.type);
        }

        return res.?;
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

    // small stmt
    fn parseSimpleStatement(self: *Parser) ?ast.Stmt {
        // TODO: check statements like assert, global...
        if (self.match("return")) |ret| {
            const expr = self.parseExpression(0);
            return .{ .ret = .{ .value = expr, .token = ret } };
        }

        // Otherwise it's some assignment etc starting with expression
        const maybe_name = self.parseExpression(0);

        const name = maybe_name orelse return null;

        if (self.match(TokenType.EQUAL)) |_| {
            const value = self.parseExpression(0);
            return .{ .assign = .{ .value = value.?, .target = name } };
        }

        // no other forms matched so it's a standalone expression
        return .{ .expr = .{ .value = name } };
    }

    fn handleIf(self: *Parser) ast.Stmt {
        const expression = self.parseExpression(0) orelse std.debug.panic("Expected expression, found {}\n", .{self.cur_token});
        _ = self.expect(TokenType.COLON);

        // parse indented block
        const body = self.parseBlock();

        var or_else = std.ArrayList(ast.Stmt).init(self.arena.allocator());

        if (self.match("elif")) |_| {
            or_else.append(self.handleIf()) catch unreachable;
        } else if (self.match("else")) |_| {
            _ = self.expect(TokenType.COLON);
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
        if (self.match("if")) |_| {
            return self.handleIf();
        }

        return null;
    }

    // TODO: debug loop
    fn parseCallArgs(self: *Parser) struct {
        args: std.ArrayList(*const ast.Expr),
        kwargs: std.ArrayList(ast.Keyword),
    } {
        const args = std.ArrayList(ast.Expr).init(self.arena.allocator());
        var kwargs = std.ArrayList(ast.Keyword).init(self.arena.allocator());

        // if there are any arguments
        if (!self.check(TokenType.RPAR)) {
            while (true) {
                var starred: ?[]const u8 = null;

                // handle *args/**kwargs
                if (self.match("*")) |_| {
                    starred = "*";
                } else if (self.match("**")) |_| {
                    starred = "**";
                }

                // match until comma
                const arg = self.parseExpression(BP_UNTIL_COMMA);

                // TODO: handle GenComp

                if (self.match(TokenType.EQUAL)) |_| {
                    // arg must be ast.Name
                    switch (arg.?) {
                        .Name => |name| {
                            const val = self.parseExpression(BP_UNTIL_COMMA);
                            kwargs.append(ast.Keyword{ .value = val, .identifier = name.value }) catch unreachable;
                        },
                        else => std.debug.panic("call expression matched must be ast.Name if followed by '='"),
                    }
                } else {
                    if (std.mem.eql(u8, starred, "**")) {
                        kwargs.append(ast.Keyword{ .value = arg.?, .identifier = null }) catch unreachable;
                    } else {
                        // TODO
                        // if (std.mem.eql(u8, starred, "*")) {
                        //     arg = ast.Starred(...)
                        // }
                        args.append(arg.?);
                    }
                }

                // skip over commas
                _ = self.match(TokenType.COMMA);

                if (self.check(TokenType.RPAR)) |_| {
                    break;
                }
            }
        }

        _ = self.expect(TokenType.RPAR);

        return .{ .args = args, .kwargs = kwargs };
    }

    fn parseArgs(self: *Parser) ast.Arguments {
        // TODO
        // var posonlyargs = std.ArrayList(ast.Arg).init(self.arena.allocator());
        var args = std.ArrayList(ast.Arg).init(self.arena.allocator());
        var kwonlyargs = std.ArrayList(ast.Arg).init(self.arena.allocator());
        var kw_defaults = std.ArrayList(?ast.Constant).init(self.arena.allocator());
        var defaults = std.ArrayList(ast.Constant).init(self.arena.allocator());

        var kw_only = false;

        var vararg: ?ast.Arg = null;
        var match_vararg = false;

        var kwarg: ?ast.Arg = null;
        var match_kwarg = false;

        // until end of args - colon for lambda, rpar for standard function
        while (!self.check(TokenType.RPAR) and !self.check(TokenType.COLON)) {
            if (self.match(TokenType.STAR)) |_| {
                // after the star the rest of the args are kw only
                kw_only = true;

                if (self.match(TokenType.COMMA)) |_| {
                    continue;
                }

                if (match_vararg) {
                    std.debug.panic("Found more than one vararg", .{});
                }

                match_vararg = true;
            } else if (self.match(TokenType.DOUBLESTAR)) |_| {
                if (match_kwarg) {
                    std.debug.panic("Found more than one kwarg", .{});
                }

                match_kwarg = true;
            }

            const arg = ast.Arg{ .arg = self.expect(TokenType.NAME).literal };

            // TODO: match annotation here

            if (match_vararg) {
                vararg = arg;
                _ = self.match(TokenType.COMMA);
                continue;
            } else if (match_kwarg) {
                kwarg = arg;
                _ = self.match(TokenType.COMMA);
                continue;
            }

            if (kw_only) {
                kwonlyargs.append(arg) catch unreachable;
            } else {
                args.append(arg) catch unreachable;
            }

            // default
            if (self.match(TokenType.EQUAL)) |_| {
                const default = self.parseExpression(BP_UNTIL_COMMA).?;

                switch (default.*) {
                    .constant => |cst| {
                        if (kw_only) {
                            kw_defaults.append(cst) catch unreachable;
                        } else {
                            defaults.append(cst) catch unreachable;
                        }
                    },
                    else => unreachable,
                }
            } else if (kw_only) {
                kw_defaults.append(null) catch unreachable;
            }

            _ = self.match(TokenType.COMMA);
        }

        return .{
            // .posonlyargs = posonlyargs,
            .args = args,
            .kwonlyargs = kwonlyargs,
            .kw_defaults = kw_defaults,
            .defaults = defaults,
            .vararg = vararg,
            .kwarg = kwarg,
        };
    }

    fn parseFunctionDef(self: *Parser) ?ast.Stmt {
        if (self.match("def") == null) {
            return null;
        }
        // TODO: async
        // TODO: decorators

        const func_name = self.expect(TokenType.NAME);
        _ = self.expect(TokenType.LPAR);
        const arg_spec = self.parseArgs();
        _ = self.expect(TokenType.RPAR);
        // TODO: support type annotation
        // const returns = if (self.match("->")) self.parseExpression(0) else null;
        _ = self.expect(TokenType.COLON);

        const body = self.parseBlock();

        return .{ .func_def = .{
            .name = func_name.literal,
            .args = arg_spec,
            .body = body,
        } };
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
        if (self.parseFunctionDef()) |func_def| {
            return func_def;
        }

        if (self.parseIfStmt()) |if_stmt| {
            return if_stmt;
        }

        return null;
    }

    fn parseBlock(self: *Parser) std.ArrayList(ast.Stmt) {
        var statements = std.ArrayList(ast.Stmt).init(self.arena.allocator());

        if (self.match(TokenType.NEWLINE)) |_| {
            _ = self.expect(TokenType.INDENT);

            while (self.match(TokenType.DEDENT) == null) {
                const stmt = self.parseStatement();
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
            if (self.match(TokenType.SEMI) == null) {
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

        while (!self.check(TokenType.ENDMARKER)) {
            var statements = self.parseStatement();
            if (statements) |*stmts| {
                const stmts_slice = try stmts.toOwnedSlice();
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
    const out = try ast.dumpAlloc(module, t.allocator, 4);
    defer t.allocator.free(out);
    try want.diff(out);
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
        \\    body=[
        \\        Assign(
        \\            target=Name(value=x),
        \\            value=Constant(value=5)
        \\        ),
        \\        Assign(
        \\            target=Name(value=y),
        \\            value=Constant(value=10)
        \\        ),
        \\        Assign(
        \\            target=Name(value=foobar),
        \\            value=Constant(value=838383)
        \\        ),
        \\    ]
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
        \\    body=[
        \\        Expr(
        \\            value=Name(value=foobar)
        \\        ),
        \\        Expr(
        \\            value=Constant(value=True)
        \\        ),
        \\        Expr(
        \\            value=Constant(value=False)
        \\        ),
        \\        Expr(
        \\            value=Constant(value=None)
        \\        ),
        \\    ]
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
        \\    body=[
        \\        Expr(
        \\            value=UnaryOp(
        \\                USub(),
        \\                operand=Constant(value=5)
        \\            )
        \\        ),
        \\        Expr(
        \\            value=UnaryOp(
        \\                Invert(),
        \\                operand=Constant(value=9)
        \\            )
        \\        ),
        \\    ]
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
        \\    body=[
        \\        Expr(
        \\            value=BinOp(
        \\                left=Constant(value=5),
        \\                op=Add(),
        \\                right=Constant(value=5)
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=Constant(value=5),
        \\                op=Sub(),
        \\                right=Constant(value=5)
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=Constant(value=5),
        \\                op=Mult(),
        \\                right=Constant(value=5)
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=Constant(value=5),
        \\                op=Div(),
        \\                right=Constant(value=5)
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=Constant(value=5),
        \\                op=FloorDiv(),
        \\                right=Constant(value=5)
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=Constant(value=5),
        \\                op=Mod(),
        \\                right=Constant(value=5)
        \\            )
        \\        ),
        \\    ]
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
        \\    body=[
        \\        Expr(
        \\            value=Compare(
        \\                left=Name(value=a),
        \\                ops=[
        \\                    Gt()
        \\                ],
        \\                comparators=[
        \\                    Constant(value=5)
        \\                ]
        \\            )
        \\        ),
        \\        Expr(
        \\            value=Compare(
        \\                left=Name(value=b),
        \\                ops=[
        \\                    Lt()
        \\                ],
        \\                comparators=[
        \\                    Constant(value=4)
        \\                ]
        \\            )
        \\        ),
        \\        Expr(
        \\            value=Compare(
        \\                left=Name(value=c),
        \\                ops=[
        \\                    Eq()
        \\                ],
        \\                comparators=[
        \\                    Constant(value=5)
        \\                ]
        \\            )
        \\        ),
        \\        Expr(
        \\            value=Compare(
        \\                left=Name(value=c),
        \\                ops=[
        \\                    NotEq()
        \\                ],
        \\                comparators=[
        \\                    Constant(value=5)
        \\                ]
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=UnaryOp(
        \\                    USub(),
        \\                    operand=Name(value=a)
        \\                ),
        \\                op=Mult(),
        \\                right=Name(value=b)
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=BinOp(
        \\                    left=Name(value=a),
        \\                    op=Add(),
        \\                    right=Name(value=b)
        \\                ),
        \\                op=Add(),
        \\                right=Name(value=c)
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=BinOp(
        \\                    left=Name(value=a),
        \\                    op=Add(),
        \\                    right=Name(value=b)
        \\                ),
        \\                op=Sub(),
        \\                right=Name(value=c)
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=BinOp(
        \\                    left=Name(value=a),
        \\                    op=Mult(),
        \\                    right=Name(value=b)
        \\                ),
        \\                op=Mult(),
        \\                right=Name(value=c)
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=BinOp(
        \\                    left=Name(value=a),
        \\                    op=Mult(),
        \\                    right=Name(value=b)
        \\                ),
        \\                op=Div(),
        \\                right=Name(value=c)
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=Name(value=a),
        \\                op=Add(),
        \\                right=BinOp(
        \\                    left=Name(value=b),
        \\                    op=Div(),
        \\                    right=Name(value=c)
        \\                )
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=BinOp(
        \\                    left=BinOp(
        \\                        left=Name(value=a),
        \\                        op=Add(),
        \\                        right=BinOp(
        \\                            left=Name(value=b),
        \\                            op=Mult(),
        \\                            right=Name(value=c)
        \\                        )
        \\                    ),
        \\                    op=Add(),
        \\                    right=BinOp(
        \\                        left=Name(value=d),
        \\                        op=Div(),
        \\                        right=Name(value=e)
        \\                    )
        \\                ),
        \\                op=Sub(),
        \\                right=Name(value=f)
        \\            )
        \\        ),
        \\    ]
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
        \\    body=[
        \\        Expr(
        \\            value=Compare(
        \\                left=Constant(value=1),
        \\                ops=[
        \\                    Lt(),
        \\                    Gt()
        \\                ],
        \\                comparators=[
        \\                    Constant(value=2),
        \\                    Constant(value=3)
        \\                ]
        \\            )
        \\        ),
        \\        Expr(
        \\            value=Compare(
        \\                left=Constant(value=5),
        \\                ops=[
        \\                    Gt(),
        \\                    Eq(),
        \\                    Lt()
        \\                ],
        \\                comparators=[
        \\                    Constant(value=4),
        \\                    Constant(value=3),
        \\                    Constant(value=4)
        \\                ]
        \\            )
        \\        ),
        \\        Expr(
        \\            value=Compare(
        \\                left=Constant(value=5),
        \\                ops=[
        \\                    Lt(),
        \\                    NotEq(),
        \\                    Gt()
        \\                ],
        \\                comparators=[
        \\                    Constant(value=4),
        \\                    Constant(value=3),
        \\                    Constant(value=4)
        \\                ]
        \\            )
        \\        ),
        \\        Expr(
        \\            value=Compare(
        \\                left=BinOp(
        \\                    left=Constant(value=3),
        \\                    op=Add(),
        \\                    right=BinOp(
        \\                        left=Constant(value=4),
        \\                        op=Mult(),
        \\                        right=Constant(value=5)
        \\                    )
        \\                ),
        \\                ops=[
        \\                    Eq()
        \\                ],
        \\                comparators=[
        \\                    BinOp(
        \\                        left=BinOp(
        \\                            left=Constant(value=3),
        \\                            op=Mult(),
        \\                            right=Constant(value=1)
        \\                        ),
        \\                        op=Add(),
        \\                        right=BinOp(
        \\                            left=Constant(value=4),
        \\                            op=Mult(),
        \\                            right=Constant(value=5)
        \\                        )
        \\                    )
        \\                ]
        \\            )
        \\        ),
        \\    ]
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
        \\    body=[
        \\        Expr(
        \\            value=BinOp(
        \\                left=BinOp(
        \\                    left=Constant(value=1),
        \\                    op=Add(),
        \\                    right=BinOp(
        \\                        left=Constant(value=2),
        \\                        op=Add(),
        \\                        right=Constant(value=3)
        \\                    )
        \\                ),
        \\                op=Add(),
        \\                right=Constant(value=4)
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=BinOp(
        \\                    left=Constant(value=5),
        \\                    op=Add(),
        \\                    right=Constant(value=5)
        \\                ),
        \\                op=Mult(),
        \\                right=Constant(value=2)
        \\            )
        \\        ),
        \\        Expr(
        \\            value=BinOp(
        \\                left=Constant(value=2),
        \\                op=Div(),
        \\                right=BinOp(
        \\                    left=Constant(value=5),
        \\                    op=Add(),
        \\                    right=Constant(value=5)
        \\                )
        \\            )
        \\        ),
        \\        Expr(
        \\            value=UnaryOp(
        \\                USub(),
        \\                operand=BinOp(
        \\                    left=Constant(value=5),
        \\                    op=Add(),
        \\                    right=Constant(value=5)
        \\                )
        \\            )
        \\        ),
        \\    ]
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
        \\    body=[
        \\        If(
        \\            test=Constant(value=True),
        \\            body=[
        \\                Expr(
        \\                    value=Constant(value=False)
        \\                )
        \\            ],
        \\            orelse=[
        \\
        \\            ]
        \\        ),
        \\        If(
        \\            test=Compare(
        \\                left=Name(value=a),
        \\                ops=[
        \\                    Gt()
        \\                ],
        \\                comparators=[
        \\                    Constant(value=5)
        \\                ]
        \\            ),
        \\            body=[
        \\                Assign(
        \\                    target=Name(value=foo),
        \\                    value=Constant(value=234)
        \\                ),
        \\                Assign(
        \\                    target=Name(value=baz),
        \\                    value=Constant(value=123)
        \\                )
        \\            ],
        \\            orelse=[
        \\
        \\            ]
        \\        ),
        \\    ]
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
        \\    body=[
        \\        If(
        \\            test=Constant(value=True),
        \\            body=[
        \\                Expr(
        \\                    value=Constant(value=False)
        \\                )
        \\            ],
        \\            orelse=[
        \\                Expr(
        \\                    value=Constant(value=True)
        \\                )
        \\            ]
        \\        ),
        \\    ]
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
        \\    body=[
        \\        If(
        \\            test=Constant(value=True),
        \\            body=[
        \\                Expr(
        \\                    value=Constant(value=1)
        \\                )
        \\            ],
        \\            orelse=[
        \\                If(
        \\                    test=Constant(value=False),
        \\                    body=[
        \\                        Expr(
        \\                            value=Constant(value=2)
        \\                        )
        \\                    ],
        \\                    orelse=[
        \\                        Expr(
        \\                            value=Constant(value=3)
        \\                        )
        \\                    ]
        \\                )
        \\            ]
        \\        ),
        \\    ]
        \\)
    ));
}

test "can handle if expressions" {
    const input: []const u8 =
        \\x = 5 if True else 4
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\Module(
        \\    body=[
        \\        Assign(
        \\            target=Name(value=x),
        \\            value=IfExp(
        \\                test=Constant(value=True),
        \\                body=Constant(value=5),
        \\                orelse=Constant(value=4)
        \\            )
        \\        ),
        \\    ]
        \\)
    ));
}

test "can handle function definitions" {
    const input: []const u8 =
        \\def my_function():
        \\  return True
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\Module(
        \\    body=[
        \\        FunctionDef(
        \\            name=my_function,
        \\            args=arguments(
        \\                args=[
        \\
        \\                ],
        \\                vararg=None,
        \\                kwonlyargs=[
        \\                ],
        \\                kw_defaults=[
        \\                ],
        \\                kwarg=None,
        \\                defaults=[
        \\                ],
        \\            ]
        \\        ),
        \\        body=[
        \\            Return(value=Constant(value=True))
        \\        ]
        \\    ),
        \\]
        \\)
    ));
}

test "can handle function arguments" {
    const input: []const u8 =
        \\def my_function(x, y):
        \\  return x + y
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\Module(
        \\    body=[
        \\        FunctionDef(
        \\            name=my_function,
        \\            args=arguments(
        \\                args=[
        \\                    arg(arg="x"),
        \\                    arg(arg="y")
        \\                ],
        \\                vararg=None,
        \\                kwonlyargs=[
        \\                ],
        \\                kw_defaults=[
        \\                ],
        \\                kwarg=None,
        \\                defaults=[
        \\                ],
        \\            ]
        \\        ),
        \\        body=[
        \\            Return(value=BinOp(
        \\                left=Name(value=x),
        \\                op=Add(),
        \\                right=Name(value=y)
        \\            ))
        \\        ]
        \\    ),
        \\]
        \\)
    ));
}

test "can handle call expression" {
    const input: []const u8 =
        \\my_func(x, y)
    ;

    const module = try parseModule(input);
    defer module.deinit();

    try checkParserOutput(module, snap(@src(),
        \\
    ));
}
