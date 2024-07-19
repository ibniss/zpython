const assert = @import("./assert.zig").assert;
const _p = @import("./parser.zig");
const Parser = _p.Parser;
const _t = @import("./token.zig");
const TokenType = _t.TokenType;
const Token = _t.Token;
const ast = @import("./ast.zig");
const std = @import("std");

// Represents information about a Pratt token
pub const TokenInfo = struct {
    const Self = @This();
    prefix: ?*const fn (info: *const Self, parser: *Parser, token: Token) ast.Expr = null,
    infix: ?*const fn (info: *const Self, parser: *Parser, left: *ast.Expr, token: Token) ast.Expr = null,
    // left-binding power ('left arg available', i.e. infix/postfix)
    lbp: u8,
    // null-binding power (i.e. prefix)
    nbp: u8,
};

fn parseName(_: *const TokenInfo, _: *Parser, token: Token) ast.Expr {
    return .{ .name = .{ .token = token, .value = token.literal } };
}

fn parseNumber(_: *const TokenInfo, parser: *Parser, token: Token) ast.Expr {
    _ = parser;
    return .{ .constant = .{ .token = token, .value = token.literal } };
}

fn parseUnaryOp(ti: *const TokenInfo, parser: *Parser, token: Token) ast.Expr {
    const prefix = token;
    return .{
        .unary = .{
            .token = prefix,
            .op = ast.UnaryOperator.fromToken(prefix),
            .operand = parser.parseExpression(ti.nbp) orelse @panic("Could not parse operand"),
        },
    };
}

fn parseBinaryOp(ti: *const TokenInfo, parser: *Parser, left: *ast.Expr, token: Token) ast.Expr {
    const right = parser.parseExpression(ti.lbp) orelse @panic("Could not parse right of binary op");

    // TODO: if left is BoolOp, join as right

    // Important: create new pointer rather than reuse the same one as passed
    // deallocation is handled by the arena
    const newLeft = parser.arena.allocator().create(ast.Expr) catch unreachable;
    newLeft.* = left.*;

    // it's a binary op
    if (ast.BinaryOperator.fromToken(token)) |bop| {
        return .{
            .binary = .{
                .left = newLeft,
                .op = bop,
                .right = right,
                .token = token,
            },
        };
    }

    // it's a comparison op
    if (ast.ComparisonOperator.fromToken(token)) |cop| {
        switch (left.*) {
            // if the left expr is also a comparison, instead append
            .compare => |*left_compare| {
                std.debug.print("Left if compare {s}\n", .{left_compare});
                left_compare.ops.append(cop) catch unreachable;
                left_compare.comparators.append(right) catch unreachable;
                return left.*;
            },
            else => {
                std.debug.print("left is not compare {s}\n", .{left});
                var comparators = std.ArrayList(*const ast.Expr).init(parser.arena.allocator());
                comparators.append(right) catch unreachable;

                var operators = std.ArrayList(ast.ComparisonOperator).init(parser.arena.allocator());
                operators.append(cop) catch unreachable;

                return .{
                    .compare = .{
                        .left = newLeft,
                        .ops = operators,
                        .comparators = comparators,
                        .token = token,
                    },
                };
            },
        }
    }

    std.debug.panic("Unsupported infix operator: {any}\n", .{token});
}

// Precedences taken from pycopy-lib parser, mostly quantifying Python's expressions docs precedence order
pub const token_infos = std.StaticStringMap(TokenInfo).initComptime(.{
    .{ @tagName(TokenType.NEWLINE), .{ .lbp = 0, .nbp = 0 } },
    .{ @tagName(TokenType.NAME), .{ .lbp = 0, .nbp = 0, .prefix = parseName } },
    .{ @tagName(TokenType.NUMBER), .{ .lbp = 0, .nbp = 0, .prefix = parseNumber } },
    .{ @tagName(TokenType.TILDE), .{ .lbp = 0, .nbp = 130, .prefix = parseUnaryOp } },
    .{ @tagName(TokenType.EQUAL), .{
        .lbp = 0,
        .nbp = 0,
    } },
    .{ @tagName(TokenType.COMMA), .{
        .lbp = 5,
        .nbp = 0,
    } },
    .{ @tagName(TokenType.LESS), .{
        .lbp = 60,
        .nbp = 0,
        .infix = parseBinaryOp,
    } },
    .{ @tagName(TokenType.GREATER), .{
        .lbp = 60,
        .nbp = 0,
        .infix = parseBinaryOp,
    } },
    .{ @tagName(TokenType.EQEQUAL), .{
        .lbp = 60,
        .nbp = 0,
        .infix = parseBinaryOp,
    } },
    .{ @tagName(TokenType.NOTEQUAL), .{
        .lbp = 60,
        .nbp = 0,
        .infix = parseBinaryOp,
    } },
    .{ @tagName(TokenType.PLUS), .{
        .lbp = 110,
        .nbp = 130,
        .infix = parseBinaryOp,
    } },
    .{ @tagName(TokenType.MINUS), .{
        .lbp = 110,
        .nbp = 130,
        .prefix = parseUnaryOp,
        .infix = parseBinaryOp,
    } },
    .{ @tagName(TokenType.STAR), .{
        .lbp = 120,
        .nbp = 159,
        .infix = parseBinaryOp,
    } },
    .{ @tagName(TokenType.SLASH), .{
        .lbp = 120,
        .nbp = 0,
        .infix = parseBinaryOp,
    } },
    .{ @tagName(TokenType.PERCENT), .{
        .lbp = 120,
        .nbp = 0,
        .infix = parseBinaryOp,
    } },
    .{ "//", .{
        .lbp = 120,
        .nbp = 0,
        .infix = parseBinaryOp,
    } },
    .{ @tagName(TokenType.STRING), .{
        .lbp = 200,
        .nbp = 0,
    } },
});
