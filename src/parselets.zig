const assert = @import("./assert.zig").assert;
const _p = @import("./parser.zig");
const Parser = _p.Parser;
const _t = @import("./token.zig");
const TokenType = _t.TokenType;
const ast = @import("./ast.zig");
const std = @import("std");

// Represents information about a Pratt token
pub const TokenInfo = struct {
    prefix: ?*const fn (*Parser) ?ast.Expr = null,
    infix: ?*const fn (*Parser) ?ast.Expr = null,
    // left-binding power
    lbp: u8,
};

fn parseName(parser: *Parser) ?ast.Expr {
    return .{ .name = .{ .token = parser.cur_token, .value = parser.cur_token.literal } };
}

fn parseNumber(parser: *Parser) ?ast.Expr {
    return .{ .constant = .{ .token = parser.cur_token, .value = parser.cur_token.literal } };
}

fn parseUnaryOp(parser: *Parser) ?ast.Expr {
    const prefix = parser.cur_token;
    parser.nextToken();
    return .{
        .unary = .{
            .token = prefix,
            .op = ast.UnaryOperator.fromToken(prefix),
            .operand = parser.parseExpression(0) orelse @panic("Could not parse operand"),
        },
    };
}

// Precedences taken from pycopy-lib parser, mostly quantifying Python's expressions docs precedence order
pub const token_infos = std.StaticStringMap(TokenInfo).initComptime(.{
    .{ @tagName(TokenType.NAME), .{ .lbp = 0, .prefix = parseName } },
    .{ @tagName(TokenType.NUMBER), .{ .lbp = 0, .prefix = parseNumber } },
    .{ @tagName(TokenType.TILDE), .{ .lbp = 0, .prefix = parseUnaryOp } },
    .{ @tagName(TokenType.EQUAL), .{
        .lbp = 0,
    } },
    .{ @tagName(TokenType.COMMA), .{
        .lbp = 5,
    } },
    .{ @tagName(TokenType.LESS), .{
        .lbp = 60,
    } },
    .{ @tagName(TokenType.GREATER), .{
        .lbp = 60,
    } },
    .{ @tagName(TokenType.EQEQUAL), .{
        .lbp = 60,
    } },
    .{ @tagName(TokenType.PLUS), .{
        .lbp = 110,
    } },
    .{ @tagName(TokenType.MINUS), .{
        .lbp = 110,
        .prefix = parseUnaryOp,
    } },
    .{ @tagName(TokenType.STAR), .{
        .lbp = 120,
    } },
    .{ @tagName(TokenType.SLASH), .{
        .lbp = 120,
    } },
    .{ "//", .{
        .lbp = 120,
    } },
    .{ @tagName(TokenType.STRING), .{
        .lbp = 200,
    } },
});
