const std = @import("std");
const assert = @import("./assert.zig").assert;

const TokenType = enum {
    // Literals
    NUMBER,

    // Operators
    PLUS,
    ASSIGN,

    // Delimiters
    COMMA,

    // Brackets
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    EOF,
};

const Token = struct {
    // ADD LATER
    // filename: []const u8,
    // line: isize,
    // column: isize,
    type: TokenType,
    literal: u8,

    // Override default formatting, see std.fmt.format for signature
    pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("lexer.Token {{ .literal = \"{c}\", .type = \"{s}\" }}", .{ self.literal, @tagName(self.type) });
    }
};

const Lexer = struct {
    // Whole input string
    input: []const u8,
    // Current position in input - points to current char
    position: isize = 0,
    // Current reading position in input - after current char
    readPosition: isize = 0,
    // current char under examination
    ch: u8 = 0,

    fn init(input: []const u8) Lexer {
        var lex: Lexer = .{ .input = input };
        lex.readChar();
        return lex;
    }

    fn readChar(self: *Lexer) void {
        if (self.readPosition >= self.input.len) {
            self.ch = 0;
        } else {
            // TODO: this might not work for utf-8 and only ascii?
            self.ch = self.input[@intCast(self.readPosition)];
        }

        self.position = self.readPosition;
        self.readPosition += 1;
    }

    fn nextToken(self: *Lexer) Token {
        // std.debug.print("self.ch: {c}\n", .{self.ch});
        const tokenType: TokenType = switch (self.ch) {
            '+' => .PLUS,
            '=' => .ASSIGN,
            ',' => .COMMA,
            '(' => .LPAREN,
            ')' => .RPAREN,
            '{' => .LBRACE,
            '}' => .RBRACE,
            0 => .EOF,
            else => unreachable,
        };

        const token = .{ .type = tokenType, .literal = self.ch };
        self.readChar(); // read next one
        return token;
    }
};

test "next token" {
    const t = std.testing;

    const input = "=+,(){}";

    const expectedTokens = [_]struct { TokenType, u8 }{ .{ .ASSIGN, '=' }, .{ .PLUS, '+' }, .{ .COMMA, ',' }, .{ .LPAREN, '(' }, .{ .RPAREN, ')' }, .{ .LBRACE, '{' }, .{ .RBRACE, '}' }, .{ .EOF, 0 } };

    var lexer = Lexer.init(input);

    for (0..expectedTokens.len) |i| {
        const token = lexer.nextToken();
        const expectedToken = expectedTokens[i][0];
        const expectedLiteral = expectedTokens[i][1];

        try t.expectEqual(expectedToken, token.type);
        try t.expectEqual(expectedLiteral, token.literal);
    }
}
