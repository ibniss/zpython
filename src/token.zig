const std = @import("std");
const Parser = @import("./parser.zig").Parser;
const ast = @import("./ast.zig");
const assert = @import("./assert.zig").assert;

// Refer to https://docs.python.org/3/library/token.html
pub const TokenType = enum {
    // Literals
    NUMBER,
    STRING,

    // Operators
    PLUS,
    EQUAL,
    MINUS,
    EXCLAMATION,
    STAR,
    SLASH,
    TILDE,
    DOUBLESLASH,
    PERCENT,
    DOUBLESTAR,

    // Comparison
    LESS,
    GREATER,
    EQEQUAL,
    NOTEQUAL,

    // Delimiters
    COMMA,
    COLON,

    // Brackets
    LPAR,
    RPAR,
    LBRACE,
    RBRACE,

    NAME, // Any identifier, whether user defined or not

    // Indentation
    INDENT,
    DEDENT,

    // Logical new line start
    NEWLINE,
    // TODO: NL - when a logical line is continued over physical lines

    SEMI,
    ENDMARKER,
    ILLEGAL,
};

// Represents an instance of a token
pub const Token = struct {
    // ADD LATER
    // filename: []const u8,
    line: usize,
    column: usize,
    type: TokenType,
    literal: []const u8, // should this be optional?

    // Override default formatting, see std.fmt.format for signature
    pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("lexer.Token {{ .literal = \"{s}\", .type = \"{s}\", .line = {}, .column = {} }}", .{ self.literal, @tagName(self.type), self.line, self.column });
    }
};
