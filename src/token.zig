const std = @import("std");

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

    // Reserved keywords/identifiers TODO: these aren't different tokens in Python, just identifiers
    DEF,
    IF,
    ELSE,
    RETURN,
    TRUE,
    FALSE,

    NAME, // user-defined

    // Indentation
    INDENT,
    DEDENT,

    // Logical new line start
    NEWLINE,

    // illegal
    EOF,
    ILLEGAL,
};
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
