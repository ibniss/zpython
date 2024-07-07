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

    // Reserved keywords/identifiers
    DEF,
    IDENT, // user-defined

    // illegal
    EOF,
    ILLEGAL,
};

fn isLetter(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

// lookup for reserved keywords
fn lookupIdent(ident: []const u8) TokenType {
    if (std.mem.eql(u8, ident, "def")) {
        return .DEF;
    }

    return .IDENT;
}

const Token = struct {
    // ADD LATER
    // filename: []const u8,
    // line: isize,
    // column: isize,
    type: TokenType,
    literal: []const u8,

    // Override default formatting, see std.fmt.format for signature

    pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("lexer.Token {{ .literal = \"{s}\", .type = \"{s}\" }}", .{ self.literal, @tagName(self.type) });
    }
    //
    // pub fn debug(self: *const Token) void {
    //     std.debug.print("lexer.Token {{ .literal = \"{s}\", .type = \"{s}\" }}\n", .{ self.literal, @tagName(self.type) });
    // }
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
    indentStack: std.ArrayList(isize),
    done: bool = false,

    fn init(alloc: std.mem.Allocator, input: []const u8) !Lexer {
        var lex: Lexer = .{ .input = input, .indentStack = std.ArrayList(isize).init(alloc) };
        // start with indent 0
        try lex.indentStack.insert(0, 0);
        lex.readChar();
        return lex;
    }

    fn deinit(self: *Lexer) void {
        self.indentStack.deinit();
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

    fn skipWhitespace(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n') {
            self.readChar();
        }
    }

    fn readNumber(self: *Lexer) []const u8 {
        const pos = self.position;

        while (std.ascii.isDigit(self.ch)) {
            self.readChar();
        }

        return self.input[@intCast(pos)..@intCast(self.position)];
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const pos = self.position;

        while (isLetter(self.ch)) {
            self.readChar();
        }

        return self.input[@intCast(pos)..@intCast(self.position)];
    }

    fn nextToken(self: *Lexer) ?Token {
        if (self.done) {
            return null;
        }

        self.skipWhitespace();

        const chSlice = if (self.readPosition <= self.input.len) self.input[@intCast(self.position)..@intCast(self.readPosition)] else "";

        // self.ch as a slice
        std.debug.print("self.ch: \"{c}\"\n", .{self.ch});
        std.debug.print("currentChSlice: \"{s}\"\n", .{chSlice});

        const token: Token = switch (self.ch) {
            '+' => .{ .type = .PLUS, .literal = chSlice },
            '=' => .{ .type = .ASSIGN, .literal = chSlice },
            ',' => .{ .type = .COMMA, .literal = chSlice },
            '(' => .{ .type = .LPAREN, .literal = chSlice },
            ')' => .{ .type = .RPAREN, .literal = chSlice },
            '{' => .{ .type = .LBRACE, .literal = chSlice },
            '}' => .{ .type = .RBRACE, .literal = chSlice },
            0 => {
                self.done = true;
                return .{ .type = .EOF, .literal = chSlice };
            },
            else => {
                if (isLetter(self.ch)) {
                    const literal = self.readIdentifier();
                    const typ = lookupIdent(literal);

                    return .{ .type = typ, .literal = literal };
                } else if (std.ascii.isDigit(self.ch)) {
                    return .{ .type = .NUMBER, .literal = self.readNumber() };
                }

                self.done = true;
                return .{ .type = .ILLEGAL, .literal = chSlice };
            },
        };

        self.readChar(); // read next one
        return token;
    }
};

const t = std.testing;

const Snap = @import("./snaptest.zig").Snap;
const snap = Snap.snap;

// util to perform snapshot testing on a given input
fn checkLexerOutput(input: []const u8, want: Snap) !void {
    var lexer = try Lexer.init(t.allocator, input);
    defer lexer.deinit();

    var tokens = std.ArrayList(Token).init(t.allocator);
    defer tokens.deinit();

    while (lexer.nextToken()) |tok| {
        try tokens.append(tok);
    }

    // build a string by printing into the buffer
    var strings = std.ArrayList([]u8).init(t.allocator);
    defer strings.deinit();
    var stringsLen: usize = 0;
    for (tokens.items) |tok| {
        const printed = try std.fmt.allocPrint(t.allocator, "{any}\n", .{tok});

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

test "next token basic" {
    const input = "=+,(){}";
    try checkLexerOutput(input, snap(@src(),
        \\lexer.Token { .literal = "=", .type = "ASSIGN" }
        \\lexer.Token { .literal = "+", .type = "PLUS" }
        \\lexer.Token { .literal = ",", .type = "COMMA" }
        \\lexer.Token { .literal = "(", .type = "LPAREN" }
        \\lexer.Token { .literal = ")", .type = "RPAREN" }
        \\lexer.Token { .literal = "{", .type = "LBRACE" }
        \\lexer.Token { .literal = "}", .type = "RBRACE" }
        \\lexer.Token { .literal = "", .type = "EOF" }
        \\
    ));
}

test "next token ident" {
    const input =
        \\five = 5
        \\ten = 10
        \\result = add(five, ten)
    ;

    try checkLexerOutput(input, snap(@src(),
        \\lexer.Token { .literal = "five", .type = "IDENT" }
        \\lexer.Token { .literal = "=", .type = "ASSIGN" }
        \\lexer.Token { .literal = "5", .type = "NUMBER" }
        \\lexer.Token { .literal = "ten", .type = "IDENT" }
        \\lexer.Token { .literal = "=", .type = "ASSIGN" }
        \\lexer.Token { .literal = "10", .type = "NUMBER" }
        \\lexer.Token { .literal = "result", .type = "IDENT" }
        \\lexer.Token { .literal = "=", .type = "ASSIGN" }
        \\lexer.Token { .literal = "add", .type = "IDENT" }
        \\lexer.Token { .literal = "(", .type = "LPAREN" }
        \\lexer.Token { .literal = "five", .type = "IDENT" }
        \\lexer.Token { .literal = ",", .type = "COMMA" }
        \\lexer.Token { .literal = "ten", .type = "IDENT" }
        \\lexer.Token { .literal = ")", .type = "RPAREN" }
        \\lexer.Token { .literal = "", .type = "EOF" }
        \\
    ));
}
