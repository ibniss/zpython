const std = @import("std");
const assert = @import("./assert.zig").assert;
const _tok = @import("./token.zig");
const TokenType = _tok.TokenType;
const Token = _tok.Token;

fn isLetter(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

pub const Lexer = struct {
    // Whole input string
    input: []const u8,
    // Current position in input - points to current char
    position: usize = 0,
    // Current column in line
    col: usize = 0,
    // Current line (row) number
    row: usize = 1,
    // Current reading position in input - after current char
    read_position: usize = 0,
    // current char under examination
    ch: u8 = 0,
    // indentation level stack
    indent_stack: std.ArrayList(usize),
    // stack of tokens; this is kept so that nextToken() can always return a singular character, as the lexer can sometimes output multiple tokens e.g. when moving out of nested blocks
    tokens_stack: std.ArrayList(Token),
    // Whether the lexer has reached the end
    done: bool = false,
    // Whether the lexer cannot proceed further
    errored: bool = false,

    pub fn init(alloc: std.mem.Allocator, input: []const u8) !Lexer {
        var lex: Lexer = .{ .input = input, .indent_stack = std.ArrayList(usize).init(alloc), .tokens_stack = std.ArrayList(Token).init(alloc) };
        // start with indent 0
        try lex.indent_stack.insert(0, 0);
        lex.readChar();
        return lex;
    }

    pub fn deinit(self: *const Lexer) void {
        self.indent_stack.deinit();
        self.tokens_stack.deinit();
    }

    fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
        self.col += 1;
    }

    // Can return INDENT/DEDENT tokens
    fn skipWhitespace(self: *Lexer) !void {
        var movedLine = false;
        var whitespaceStart: usize = 0;

        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n') {
            const isNewline = self.ch == '\n';
            self.readChar();

            if (isNewline) {
                // adjust position pointers
                self.row += 1;
                self.col = 1;

                // reset whitespace start
                movedLine = true;
                whitespaceStart = self.position;
            }
        }

        // if we moved line, keep track of indentation changes
        if (movedLine) {
            const currentIndent = self.indent_stack.getLast();
            const whitespaceCount: usize = self.position - whitespaceStart;

            // if indentation level is larger, it's pushed on the stack and INDENT is generated
            if (whitespaceCount > currentIndent) {
                try self.indent_stack.append(whitespaceCount);
                try self.tokens_stack.append(.{ .type = .INDENT, .literal = self.input[whitespaceStart..self.position], .line = self.row, .column = self.col });
            } else if (whitespaceCount < currentIndent) {
                // if it's smaller, it must be one of the numbers occuring on the stack
                const index = std.mem.indexOf(usize, self.indent_stack.items, &[_]usize{whitespaceCount});

                // if it's not, this is an error
                if (index == null) {
                    self.done = true;
                    self.errored = true;
                    try self.tokens_stack.append(.{ .type = .ILLEGAL, .literal = self.input[whitespaceStart..self.position], .line = self.row, .column = self.col });
                    return;
                }

                // all numbers on the stack that are larger are popped off, and for each number popped off a DEDENT token is generated
                var idx = self.indent_stack.items.len - 1;
                while (idx >= 0 and self.indent_stack.items[idx] > whitespaceCount) {
                    idx -= 1;
                    _ = self.indent_stack.pop();
                    try self.tokens_stack.append(.{ .type = .DEDENT, .literal = self.input[whitespaceStart..self.position], .line = self.row, .column = self.col });
                }
            }

            // start of new logical line
            // TODO: support implicit and explicit line joining
            try self.tokens_stack.append(.{ .type = .NEWLINE, .literal = "", .line = self.row, .column = self.col });
        }
    }

    fn readNumber(self: *Lexer) []const u8 {
        const pos = self.position;

        while (std.ascii.isDigit(self.ch)) {
            self.readChar();
        }

        return self.input[pos..self.position];
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const pos = self.position;

        while (isLetter(self.ch)) {
            self.readChar();
        }

        return self.input[pos..self.position];
    }

    fn peekChar(self: *Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        }

        return self.input[self.read_position];
    }

    pub fn nextToken(self: *Lexer) !?Token {
        if (self.errored) {
            return null;
        }

        // return item from the stack before proceeding
        if (self.tokens_stack.items.len > 0) {
            return self.tokens_stack.pop();
        }

        if (self.done) {
            return null;
        }

        // skip until next non-whitespace token
        try self.skipWhitespace();

        // return item from the stack before proceeding, as whitespace skip could've added items to the stack
        if (self.tokens_stack.items.len > 0) {
            return self.tokens_stack.pop();
        }

        const col = self.col;
        const row = self.row;

        // self.ch represented as a string slice
        const chSlice = if (self.read_position <= self.input.len) self.input[self.position..self.read_position] else "";

        // TODO: can this be improved?
        const tokenData: struct { type: TokenType, literal: []const u8 } = switch (self.ch) {
            '+' => .{ .type = .PLUS, .literal = chSlice },
            '-' => .{ .type = .MINUS, .literal = chSlice },
            '~' => .{ .type = .TILDE, .literal = chSlice },
            '%' => .{ .type = .PERCENT, .literal = chSlice },
            '!' => bang: {
                if (self.peekChar() == '=') {
                    self.readChar();
                    // extend the slice to include next char
                    break :bang .{ .type = .NOTEQUAL, .literal = self.input[self.position - 1 .. self.read_position] };
                }

                break :bang .{ .type = .EXCLAMATION, .literal = chSlice };
            },
            '*' => star: {
                if (self.peekChar() == '*') {
                    self.readChar();
                    break :star .{ .type = .DOUBLESTAR, .literal = self.input[self.position - 1 .. self.read_position] };
                }

                break :star .{ .type = .STAR, .literal = chSlice };
            },
            '/' => slash: {
                if (self.peekChar() == '/') {
                    self.readChar();
                    // extend the slice to include next char
                    break :slash .{ .type = .DOUBLESLASH, .literal = self.input[self.position - 1 .. self.read_position] };
                }

                break :slash .{ .type = .SLASH, .literal = chSlice };
            },
            '<' => .{ .type = .LESS, .literal = chSlice },
            '>' => .{ .type = .GREATER, .literal = chSlice },
            '=' => eq: {
                if (self.peekChar() == '=') {
                    self.readChar();
                    // extend the slice to include next char
                    break :eq .{ .type = .EQEQUAL, .literal = self.input[self.position - 1 .. self.read_position] };
                }

                break :eq .{ .type = .EQUAL, .literal = chSlice };
            },
            ',' => .{ .type = .COMMA, .literal = chSlice },
            ':' => .{ .type = .COLON, .literal = chSlice },
            '(' => .{ .type = .LPAR, .literal = chSlice },
            ')' => .{ .type = .RPAR, .literal = chSlice },
            '{' => .{ .type = .LBRACE, .literal = chSlice },
            '}' => .{ .type = .RBRACE, .literal = chSlice },
            ';' => .{ .type = .SEMI, .literal = chSlice },
            0 => {
                self.done = true;

                try self.tokens_stack.append(.{ .type = .ENDMARKER, .literal = chSlice, .column = col, .line = row });

                // Produce dedents for each leftover on indentation stack
                while (self.indent_stack.popOrNull()) |indent| {
                    if (indent > 0) {
                        try self.tokens_stack.append(.{ .type = .DEDENT, .literal = "", .line = self.row, .column = self.col });
                    }
                }

                // check if previous char was a newline if it wasn't, emit one anyway for simplicity
                if (self.input[self.position - 1] != '\n') {
                    try self.tokens_stack.append(.{ .type = .NEWLINE, .literal = "", .column = col, .line = row });
                }

                const foo = self.tokens_stack.pop();
                return foo;
                //return self.tokens_stack.pop();
            },
            else => {
                if (isLetter(self.ch)) {
                    const literal = self.readIdentifier();
                    return .{ .type = .NAME, .literal = literal, .column = col, .line = row };
                } else if (std.ascii.isDigit(self.ch)) {
                    return .{ .type = .NUMBER, .literal = self.readNumber(), .column = col, .line = row };
                }

                self.done = true;
                self.errored = true;
                return .{ .type = .ILLEGAL, .literal = chSlice, .column = col, .line = row };
            },
        };

        self.readChar(); // read next one
        return .{ .type = tokenData.type, .literal = tokenData.literal, .column = col, .line = row };
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

    while (try lexer.nextToken()) |tok| {
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

test "next token no indent" {
    const input =
        \\five = 5
        \\ten = 10
        \\result = add(five, ten)
        \\!-/*5
        \\5 < 10 > 5
    ;

    try checkLexerOutput(input, snap(@src(),
        \\lexer.Token { .literal = "five", .type = "NAME", .line = 1, .column = 1 }
        \\lexer.Token { .literal = "=", .type = "EQUAL", .line = 1, .column = 6 }
        \\lexer.Token { .literal = "5", .type = "NUMBER", .line = 1, .column = 8 }
        \\lexer.Token { .literal = "", .type = "NEWLINE", .line = 2, .column = 1 }
        \\lexer.Token { .literal = "ten", .type = "NAME", .line = 2, .column = 1 }
        \\lexer.Token { .literal = "=", .type = "EQUAL", .line = 2, .column = 5 }
        \\lexer.Token { .literal = "10", .type = "NUMBER", .line = 2, .column = 7 }
        \\lexer.Token { .literal = "", .type = "NEWLINE", .line = 3, .column = 1 }
        \\lexer.Token { .literal = "result", .type = "NAME", .line = 3, .column = 1 }
        \\lexer.Token { .literal = "=", .type = "EQUAL", .line = 3, .column = 8 }
        \\lexer.Token { .literal = "add", .type = "NAME", .line = 3, .column = 10 }
        \\lexer.Token { .literal = "(", .type = "LPAR", .line = 3, .column = 13 }
        \\lexer.Token { .literal = "five", .type = "NAME", .line = 3, .column = 14 }
        \\lexer.Token { .literal = ",", .type = "COMMA", .line = 3, .column = 18 }
        \\lexer.Token { .literal = "ten", .type = "NAME", .line = 3, .column = 20 }
        \\lexer.Token { .literal = ")", .type = "RPAR", .line = 3, .column = 23 }
        \\lexer.Token { .literal = "", .type = "NEWLINE", .line = 4, .column = 1 }
        \\lexer.Token { .literal = "!", .type = "EXCLAMATION", .line = 4, .column = 1 }
        \\lexer.Token { .literal = "-", .type = "MINUS", .line = 4, .column = 2 }
        \\lexer.Token { .literal = "/", .type = "SLASH", .line = 4, .column = 3 }
        \\lexer.Token { .literal = "*", .type = "STAR", .line = 4, .column = 4 }
        \\lexer.Token { .literal = "5", .type = "NUMBER", .line = 4, .column = 5 }
        \\lexer.Token { .literal = "", .type = "NEWLINE", .line = 5, .column = 1 }
        \\lexer.Token { .literal = "5", .type = "NUMBER", .line = 5, .column = 1 }
        \\lexer.Token { .literal = "<", .type = "LESS", .line = 5, .column = 3 }
        \\lexer.Token { .literal = "10", .type = "NUMBER", .line = 5, .column = 5 }
        \\lexer.Token { .literal = ">", .type = "GREATER", .line = 5, .column = 8 }
        \\lexer.Token { .literal = "5", .type = "NUMBER", .line = 5, .column = 10 }
        \\lexer.Token { .literal = "", .type = "NEWLINE", .line = 5, .column = 11 }
        \\lexer.Token { .literal = "", .type = "ENDMARKER", .line = 5, .column = 11 }
        \\
    ));
}

test "with indents" {
    const input =
        \\def add(x, y):
        \\  return x + y
        \\
        \\if 5 < 10:
        \\   return True
        \\else:
        \\   return False
    ;

    try checkLexerOutput(input, snap(@src(),
        \\lexer.Token { .literal = "def", .type = "NAME", .line = 1, .column = 1 }
        \\lexer.Token { .literal = "add", .type = "NAME", .line = 1, .column = 5 }
        \\lexer.Token { .literal = "(", .type = "LPAR", .line = 1, .column = 8 }
        \\lexer.Token { .literal = "x", .type = "NAME", .line = 1, .column = 9 }
        \\lexer.Token { .literal = ",", .type = "COMMA", .line = 1, .column = 10 }
        \\lexer.Token { .literal = "y", .type = "NAME", .line = 1, .column = 12 }
        \\lexer.Token { .literal = ")", .type = "RPAR", .line = 1, .column = 13 }
        \\lexer.Token { .literal = ":", .type = "COLON", .line = 1, .column = 14 }
        \\lexer.Token { .literal = "", .type = "NEWLINE", .line = 2, .column = 3 }
        \\lexer.Token { .literal = "  ", .type = "INDENT", .line = 2, .column = 3 }
        \\lexer.Token { .literal = "return", .type = "NAME", .line = 2, .column = 3 }
        \\lexer.Token { .literal = "x", .type = "NAME", .line = 2, .column = 10 }
        \\lexer.Token { .literal = "+", .type = "PLUS", .line = 2, .column = 12 }
        \\lexer.Token { .literal = "y", .type = "NAME", .line = 2, .column = 14 }
        \\lexer.Token { .literal = "", .type = "NEWLINE", .line = 4, .column = 1 }
        \\lexer.Token { .literal = "", .type = "DEDENT", .line = 4, .column = 1 }
        \\lexer.Token { .literal = "if", .type = "NAME", .line = 4, .column = 1 }
        \\lexer.Token { .literal = "5", .type = "NUMBER", .line = 4, .column = 4 }
        \\lexer.Token { .literal = "<", .type = "LESS", .line = 4, .column = 6 }
        \\lexer.Token { .literal = "10", .type = "NUMBER", .line = 4, .column = 8 }
        \\lexer.Token { .literal = ":", .type = "COLON", .line = 4, .column = 10 }
        \\lexer.Token { .literal = "", .type = "NEWLINE", .line = 5, .column = 4 }
        \\lexer.Token { .literal = "   ", .type = "INDENT", .line = 5, .column = 4 }
        \\lexer.Token { .literal = "return", .type = "NAME", .line = 5, .column = 4 }
        \\lexer.Token { .literal = "True", .type = "NAME", .line = 5, .column = 11 }
        \\lexer.Token { .literal = "", .type = "NEWLINE", .line = 6, .column = 1 }
        \\lexer.Token { .literal = "", .type = "DEDENT", .line = 6, .column = 1 }
        \\lexer.Token { .literal = "else", .type = "NAME", .line = 6, .column = 1 }
        \\lexer.Token { .literal = ":", .type = "COLON", .line = 6, .column = 5 }
        \\lexer.Token { .literal = "", .type = "NEWLINE", .line = 7, .column = 4 }
        \\lexer.Token { .literal = "   ", .type = "INDENT", .line = 7, .column = 4 }
        \\lexer.Token { .literal = "return", .type = "NAME", .line = 7, .column = 4 }
        \\lexer.Token { .literal = "False", .type = "NAME", .line = 7, .column = 11 }
        \\lexer.Token { .literal = "", .type = "NEWLINE", .line = 7, .column = 16 }
        \\lexer.Token { .literal = "", .type = "DEDENT", .line = 7, .column = 16 }
        \\lexer.Token { .literal = "", .type = "ENDMARKER", .line = 7, .column = 16 }
        \\
    ));
}

test "with two char tokens" {
    const input =
        \\10 == 10
        \\10 != 9
    ;

    try checkLexerOutput(input, snap(@src(),
        \\lexer.Token { .literal = "10", .type = "NUMBER", .line = 1, .column = 1 }
        \\lexer.Token { .literal = "==", .type = "EQEQUAL", .line = 1, .column = 4 }
        \\lexer.Token { .literal = "10", .type = "NUMBER", .line = 1, .column = 7 }
        \\lexer.Token { .literal = "", .type = "NEWLINE", .line = 2, .column = 1 }
        \\lexer.Token { .literal = "10", .type = "NUMBER", .line = 2, .column = 1 }
        \\lexer.Token { .literal = "!=", .type = "NOTEQUAL", .line = 2, .column = 4 }
        \\lexer.Token { .literal = "9", .type = "NUMBER", .line = 2, .column = 7 }
        \\lexer.Token { .literal = "", .type = "NEWLINE", .line = 2, .column = 8 }
        \\lexer.Token { .literal = "", .type = "ENDMARKER", .line = 2, .column = 8 }
        \\
    ));
}
