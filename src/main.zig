const std = @import("std");
const l = @import("./lexer.zig");
const Lexer = l.Lexer;
const Token = l.Token;
const TokenType = l.TokenType;

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const reader = std.io.getStdIn().reader();

    while (true) {
        std.debug.print(">> ", .{});
        const scanned = try reader.readUntilDelimiterAlloc(alloc, '\n', 1024);
        defer alloc.free(scanned);

        var lexer = try Lexer.init(alloc, scanned);
        defer lexer.deinit();

        while (try lexer.nextToken()) |tok| {
            if (tok.type == .EOF) {
                break;
            }
            std.debug.print("{any}\n", .{tok});
        }
    }
}
