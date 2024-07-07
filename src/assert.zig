const std = @import("std");

pub fn assert(truthy: bool, msg: []const u8) void {
    if (truthy) {
        return;
    }

    @panic(msg);
}
