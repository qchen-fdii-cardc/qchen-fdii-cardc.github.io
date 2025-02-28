const std = @import("std");
const WriteError = std.posix.WriteError;
pub fn main() !void {
    // std.debug.print("Hello, Wolrd!\n", .{});
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Hello, World!\n", .{});
}
