const std = @import("std");
const builtin = @import("builtin");

pub fn main() !void {
    var it = try std.process.argsWithAllocator(std.heap.page_allocator);
    defer it.deinit();

    while (it.next()) |arg| {
        std.debug.print("|{s}|\n", .{arg});
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "aray allocation" {
    // array literal
    const arr = [_]i32{ 1, 2, 3 };
    try std.testing.expectEqual(@as(i32, 1), arr[0]);
    try std.testing.expectEqual(@as(i32, 2), arr[1]);
    try std.testing.expectEqual(@as(i32, 3), arr[2]);
}
