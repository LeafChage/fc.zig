const std = @import("std");

pub const StatusFlag = packed struct {
    // 7  bit  0
    // ---- ----
    // CZID BRVN
    negative: bool,
    overflow: bool,
    reserved: bool,
    break_cmd: bool,
    decimal: bool,
    interrupt: bool,
    zero: bool,
    carry: bool,

    const Self = @This();
    pub fn init() Self {
        return StatusFlag{
            .negative = false,
            .overflow = false,
            .reserved = true,
            .break_cmd = false,
            .decimal = false,
            .interrupt = false,
            .zero = false,
            .carry = false,
        };
    }

    pub fn from(b: u8) Self {
        var flag: Self = @bitCast(b);
        flag.reserved = true;
        return flag;
    }

    pub fn as_u8(self: Self) u8 {
        return @bitCast(self);
    }

    pub fn format(
        status: StatusFlag,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        return writer.print("n: {}, o: {}, r: {}, b: {}, d: {}, i: {}, z: {}, c:{}", .{
            @as(u8, if (status.negative) 1 else 0),
            @as(u8, if (status.overflow) 1 else 0),
            @as(u8, if (status.reserved) 1 else 0),
            @as(u8, if (status.break_cmd) 1 else 0),
            @as(u8, if (status.decimal) 1 else 0),
            @as(u8, if (status.interrupt) 1 else 0),
            @as(u8, if (status.zero) 1 else 0),
            @as(u8, if (status.carry) 1 else 0),
        });
    }
};

test "status_flag as u8" {
    var flag = StatusFlag.init();
    flag.overflow = true;
    try std.testing.expectEqual(0b0000_0110, flag.as_u8());
}

test "status_flag from" {
    const flag = StatusFlag.from(0b0001_0010);
    try std.testing.expect(!flag.negative);
    try std.testing.expect(flag.overflow);
    try std.testing.expect(flag.reserved);
    try std.testing.expect(!flag.break_cmd);
    try std.testing.expect(flag.decimal);
    try std.testing.expect(!flag.interrupt);
    try std.testing.expect(!flag.zero);
    try std.testing.expect(!flag.carry);
}
