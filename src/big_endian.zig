const testing = @import("std").testing;

// BigEndian
pub const BE = struct {
    upper: u8,
    lower: u8,

    const Self = @This();

    pub fn init() Self {
        return BE{ .upper = 0, .lower = 0 };
    }

    pub fn set(self: *Self, data: u16) void {
        self.upper = @truncate(data >> 8);
        self.lower = @truncate(data);
    }

    pub fn get(self: Self) u16 {
        return @as(u16, self.upper) << 8 | @as(u16, self.lower);
    }
};

test "BE set" {
    var be = BE.init();
    _ = be.set(0x1234);
    try testing.expectEqual(0x12, be.upper);
    try testing.expectEqual(0x34, be.lower);
}

test "BE get" {
    var be = BE.init();
    be.upper = 0x12;
    be.lower = 0x34;
    try testing.expectEqual(0x1234, be.get());
}
