const BE = @import("./big_endian.zig").BE;

pub const AddrRegister = struct {
    value: BE,
    hi_ptr: bool,

    const Self = @This();
    pub fn init() Self {
        return AddrRegister{
            .value = BE.init(0, 0),
            .ht_ptr = true,
        };
    }

    pub fn update(self: *Self, data: u8) void {
        if (self.hi_ptr) {
            self.value.upper = data;
        } else {
            self.value.lower = data;
        }

        if (self.value.get() > 0x3FFF) {
            // mirror down addr above 0x3fff
            self.value.set(self.value.get() & 0x3FFF);
        }

        self.hi_ptr = !self.hi_ptr;
    }

    pub fn increment(self: *Self, inc: u8) void {
        const lower = self.value.lower;

        self.value.lower +%= inc;
        if (lower > self.value.lower) {
            self.value.upper +%= 1;
        }

        if (self.value.get() > 0x3FFF) {
            // mirror down addr above 0x3fff
            self.value.set(self.value.get() & 0x3FFF);
        }
    }

    pub fn reset_latch(self: *Self) void {
        self.hi_ptr = true;
    }

    pub fn get(self: Self) u16 {
        return self.value.get();
    }
};
