pub const StatusFlag = struct {
    negative: bool,
    overflow: bool,
    reserved: bool,
    break_cmd: bool,
    decimal: bool,
    interrupt: bool,
    zero: bool,
    carry: bool,

    pub const Mask = struct {
        const negative = 0b1000_0000;
        const overflow = 0b0100_0000;
        const reserved = 0b0010_0000;
        const break_cmd = 0b0001_0000;
        const decimal = 0b0000_1000;
        const interrupt = 0b0000_0100;
        const zero = 0b0000_0010;
        const carry = 0b0000_0001;
    };

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
        return StatusFlag{
            .negative = b | Mask.negative == Mask.negative,
            .overflow = b | Mask.overflow == Mask.overflow,
            .reserved = true,
            .break_cmd = b | Mask.break_cmd == Mask.break_cmd,
            .decimal = b | Mask.decimal == Mask.decimal,
            .interrupt = b | Mask.interrupt == Mask.interrupt,
            .zero = b | Mask.zero == Mask.zero,
            .carry = b | Mask.carry == Mask.carry,
        };
    }

    pub fn raw(self: Self) u8 {
        return @as(u8, if (self.negative) Mask.negative else 0) |
            @as(u8, if (self.overflow) Mask.overflow else 0) |
            @as(u8, if (self.reserved) Mask.reserved else 0) |
            @as(u8, if (self.break_cmd) Mask.break_cmd else 0) |
            @as(u8, if (self.decimal) Mask.decimal else 0) |
            @as(u8, if (self.interrupt) Mask.interrupt else 0) |
            @as(u8, if (self.zero) Mask.zero else 0) |
            @as(u8, if (self.carry) Mask.carry else 0);
    }
};
