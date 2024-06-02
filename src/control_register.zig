const testing = @import("std").testing;

pub const ControlRegister = packed struct(u8) {
    // 7  bit  0
    // ---- ----
    // VPHB SINN
    // |||| ||||
    // |||| ||++- Base nametable address
    // |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
    // |||| |+--- VRAM address increment per CPU read/write of PPUDATA
    // |||| |     (0: add 1, going across; 1: add 32, going down)
    // |||| +---- Sprite pattern table address for 8x8 sprites
    // ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
    // |||+------ Background pattern table address (0: $0000; 1: $1000)
    // ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels)
    // |+-------- PPU master/slave select
    // |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
    // +--------- Generate an NMI at the start of the
    //            vertical blanking interval (0: off; 1: on)
    nametable1: bool,
    nametable2: bool,
    vram_add_increment: bool,
    sprite_pattern_addr: bool,
    backround_pattern_addr: bool,
    sprite_size: bool,
    master_slave_select: bool,
    generate_nmi: bool,

    const Self = @This();
    pub fn as_u8(self: Self) u8 {
        return @ptrCast(self);
    }

    pub fn vram_addr_increment(self: Self) u8 {
        return if (!self.vram_add_increment) 1 else 32;
    }

    pub fn update(self: *Self, data: u8) void {
        self = @bitCast(data);
    }
};
