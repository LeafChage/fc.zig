const std = @import("std");
const Mirroring = @import("./mirroring.zig").Mirroring;
const AddrRegister = @import("./addr_register.zig").AddrRegister;
const ControlRegister = @import("./control_register.zig").ControlRegister;

pub const NesPPU = struct {
    chr_rom: []const u8,
    palette_table: [32]u8,
    vram: [2048]u8,
    oam_data: [2048]u8,
    mirroring: Mirroring,
    addr: AddrRegister,
    ctrl: ControlRegister,
    internal_data_buf: u8,

    const Self = @This();

    pub fn init(chr_rom: []const u8, mirroring: Mirroring) Self {
        return NesPPU{
            .chr_rom = chr_rom,
            .mirroring = mirroring,
            .palette_table = [_]u8{0} ** 32,
            .vram = [_]u8{0} ** 2048,
            .oam_data = [_]u8{0} ** 2048,
        };
    }

    pub fn write_to_ppu_addr(self: *Self, value: u8) void {
        self.addr.update(value);
    }

    pub fn write_to_ctrl(self: *Self, value: u8) void {
        self.ctrl.update(value);
    }

    fn increment_vram_addr(self: *Self) void {
        self.addr.increment(self.ctrl.vram_add_increment());
    }

    // Horizontal:
    //   [ A ] [ a ]
    //   [ B ] [ b ]

    // Vertical:
    //   [ A ] [ B ]
    //   [ a ] [ b ]
    pub fn mirror_vram_addr(self: Self, addr: u16) u16 {
        const mirrored_vram = addr & 0b10111111111111;
        const vram_index = mirrored_vram - 0x2000;
        const name_table = vram_index / 0x400;
        return switch (.{ self.mirroring, name_table }) {
            .{ Mirroring.Vertical, 2 }, .{ Mirroring.Vertical, 3 } => vram_index - 0x800,
            .{ Mirroring.Horizontal, 1 }, .{ Mirroring.Horizontal, 2 } => vram_index - 0x400,
            .{ Mirroring.Horizontal, 3 } => vram_index - 0x800,
            else => vram_index,
        };
    }

    pub fn read_addr(self: *Self) u8 {
        const addr = self.addr.get();
        self.increment_vram_addr();

        return switch (addr) {
            0...0x1FFF => {
                const result = self.internal_data_buf;
                self.internal_data_buf = self.chr_rom[addr];
                return result;
            },
            0x2000...0x2FFF => {
                const result = self.internal_data_buf;
                self.internal_data_buf = self.vram[self.mirror_vram_addr(addr)];
                return result;
            },
            0x3000...0x3EFF => std.debug.panic("addr space 0x3000..0x3EFF is not expected to be used, requested = 0x{x}", .{addr}),
            0x3F00...0x3FFF => {
                self.palette_table[(addr - 0x3f00)];
            },
            else => std.debug.panic("unexpected access to mirrored space 0x{x}", .{addr}),
        };
    }

    pub fn write_data(self: *Self) u8 {
        _ = self;
        std.debug.panic("", .{});
    }
};
