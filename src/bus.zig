const std = @import("std");
const Rom = @import("./rom.zig").Rom;
const NesPPU = @import("./ppu.zig").NesPPU;

pub const Mem = struct {
    ptr: *anyopaque,
    impl: *const Interface,

    const Self = @This();
    pub const Interface = struct {
        mem_read: *const fn (ctx: *anyopaque, addr: u16) u8,
        mem_read_u16: *const fn (ctx: *anyopaque, pos: u16) u16,
        mem_write: *const fn (ctx: *anyopaque, addr: u16, data: u8) void,
        mem_write_u16: *const fn (ctx: *anyopaque, pos: u16, data: u16) void,
    };

    pub fn mem_read(self: Self, addr: u16) u8 {
        return self.impl.mem_read(self.ptr, addr);
    }

    pub fn mem_read_u16(self: Self, pos: u16) u16 {
        return self.impl.mem_read_u16(self.ptr, pos);
    }

    pub fn mem_write(self: *Self, addr: u16, data: u8) void {
        self.impl.mem_write(self.ptr, addr, data);
    }

    pub fn mem_write_u16(self: *Self, pos: u16, data: u16) void {
        self.impl.mem_write_u16(self.ptr, pos, data);
    }
};

// CPU has 0x0000 - 0x2000 addressing space reserved for RAM space,
// but
// 0x0800 .. 0x1000
// 0x1000 .. 0x1800
// 0x1800 .. 0x2000
// these memories addressing spaces are mirror of 0x0000 .. 0x0800
// so we can get a same value from 0x0000, 0x0800, 0x1000, 0x1800
// we can write and read everywhere.
pub const Bus = struct {
    const RAM: u16 = 0x0000;
    const RAM_MIRROR_END: u16 = 0x1FFF;
    const PPU_REGISTERS: u16 = 0x2000;
    const PPU_REGISTERS_MIRROR_END: u16 = 0x3FFF;

    cpu_vram: [0x0800]u8,
    prg_rom: []const u8,
    ppu: NesPPU,

    const Self = @This();
    pub fn init(rom: Rom) Self {
        const ppu = NesPPU.init(rom, rom.screen_mirroring);
        return Self{
            .cpu_vram = [_]u8{0} ** 0x0800,
            .prg_rom = rom.program_rom,
            .ppu = ppu,
        };
    }

    fn read_program_rom(self: Self, addr: u16) u8 {
        var to = addr - 0x8000;
        if (self.rom.program_rom.len == 0x4000 and addr > 0x4000) {
            to %= 0x4000;
        }
        return self.rom.program_rom[to];
    }

    pub fn mem_read(ctx: *anyopaque, addr: u16) u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));
        switch (addr) {
            RAM...RAM_MIRROR_END => {
                const mirror_down_addr = addr & 0b0000_0111_1111_1111;
                return self.cpu_vram[mirror_down_addr];
            },
            0x2000, 0x2001, 0x2003, 0x2005, 0x2006, 0x4014 => {
                std.debug.panic("Attempt to read from write-only PPU address 0x{x}", addr);
            },
            0x2007 => self.ppu.read_addr(),
            0x2008...PPU_REGISTERS_MIRROR_END => {
                const mirror_down_addr = addr & 0b0010_0000_0000_0111;
                std.debug.panic("PPU is not supported yet. 0x{x}", .{mirror_down_addr});
            },
            0x8000...0xFFFF => {
                return self.read_program_rom(addr);
            },
            else => {
                std.debug.panic("Ignoring mem access at 0x{x}", .{addr});
                return 0;
            },
        }
    }

    pub fn mem_read_u16(ctx: *anyopaque, pos: u16) u16 {
        const lower = @as(u16, mem_read(ctx, pos));
        const upper = @as(u16, mem_read(ctx, pos + 1));
        return (upper << 8) | lower;
    }

    pub fn mem_write(ctx: *anyopaque, addr: u16, data: u8) void {
        const self: *Self = @ptrCast(@alignCast(ctx));
        switch (addr) {
            RAM...RAM_MIRROR_END => {
                const mirror_down_addr = addr & 0b0000_0111_1111_1111;
                self.cpu_vram[mirror_down_addr] = data;
            },
            0x2000 => self.ppu.write_to_ctrl(data),
            0x2006 => self.ppu.write_to_ppu_addr(data),
            0x2007 => self.ppu.write_data(data),
            0x2008...PPU_REGISTERS_MIRROR_END => {
                const mirror_down_addr = addr & 0b0010_0000_0000_0111;
                self.mem_write(mirror_down_addr, data);
            },
            0x8000...0xFFFF => {
                std.debug.panic("Attempt to write to Cartridge ROM space. at: 0x{x}", .{addr});
            },
            else => {
                std.debug.panic("Ignoring mem access at 0x{x}", .{addr});
            },
        }
    }

    pub fn mem_write_u16(ctx: *anyopaque, pos: u16, data: u16) void {
        const upper: u8 = @intCast(data >> 8);
        const lower: u8 = @intCast(data & 0x00FF);
        mem_write(ctx, pos, lower);
        mem_write(ctx, pos + 1, upper);
    }

    pub fn memory(self: *@This()) Mem {
        return Mem{ .ptr = @ptrCast(self), .impl = &.{ .mem_read = mem_read, .mem_read_u16 = mem_read_u16, .mem_write = mem_write, .mem_write_u16 = mem_write_u16 } };
    }
};
