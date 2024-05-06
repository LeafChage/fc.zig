const std = @import("std");
pub const Mem = struct {
    ptr: *anyopaque,
    impl: *const Interface,

    const Self = @This();
    pub const Interface = struct {
        mem_read: *const fn (self: Self, addr: u16) u8,
        mem_read_u16: *const fn (self: Self, pos: u16) u16,
        mem_write: *const fn (self: *Self, addr: u16, data: u8) void,
        mem_write_u16: *const fn (self: *Self, pos: u16, data: u16) void,
    };

    pub fn mem_read(self: Self, addr: u16) u8 {
        self.impl.mem_read(self.ptr, addr);
    }

    pub fn mem_read_u16(self: Self, pos: u16) u16 {
        self.impl.mem_read_u16(self.ptr, pos);
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

    const Self = @This();
    fn init() Self {
        return Self{
            .cpu_vram = [0]u8 ** 0x0800,
        };
    }

    pub fn mem_read(self: Self, addr: u16) u8 {
        switch (addr) {
            RAM...RAM_MIRROR_END => {
                const mirror_down_addr = addr & 0b0000_0111_1111_1111;
                return self.cpu_vram[mirror_down_addr];
            },
            PPU_REGISTERS...PPU_REGISTERS_MIRROR_END => {
                const mirror_down_addr = addr & 0b0010_0000_0000_0111;
                std.debug.panic("PPU is not supported yet. 0x{x}", .{mirror_down_addr});
            },
            else => {
                std.debug.panic("Ignoring mem access at 0x{x}", .{addr});
                return 0;
            },
        }
    }

    pub fn mem_read_u16(self: Self, pos: u16) u16 {
        const lower = @as(u16, self.mem_read(pos));
        const upper = @as(u16, self.mem_read(pos + 1));
        return (upper << 8) | lower;
    }

    pub fn mem_write(self: *Self, addr: u16, data: u8) void {
        switch (addr) {
            RAM...RAM_MIRROR_END => {
                const mirror_down_addr = addr & 0b0000_0111_1111_1111;
                self.cpu_vram[mirror_down_addr] = data;
            },
            PPU_REGISTERS...PPU_REGISTERS_MIRROR_END => {
                const mirror_down_addr = addr & 0b0010_0000_0000_0111;
                std.debug.panic("PPU is not supported yet. 0x{x}", .{mirror_down_addr});
            },
            else => {
                std.debug.panic("Ignoring mem access at 0x{x}", .{addr});
            },
        }
    }

    pub fn mem_write_u16(self: *Self, pos: u16, data: u16) void {
        const upper: u8 = @intCast(data >> 8);
        const lower: u8 = @intCast(data & 0x00FF);
        self.mem_write(pos, lower);
        self.mem_write(pos + 1, upper);
    }

    pub fn memory(self: *@This()) Mem {
        return Mem{ .ptr = @ptrCast(self), .impl = &.{ .mem_read = mem_read, .mem_read_u16 = mem_read_u16, .mem_write = mem_write, .mem_write_u16 = mem_write_u16 } };
    }
};
