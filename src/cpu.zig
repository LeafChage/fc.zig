const std = @import("std");
const _code = @import("./code.zig");
const Codes = _code.Codes;
const CodeType = _code.CodeType;
const AddressingMode = _code.AddressingMode;

pub const CPU = struct {
    register_a: u8,
    register_x: u8,
    register_y: u8,
    status: u8,
    stack_pointer: u8,
    program_counter: u16,

    memory: [0xFFFF]u8,
    // program ROM 0x8000 ~ 0xFFFF
    const ProgramMemoryStartIndex = 0x8000;

    const Self = @This();
    pub fn init() Self {
        return CPU{
            .register_a = 0,
            .register_x = 0,
            .register_y = 0,
            .status = 0,
            .program_counter = 0,
            .stack_pointer = 0xFF,
            .memory = std.mem.zeroes([0xFFFF]u8),
        };
    }

    pub fn reset(self: *Self) void {
        self.register_a = 0;
        self.register_x = 0;
        self.status = 0;
        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load(self: *Self, program: []const u8) void {
        // const from = ProgramMemoryStartIndex;
        const from = 0x0600;
        const to = from + program.len;
        @memcpy(self.memory[from..to], program);
        // self.mem_write_u16(0xFFFC, 0x8000);
        self.mem_write_u16(0xFFFC, from);
    }

    pub fn load_and_run(self: *Self, program: []const u8) void {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn run(self: *Self) void {
        self.run_with_callback(struct {
            pub fn f(_: Self) void {}
        }.f);
    }

    pub fn run_with_callback(self: *Self, comptime callback: fn (cpu: *Self) void) void {
        while (true) {
            callback(self);
            const opscode = self.mem_read(self.program_counter);
            self.program_counter += 1;

            const code = Codes[opscode];
            std.log.debug("code {} {}", .{ opscode, code.t });

            switch (code.t) {
                CodeType.LDA => {
                    self.lda(code.mode);
                    self.program_counter += code.mode.bytes();
                },
                CodeType.STA => {
                    self.sta(code.mode);
                    self.program_counter += code.mode.bytes();
                },
                CodeType.STX => {
                    self.stx(code.mode);
                    self.program_counter += code.mode.bytes();
                },
                CodeType.STY => {
                    self.sty(code.mode);
                    self.program_counter += code.mode.bytes();
                },
                CodeType.TAX => {
                    self.tax();
                },
                CodeType.INX => {
                    self.inx();
                },
                CodeType.INY => {
                    self.iny();
                },
                CodeType.BRK => {
                    return;
                },
                CodeType.LDX => {
                    self.ldx(code.mode);
                    self.program_counter += code.mode.bytes();
                },
                CodeType.LDY => {
                    self.ldy(code.mode);
                    self.program_counter += code.mode.bytes();
                },
                CodeType.NOP => {
                    // do nothing
                },
                CodeType.SEC => {
                    self.sec();
                },
                CodeType.SED => {
                    self.sed();
                },
                CodeType.SEI => {
                    self.sei();
                },
                CodeType.JSR => {
                    self.jsr(code.mode);
                },
                CodeType.RTS => {
                    self.rts();
                },
                else => {
                    std.debug.panic("I don't know the opecode: 0x{x} {} {}", .{
                        opscode,
                        code.t,
                        code.mode,
                    });
                },
            }
        }
    }

    fn get_operand_address(self: Self, mode: AddressingMode) u16 {
        return switch (mode) {
            AddressingMode.Immediate => self.program_counter,
            AddressingMode.ZeroPage => @intCast(self.mem_read(self.program_counter)),
            AddressingMode.Absolute => self.mem_read_u16(self.program_counter),
            AddressingMode.ZeroPageX => {
                const pos = self.mem_read(self.program_counter);
                return @intCast(pos +% self.register_x);
            },
            AddressingMode.ZeroPageY => {
                const pos = self.mem_read(self.program_counter);
                return @intCast(pos +% self.register_y);
            },
            AddressingMode.AbsoluteX => {
                const base = self.mem_read_u16(self.program_counter);
                return @intCast(base +% self.register_x);
            },
            AddressingMode.AbsoluteY => {
                const base = self.mem_read_u16(self.program_counter);
                return @intCast(base +% self.register_y);
            },
            AddressingMode.IndirectX => {
                const base = self.mem_read(self.program_counter);
                const ptr = base +% self.register_x;

                const lower = @as(u16, self.mem_read(@intCast(ptr)));
                const upper = @as(u16, self.mem_read(@intCast(ptr +% 1)));
                return upper << 8 | lower;
            },
            AddressingMode.IndirectY => {
                const base = self.mem_read(self.program_counter);
                const lower = @as(u16, self.mem_read(@as(u16, base)));
                const upper = @as(u16, self.mem_read(@as(u16, base +% 1)));
                const deref_base = upper << 8 | lower;
                return deref_base +% self.register_y;
            },
            AddressingMode.None => @panic("mode ? is not supported"),
        };
    }

    // load to A
    fn lda(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        const value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flag(self.register_a);
    }

    // load to X
    fn ldx(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        const value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flag(self.register_x);
    }
    test "ldx" {
        var cpu = CPU.init();
        cpu.load_and_run(&[_]u8{ 0xA2, 0x05, 0x00 });
        try std.testing.expectEqual(cpu.register_x, 0x05);
        try std.testing.expect(cpu.status & 0b0000_0010 == 0);
        try std.testing.expect(cpu.status & 0b1000_0000 == 0);
    }
    test "ldx zero flag" {
        var cpu = CPU.init();
        cpu.load_and_run(&[_]u8{ 0xA2, 0x00, 0x00 });
        try std.testing.expect(cpu.status & 0b0000_0010 == 0b10);
    }

    // load to Y
    fn ldy(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        const value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flag(self.register_y);
    }
    test "ldy" {
        var cpu = CPU.init();
        cpu.load_and_run(&[_]u8{ 0xA0, 0x05, 0x00 });
        try std.testing.expectEqual(cpu.register_y, 0x05);
        try std.testing.expect(cpu.status & 0b0000_0010 == 0);
        try std.testing.expect(cpu.status & 0b1000_0000 == 0);
    }
    test "ldy zero flag" {
        var cpu = CPU.init();
        cpu.load_and_run(&[_]u8{ 0xA0, 0x00, 0x00 });
        try std.testing.expect(cpu.status & 0b0000_0010 == 0b10);
    }

    // transfer A to X
    fn tax(self: *Self) void {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flag(self.register_x);
    }

    // increment X
    fn inx(self: *Self) void {
        self.register_x = self.register_x +% 1;
        self.update_zero_and_negative_flag(self.register_x);
    }

    test "inx" {
        var cpu = CPU.init();
        cpu.load_and_run(&[_]u8{ 0xE8, 0x00 });
        cpu.reset();
        cpu.register_x = 0x01;
        cpu.run();
        try std.testing.expectEqual(cpu.register_x, 0x02);
    }

    test "inx overflow" {
        var cpu = CPU.init();
        cpu.load_and_run(&[_]u8{ 0xE8, 0x00 });
        cpu.reset();
        cpu.register_x = 0xFF;
        cpu.run();
        try std.testing.expectEqual(cpu.register_x, 0x00);
    }

    // increment Y
    fn iny(self: *Self) void {
        self.register_y = self.register_y +% 1;
        self.update_zero_and_negative_flag(self.register_y);
    }

    test "iny" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0xC8, 0x00 });
        cpu.reset();
        cpu.register_y = 0x01;
        cpu.run();
        try std.testing.expectEqual(cpu.register_y, 0x02);
    }

    test "iny overflow" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0xC8, 0x00 });
        cpu.reset();
        cpu.register_y = 0xFF;
        cpu.run();
        try std.testing.expectEqual(cpu.register_y, 0x00);
    }

    // store A to memory
    fn sta(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }
    test "sta" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0x85, 0x10 });
        cpu.reset();
        cpu.register_a = 0x55;
        cpu.run();
        try std.testing.expectEqual(cpu.mem_read(0x10), 0x55);
    }

    // store X to memory
    fn stx(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }
    test "stx" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0x86, 0x10 });
        cpu.reset();
        cpu.register_x = 0x55;
        cpu.run();
        try std.testing.expectEqual(cpu.mem_read(0x10), 0x55);
    }

    // store Y to memory
    fn sty(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }
    test "sty" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0x84, 0x10 });
        cpu.reset();
        cpu.register_y = 0x55;
        cpu.run();
        try std.testing.expectEqual(cpu.mem_read(0x10), 0x55);
    }

    // set carry flag
    fn sec(self: *Self) void {
        self.status = self.status | 0b1000_0000;
    }
    test "sec" {
        var cpu = CPU.init();
        cpu.load_and_run(&[_]u8{ 0x38, 0x00 });
        try std.testing.expectEqual(cpu.status & 0b1000_0000, 0b1000_0000);
    }

    // set decimal flag
    fn sed(self: *Self) void {
        self.status = self.status | 0b0001_0000;
    }
    test "sed" {
        var cpu = CPU.init();
        cpu.load_and_run(&[_]u8{ 0xF8, 0x00 });
        try std.testing.expectEqual(cpu.status & 0b0001_0000, 0b0001_0000);
    }

    // set interrupt flag
    fn sei(self: *Self) void {
        self.status = self.status | 0b0010_0000;
    }
    test "sei" {
        var cpu = CPU.init();
        cpu.load_and_run(&[_]u8{ 0x78, 0x00 });
        try std.testing.expectEqual(cpu.status & 0b0010_0000, 0b0010_0000);
    }

    fn jsr(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        const p = self.program_counter - 1;
        const lower: u8 = @intCast(p & 0x00FF);
        const upper: u8 = @intCast(p >> 8);
        // little endian
        self.stack_push(lower);
        self.stack_push(upper);

        std.log.debug("JSR: push: 0x{x}, jump to: 0x{x} ", .{ p, addr });
        self.program_counter = addr;
    }

    fn rts(self: *Self) void {
        const upper = @as(u16, self.stack_pop());
        const lower = @as(u16, self.stack_pop());
        const addr = (upper << 8) | lower;
        std.log.debug("RTS: return to: 0x{x} ", .{addr});
        self.program_counter = addr;
    }

    fn update_zero_and_negative_flag(self: *Self, result: u8) void {
        if (result == 0) {
            self.status = self.status | 0b0000_0010;
        } else {
            self.status = self.status & 0b1111_1101;
        }

        if ((self.register_a & 0b1000_0000) != 0) {
            self.status = self.status | 0b1000_0000;
        } else {
            self.status = self.status & 0b0111_1111;
        }
    }

    ///
    /// control memory
    ///
    fn mem_read(self: Self, addr: u16) u8 {
        return self.memory[addr];
    }

    fn mem_read_u16(self: Self, pos: u16) u16 {
        const lower = @as(u16, self.mem_read(pos));
        const upper = @as(u16, self.mem_read(pos + 1));
        return (upper << 8) | lower;
    }

    fn mem_write(self: *Self, addr: u16, data: u8) void {
        self.memory[addr] = data;
    }

    fn mem_write_u16(self: *Self, pos: u16, data: u16) void {
        const upper: u8 = @intCast(data >> 8);
        const lower: u8 = @intCast(data & 0x00FF);
        self.mem_write(pos, lower);
        self.mem_write(pos + 1, upper);
    }

    ///
    /// control stack
    ///
    fn stack_push(self: *Self, data: u8) void {
        self.stack_pointer -= 1;
        const p = 0x0100 & @as(u16, self.stack_pointer);
        return self.mem_write(p, data);
    }

    fn stack_pop(self: *Self) u8 {
        const p = 0x0100 & @as(u16, self.stack_pointer);
        self.stack_pointer += 1;
        return self.mem_read(p);
    }
};

test "lda immediate" {
    var cpu = CPU.init();
    cpu.load_and_run(&[_]u8{ 0xA9, 0x05, 0x00 });
    try std.testing.expectEqual(cpu.register_a, 0x05);
    try std.testing.expect(cpu.status & 0b0000_0010 == 0b00);
    try std.testing.expect(cpu.status & 0b1000_0000 == 0);
}
test "lda from memory" {
    var cpu = CPU.init();
    cpu.mem_write(0x10, 0x55);
    cpu.load_and_run(&[_]u8{ 0xa5, 0x10, 0x00 });
    try std.testing.expectEqual(cpu.register_a, 0x55);
}

test "lda zero flag" {
    var cpu = CPU.init();
    cpu.load_and_run(&[_]u8{ 0xA9, 0x00, 0x00 });
    try std.testing.expectEqual(cpu.register_a, 0x00);
    try std.testing.expect(cpu.status & 0b0000_0010 == 0b10);
}

test "tax" {
    var cpu = CPU.init();
    cpu.load(&[_]u8{ 0xAA, 0x00 });
    cpu.reset();
    cpu.register_a = 0x10;
    cpu.run();
    try std.testing.expectEqual(cpu.register_x, 0x10);
}

test "working_together" {
    var cpu = CPU.init();
    cpu.load_and_run(&[_]u8{ 0xA9, 0xc0, 0xAA, 0xE8, 0x00 });
    try std.testing.expectEqual(cpu.register_x, 0xC1);
}
