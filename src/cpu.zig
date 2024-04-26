const std = @import("std");

const Codes: [std.math.maxInt(u8)]Code = init: {
    const codes = [_]Code{
        Code.init(0x69, CodeType.ADC, 2, AddressingMode.Immediate),
        Code.init(0x65, CodeType.ADC, 3, AddressingMode.ZeroPage),
        Code.init(0x75, CodeType.ADC, 4, AddressingMode.ZeroPageX),
        Code.init(0x6D, CodeType.ADC, 4, AddressingMode.Absolute),
        Code.init(0x7D, CodeType.ADC, 4, AddressingMode.AbsoluteX),
        Code.init(0x79, CodeType.ADC, 4, AddressingMode.AbsoluteY),
        Code.init(0x61, CodeType.ADC, 6, AddressingMode.IndirectX),
        Code.init(0x71, CodeType.ADC, 5, AddressingMode.IndirectY),
        Code.init(0x29, CodeType.AND, 2, AddressingMode.Immediate),
        Code.init(0x25, CodeType.AND, 3, AddressingMode.ZeroPage),
        Code.init(0x35, CodeType.AND, 4, AddressingMode.ZeroPageX),
        Code.init(0x2D, CodeType.AND, 4, AddressingMode.Absolute),
        Code.init(0x3D, CodeType.AND, 4, AddressingMode.AbsoluteX),
        Code.init(0x39, CodeType.AND, 4, AddressingMode.AbsoluteY),
        Code.init(0x21, CodeType.AND, 6, AddressingMode.IndirectX),
        Code.init(0x31, CodeType.AND, 5, AddressingMode.IndirectY),
        Code.init(0x0A, CodeType.ASL, 2, AddressingMode.None), // Accumulator
        Code.init(0x06, CodeType.ASL, 5, AddressingMode.ZeroPage),
        Code.init(0x16, CodeType.ASL, 6, AddressingMode.ZeroPageX),
        Code.init(0x0E, CodeType.ASL, 6, AddressingMode.Absolute),
        Code.init(0x1E, CodeType.ASL, 7, AddressingMode.AbsoluteX),
        Code.init(0x90, CodeType.BCC, 2, AddressingMode.None), // Relative
        Code.init(0xB0, CodeType.BCS, 2, AddressingMode.None), // Relative
        Code.init(0xF0, CodeType.BEQ, 2, AddressingMode.None), // Relative
        Code.init(0x24, CodeType.BIT, 3, AddressingMode.ZeroPage),
        Code.init(0x2C, CodeType.BIT, 4, AddressingMode.Absolute),
        Code.init(0x30, CodeType.BMI, 2, AddressingMode.None), // Relative
        Code.init(0xD0, CodeType.BNE, 2, AddressingMode.None), // Relative
        Code.init(0x10, CodeType.BPL, 2, AddressingMode.None), // Relative
        Code.init(0x00, CodeType.BRK, 7, AddressingMode.None), // implied
        Code.init(0x50, CodeType.BVC, 2, AddressingMode.None), // Relative
        Code.init(0x70, CodeType.BVS, 2, AddressingMode.None), // Relative
        Code.init(0x18, CodeType.CLC, 2, AddressingMode.None), // implied
        Code.init(0xD8, CodeType.CLD, 2, AddressingMode.None), // implied
        Code.init(0x58, CodeType.CLI, 2, AddressingMode.None), // implied
        Code.init(0xB8, CodeType.CLV, 2, AddressingMode.None), // implied
        Code.init(0xC9, CodeType.CMP, 2, AddressingMode.Immediate),
        Code.init(0xC5, CodeType.CMP, 2, AddressingMode.ZeroPage),
        Code.init(0xD5, CodeType.CMP, 4, AddressingMode.ZeroPageX),
        Code.init(0xCD, CodeType.CMP, 4, AddressingMode.Absolute),
        Code.init(0xDD, CodeType.CMP, 4, AddressingMode.AbsoluteX),
        Code.init(0xD9, CodeType.CMP, 4, AddressingMode.AbsoluteY),
        Code.init(0xC1, CodeType.CMP, 6, AddressingMode.IndirectX),
        Code.init(0xD1, CodeType.CMP, 5, AddressingMode.IndirectY),
        //
        // Code.init(0x, CodeType.CPX, , , AddressingMode.),
        // Code.init(0x, CodeType.CPX, , , AddressingMode.),
        // Code.init(0x, CodeType.CPX, , , AddressingMode.),
        //
        // Code.init(0x, CodeType.CPY, , , AddressingMode.),
        // Code.init(0x, CodeType.CPY, , , AddressingMode.),
        // Code.init(0x, CodeType.CPY, , , AddressingMode.),
        //
        // Code.init(0x, CodeType.DEC, , , AddressingMode.),
        // Code.init(0x, CodeType.DEC, , , AddressingMode.),
        // Code.init(0x, CodeType.DEC, , , AddressingMode.),
        // Code.init(0x, CodeType.DEC, , , AddressingMode.),
        //
        // Code.init(0x, CodeType.DEX, , , AddressingMode.),
        //
        // Code.init(0x, CodeType.DEY, , , AddressingMode.),
        //
        // Code.init(0x, CodeType.EOR, , , AddressingMode.),
        // Code.init(0x, CodeType.EOR, , , AddressingMode.),
        // Code.init(0x, CodeType.EOR, , , AddressingMode.),
        // Code.init(0x, CodeType.EOR, , , AddressingMode.),
        // Code.init(0x, CodeType.EOR, , , AddressingMode.),
        // Code.init(0x, CodeType.EOR, , , AddressingMode.),
        // Code.init(0x, CodeType.EOR, , , AddressingMode.),
        // Code.init(0x, CodeType.EOR, , , AddressingMode.),
        //
        // Code.init(0x, CodeType.INC, , , AddressingMode.),
        // Code.init(0x, CodeType.INC, , , AddressingMode.),
        // Code.init(0x, CodeType.INC, , , AddressingMode.),
        // Code.init(0x, CodeType.INC, , , AddressingMode.),

        Code.init(0xE8, CodeType.INX, 2, AddressingMode.None),
        Code.init(0xC8, CodeType.INY, 2, AddressingMode.None),
        //
        // Code.init(0x, CodeType.JMP, , , AddressingMode.),
        // Code.init(0x, CodeType.JMP, , , AddressingMode.),
        //
        // Code.init(0x, CodeType.JSR, , , AddressingMode.),
        //
        Code.init(0xA9, CodeType.LDA, 2, AddressingMode.Immediate),
        Code.init(0xA5, CodeType.LDA, 3, AddressingMode.ZeroPage),
        Code.init(0xB5, CodeType.LDA, 4, AddressingMode.ZeroPageY),
        Code.init(0xAD, CodeType.LDA, 4, AddressingMode.Absolute),
        Code.init(0xBD, CodeType.LDA, 4, AddressingMode.AbsoluteX),
        Code.init(0xB9, CodeType.LDA, 4, AddressingMode.AbsoluteY),
        Code.init(0xA1, CodeType.LDA, 6, AddressingMode.IndirectX),
        Code.init(0xB1, CodeType.LDA, 5, AddressingMode.IndirectY),

        Code.init(0xA2, CodeType.LDX, 2, AddressingMode.Immediate),
        Code.init(0xA6, CodeType.LDX, 3, AddressingMode.ZeroPage),
        Code.init(0xB6, CodeType.LDX, 4, AddressingMode.ZeroPageY),
        Code.init(0xAE, CodeType.LDX, 4, AddressingMode.Absolute),
        Code.init(0xBE, CodeType.LDX, 4, AddressingMode.AbsoluteY),

        Code.init(0xA0, CodeType.LDY, 2, AddressingMode.Immediate),
        Code.init(0xA4, CodeType.LDY, 3, AddressingMode.ZeroPage),
        Code.init(0xB4, CodeType.LDY, 4, AddressingMode.ZeroPageX),
        Code.init(0xAC, CodeType.LDY, 4, AddressingMode.Absolute),
        Code.init(0xBC, CodeType.LDY, 4, AddressingMode.AbsoluteX),

        // Code.init(0x, CodeType.LSR, , , AddressingMode.),
        // Code.init(0x, CodeType.LSR, , , AddressingMode.),
        // Code.init(0x, CodeType.LSR, , , AddressingMode.),
        // Code.init(0x, CodeType.LSR, , , AddressingMode.),
        // Code.init(0x, CodeType.LSR, , , AddressingMode.),
        //
        Code.init(0xEA, CodeType.NOP, 2, AddressingMode.None),
        //
        // Code.init(0x, CodeType.ORA, , , AddressingMode.),
        // Code.init(0x, CodeType.ORA, , , AddressingMode.),
        // Code.init(0x, CodeType.ORA, , , AddressingMode.),
        // Code.init(0x, CodeType.ORA, , , AddressingMode.),
        // Code.init(0x, CodeType.ORA, , , AddressingMode.),
        // Code.init(0x, CodeType.ORA, , , AddressingMode.),
        // Code.init(0x, CodeType.ORA, , , AddressingMode.),
        // Code.init(0x, CodeType.ORA, , , AddressingMode.),
        //
        // Code.init(0x, CodeType.PHA, , , AddressingMode.),
        //
        // Code.init(0x, CodeType.PHP, , , AddressingMode.),
        //
        // Code.init(0x, CodeType.PLA, , , AddressingMode.),
        //
        // Code.init(0x, CodeType.PLP, , , AddressingMode.),
        //
        // Code.init(0x, CodeType.ROL, , , AddressingMode.),
        // Code.init(0x, CodeType.ROL, , , AddressingMode.),
        // Code.init(0x, CodeType.ROL, , , AddressingMode.),
        // Code.init(0x, CodeType.ROL, , , AddressingMode.),
        // Code.init(0x, CodeType.ROL, , , AddressingMode.),
        //
        // Code.init(0x, CodeType.ROR, , , AddressingMode.),
        // Code.init(0x, CodeType.ROR, , , AddressingMode.),
        // Code.init(0x, CodeType.ROR, , , AddressingMode.),
        // Code.init(0x, CodeType.ROR, , , AddressingMode.),
        // Code.init(0x, CodeType.ROR, , , AddressingMode.),
        Code.init(0x40, CodeType.RTI, 6, AddressingMode.None), // implied
        Code.init(0x60, CodeType.RTS, 6, AddressingMode.None), // implied

        Code.init(0xE9, CodeType.SBC, 2, AddressingMode.Immediate),
        Code.init(0xE5, CodeType.SBC, 3, AddressingMode.ZeroPage),
        Code.init(0xF5, CodeType.SBC, 4, AddressingMode.ZeroPageX),
        Code.init(0xED, CodeType.SBC, 4, AddressingMode.Absolute),
        Code.init(0xFD, CodeType.SBC, 4, AddressingMode.AbsoluteX),
        Code.init(0xF9, CodeType.SBC, 4, AddressingMode.AbsoluteY),
        Code.init(0xE1, CodeType.SBC, 6, AddressingMode.IndirectX),
        Code.init(0xF1, CodeType.SBC, 5, AddressingMode.IndirectY),

        Code.init(0x38, CodeType.SEC, 2, AddressingMode.None),
        Code.init(0xF8, CodeType.SED, 2, AddressingMode.None),
        Code.init(0x78, CodeType.SEI, 2, AddressingMode.None),

        Code.init(0x85, CodeType.STA, 3, AddressingMode.ZeroPage),
        Code.init(0x95, CodeType.STA, 4, AddressingMode.ZeroPageX),
        Code.init(0x8D, CodeType.STA, 4, AddressingMode.Absolute),
        Code.init(0x9D, CodeType.STA, 5, AddressingMode.AbsoluteX),
        Code.init(0x99, CodeType.STA, 5, AddressingMode.AbsoluteY),
        Code.init(0x81, CodeType.STA, 6, AddressingMode.IndirectX),
        Code.init(0x91, CodeType.STA, 6, AddressingMode.IndirectY),

        Code.init(0x86, CodeType.STX, 3, AddressingMode.ZeroPage),
        Code.init(0x96, CodeType.STX, 4, AddressingMode.ZeroPageY),
        Code.init(0x8E, CodeType.STX, 4, AddressingMode.Absolute),

        Code.init(0x84, CodeType.STY, 3, AddressingMode.ZeroPage),
        Code.init(0x94, CodeType.STY, 4, AddressingMode.ZeroPageX),
        Code.init(0x8C, CodeType.STY, 4, AddressingMode.Absolute),
        Code.init(0xAA, CodeType.TAX, 2, AddressingMode.None), // implied
        Code.init(0xA8, CodeType.TAY, 2, AddressingMode.None), // implied
        Code.init(0xBA, CodeType.TSX, 2, AddressingMode.None), // implied
        Code.init(0x8A, CodeType.TXA, 2, AddressingMode.None), // implied
        Code.init(0x9A, CodeType.TXS, 2, AddressingMode.None), // implied
        Code.init(0x98, CodeType.TYA, 2, AddressingMode.None), // implied
    };

    var ordered_codes = [_]Code{Code.init(0x00, CodeType.BRK, 0, AddressingMode.None)} ** std.math.maxInt(u8);
    for (codes) |code| {
        ordered_codes[code.opecode] = code;
    }
    break :init ordered_codes;
};

pub const CPU = struct {
    register_a: u8,
    register_x: u8,
    register_y: u8,
    status: u8,
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
        const from = ProgramMemoryStartIndex;
        const to = from + program.len;
        @memcpy(self.memory[from..to], program);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    pub fn load_and_run(self: *Self, program: []const u8) void {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn run(self: *Self) void {
        while (true) {
            const opscode = self.mem_read(self.program_counter);
            self.program_counter += 1;

            const code = Codes[opscode];
            switch (code.t) {
                CodeType.LDA => self.lda(code.mode),
                CodeType.STA => self.sta(code.mode),
                CodeType.TAX => self.tax(),
                CodeType.INX => self.inx(),
                CodeType.INY => self.iny(),
                CodeType.BRK => return,
                CodeType.LDX => self.ldx(code.mode),
                CodeType.LDY => self.ldy(code.mode),
                CodeType.NOP => {},
                CodeType.SEC => self.sec(),
                CodeType.SED => self.sed(),
                CodeType.SEI => self.sei(),
                else => {
                    std.debug.panic("I don't know the opecode: {}", .{opscode});
                },
            }
            self.program_counter += code.mode.bytes();
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

const AddressingMode = enum {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    None,

    const Self = @This();
    pub fn bytes(self: Self) u16 {
        return switch (self) {
            AddressingMode.Immediate => 1,
            AddressingMode.Absolute => 2,
            AddressingMode.AbsoluteX => 2,
            AddressingMode.AbsoluteY => 2,
            AddressingMode.ZeroPage => 1,
            AddressingMode.ZeroPageX => 1,
            AddressingMode.ZeroPageY => 1,
            AddressingMode.IndirectX => 2,
            AddressingMode.IndirectY => 2,
            AddressingMode.None => 0,
        };
    }
};

const CodeType = enum {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
};

const Code = struct {
    opecode: u8,
    t: CodeType,
    cycle: usize,
    mode: AddressingMode,

    const Self = @This();
    pub fn init(opecode: u8, t: CodeType, cycle: u8, mode: AddressingMode) Self {
        return Self{
            .t = t,
            .opecode = opecode,
            .cycle = cycle,
            .mode = mode,
        };
    }
};
