const builtin = @import("builtin");
const std = @import("std");
const _code = @import("./code.zig");
const Codes = _code.Codes;
const CodeType = _code.CodeType;
const AddressingMode = _code.AddressingMode;
const StatusFlag = @import("./status_flag.zig").StatusFlag;

pub const CPUCallback = struct {
    ptr: *anyopaque,
    impl: *const Interface,

    pub const Interface = struct {
        call: *const fn (ctx: *anyopaque, cpu: *CPU) void,
    };

    pub fn run(self: @This(), cpu: *CPU) void {
        self.impl.call(self.ptr, cpu);
    }
};

const EmptyAction = struct {
    pub fn call(_: *anyopaque, _: *CPU) void {}

    pub fn callback(self: *@This()) CPUCallback {
        return CPUCallback{ .ptr = @ptrCast(self), .impl = &.{ .call = call } };
    }
};

pub const CPU = struct {
    register_a: u8,
    register_x: u8,
    register_y: u8,
    status: StatusFlag,
    stack_pointer: u8,
    program_counter: u16,

    memory: [0xFFFF + 1]u8,
    // program ROM 0x8000 ~ 0xFFFF
    const ProgramMemoryStartIndex = 0x8000;

    const Self = @This();
    pub fn init() Self {
        return CPU{
            .register_a = 0,
            .register_x = 0,
            .register_y = 0,
            .status = StatusFlag.init(),
            .program_counter = 0,
            .stack_pointer = 0xFF,
            .memory = std.mem.zeroes([0xFFFF + 1]u8),
        };
    }

    pub fn reset(self: *Self) void {
        self.register_a = 0;
        self.register_x = 0;
        self.status = StatusFlag.init();
        self.status.interrupt = true;
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
        var action = EmptyAction{};
        self.run_with_callback(action.callback());
    }

    pub fn run_with_callback(self: *Self, callback: CPUCallback) void {
        while (true) {
            callback.run(self);
            self.single_run();
        }
    }

    fn single_run(self: *Self) void {
        const opscode = self.mem_read(self.program_counter);
        const code = Codes[opscode];
        std.log.debug("0x{x} code 0x{x} {} {} {b}", .{ self.program_counter, opscode, code.t, code.mode, self.status.raw() });
        self.program_counter += 1;

        switch (code.t) {
            CodeType.BRK => {
                // self.brk();
            },
            CodeType.ADC => {
                self.adc(code.mode);
                self.program_counter += code.mode.bytes();
            },
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
            CodeType.TXA => {
                self.txa();
            },
            CodeType.INX => {
                self.inx();
            },
            CodeType.DEX => {
                self.dex();
            },
            CodeType.INY => {
                self.iny();
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
                std.log.warn("NOP: 0x{x}", .{opscode});
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
            CodeType.CLC => {
                self.clc();
            },
            CodeType.CLD => {
                self.cld();
            },
            CodeType.CLI => {
                self.cli();
            },
            CodeType.CLV => {
                self.clv();
            },
            CodeType.BIT => {
                self.bit(code.mode);
                self.program_counter += code.mode.bytes();
            },
            CodeType.JSR => {
                self.jsr();
            },
            CodeType.JMP => {
                self.jmp(code.mode);
            },
            CodeType.BCC => self.bcc(),
            CodeType.BCS => self.bcs(),
            CodeType.BEQ => self.beq(),
            CodeType.BNE => self.bne(),
            CodeType.BVC => self.bvc(),
            CodeType.BVS => self.bvs(),
            CodeType.BPL => self.bpl(),
            CodeType.BMI => self.bmi(),
            CodeType.CMP => {
                self.cmp(code.mode);
                self.program_counter += code.mode.bytes();
            },
            CodeType.RTS => {
                self.rts();
            },
            CodeType.ORA => {
                self.ora(code.mode);
                self.program_counter += code.mode.bytes();
            },
            CodeType.AND => {
                self.andexec(code.mode);
                self.program_counter += code.mode.bytes();
            },
            CodeType.EOR => {
                self.eor(code.mode);
                self.program_counter += code.mode.bytes();
            },
            CodeType.ROL => {
                self.rol(code.mode);
                self.program_counter += code.mode.bytes();
            },
            CodeType.ASL => {
                if (code.mode == AddressingMode.None) {
                    self.asl_accumulator();
                } else {
                    self.asl(code.mode);
                    self.program_counter += code.mode.bytes();
                }
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

    // for test
    fn test_for_single_run(self: *Self, program: []const u8, prepare: fn (v: *CPU) void) void {
        if (!builtin.is_test) {
            unreachable;
        }
        self.load(program);
        self.reset();
        prepare(self);
        self.single_run();
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

    fn brk(self: *Self) void {
        if (self.status.interrupt) {
            return;
        }
        self.status.break_cmd = true;
        self.stack_push_u16(self.program_counter);
        self.stack_push(self.status.raw());
        self.status.interrupt = true;
        const lower = @as(u16, self.mem_read(0xFFFE));
        const upper = @as(u16, self.mem_read(0xFFFF));
        self.program_counter = (upper << 8) | lower;
    }

    fn adc(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        const value = @as(u16, self.mem_read(addr));
        const register_a = @as(u16, self.register_a);
        const carry: u16 = if (self.status.carry) 1 else 0;

        // cast from u8 to u16 for checking carry flag
        const sum = register_a + value + carry;

        self.status.carry = sum > 0xFF;

        // TODO:
        // ref: https://github.com/bugzmanov/nes_ebook/blob/5e1433984362b62f7d4cec4280691c0c8833cb61/code/ch3.3/src/cpu.rs#L280C1-L280C70
        self.status.overflow = (value ^ sum) & (sum ^ self.register_a) & 0x80 != 0;

        self.register_a = @intCast(sum & 0x00FF);
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
        cpu.test_for_single_run(&[_]u8{ 0xA2, 0x05, 0x00 }, struct {
            fn f(_: *CPU) void {}
        }.f);
        try std.testing.expectEqual(cpu.register_x, 0x05);
        try std.testing.expect(!cpu.status.zero);
        try std.testing.expect(!cpu.status.negative);
    }
    test "ldx zero flag" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xA2, 0x00, 0x00 }, struct {
            fn f(_: *CPU) void {}
        }.f);
        try std.testing.expect(cpu.status.zero);
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
        cpu.test_for_single_run(&[_]u8{ 0xA0, 0x05, 0x00 }, struct {
            fn f(_: *CPU) void {}
        }.f);
        try std.testing.expectEqual(cpu.register_y, 0x05);
        try std.testing.expect(!cpu.status.zero);
        try std.testing.expect(!cpu.status.negative);
    }
    test "ldy zero flag" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xA0, 0x00, 0x00 }, struct {
            fn f(_: *CPU) void {}
        }.f);
        try std.testing.expect(cpu.status.zero);
    }

    // transfer A to X
    fn tax(self: *Self) void {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flag(self.register_x);
    }

    test "tax" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xAA, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_a = 0x10;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_x, 0x10);
    }

    // transfer x to A
    fn txa(self: *Self) void {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flag(self.register_a);
    }

    test "txa" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x8A, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_x = 0x10;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_a, 0x10);
    }

    // increment X
    fn inx(self: *Self) void {
        self.register_x = self.register_x +% 1;
        self.update_zero_and_negative_flag(self.register_x);
    }

    test "inx" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xE8, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_x = 0x01;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_x, 0x02);
    }

    test "inx overflow" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xE8, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_x = 0xFF;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_x, 0x00);
    }

    // decrement X
    fn dex(self: *Self) void {
        self.register_x = self.register_x -% 1;
        self.update_zero_and_negative_flag(self.register_x);
    }

    test "dex" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xCA, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_x = 0x01;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_x, 0x00);
        try std.testing.expect(cpu.status.zero);
    }

    test "dex overflow" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xCA, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_x = 0x00;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_x, 0xFF);
        try std.testing.expect(!cpu.status.zero);
    }

    // increment Y
    fn iny(self: *Self) void {
        self.register_y = self.register_y +% 1;
        self.update_zero_and_negative_flag(self.register_y);
    }

    test "iny" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xC8, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_y = 0x01;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_y, 0x02);
    }

    test "iny overflow" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xC8, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_y = 0xFF;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_y, 0x00);
    }

    // store A to memory
    fn sta(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    test "sta" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x85, 0x10 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_a = 0x55;
            }
        }.f);
        try std.testing.expectEqual(cpu.mem_read(0x10), 0x55);
    }

    // store X to memory
    fn stx(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    test "stx" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x86, 0x10 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_x = 0x55;
            }
        }.f);
        try std.testing.expectEqual(cpu.mem_read(0x10), 0x55);
    }

    // store Y to memory
    fn sty(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    test "sty" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x84, 0x10 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_y = 0x55;
            }
        }.f);
        try std.testing.expectEqual(cpu.mem_read(0x10), 0x55);
    }

    // set carry flag
    fn sec(self: *Self) void {
        self.status.carry = true;
    }

    test "sec" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x38, 0x00 }, struct {
            fn f(_: *CPU) void {}
        }.f);
        try std.testing.expect(cpu.status.carry);
    }

    // set decimal flag
    fn sed(self: *Self) void {
        self.status.decimal = true;
    }

    test "sed" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xF8, 0x00 }, struct {
            fn f(_: *CPU) void {}
        }.f);
        try std.testing.expect(cpu.status.decimal);
    }

    // set interrupt flag
    fn sei(self: *Self) void {
        self.status.interrupt = true;
    }

    test "sei" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x78, 0x00 }, struct {
            fn f(_: *CPU) void {}
        }.f);
        try std.testing.expect(cpu.status.interrupt);
    }

    // unset carry flag
    fn clc(self: *Self) void {
        self.status.carry = false;
    }

    test "clc" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x18, 0x00 }, struct {
            fn f(_: *CPU) void {}
        }.f);
        try std.testing.expect(!cpu.status.carry);
    }

    // unset decimal flag
    fn cld(self: *Self) void {
        self.status.decimal = false;
    }

    test "cld" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xD8, 0x00 }, struct {
            fn f(_: *CPU) void {}
        }.f);
        try std.testing.expect(!cpu.status.decimal);
    }

    // unset interrupt flag
    fn cli(self: *Self) void {
        self.status.interrupt = false;
    }

    test "cli" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x58, 0x00 }, struct {
            fn f(_: *CPU) void {}
        }.f);
        try std.testing.expect(!cpu.status.interrupt);
    }

    // unset overflow flag
    fn clv(self: *Self) void {
        self.status.overflow = false;
    }

    test "clv" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xB8, 0x00 }, struct {
            fn f(_: *CPU) void {}
        }.f);
        try std.testing.expect(!cpu.status.overflow);
    }

    fn bit(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        const data = self.mem_read(addr);
        const result = self.register_a & data;
        if (result == 0) {
            self.status.zero = true;
        }
        self.status.negative = data & 0b1000_0000 == 0b1000_0000;
        self.status.overflow = data & 0b0100_0000 == 0b0100_0000;
    }

    fn jsr(self: *Self) void {
        // JSR is with absolute operand which has two bites, and this tries to push address before next code
        self.stack_push_u16(self.program_counter + 1);

        const addr = self.mem_read_u16(self.program_counter);
        std.log.debug("JSR: push: 0x{x}, jump to: 0x{x} ", .{ self.program_counter + 1, addr });
        self.program_counter = addr;
    }

    fn jmp(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        std.log.debug("JMP: jump to: 0x{x} ", .{addr});
        self.program_counter = addr;
    }

    fn branch(self: *Self, flag: bool) void {
        if (flag) {
            const offset: i8 = @bitCast(self.mem_read(self.program_counter));
            const addr = @as(isize, self.program_counter + 1) + @as(isize, offset);
            std.log.debug("Branch: offset 0x{x}, jump to: 0x{x}", .{ offset, addr });
            self.program_counter = @as(u16, @intCast(addr));
        } else {
            self.program_counter += 1;
        }
    }

    fn bcc(self: *Self) void {
        self.branch(!self.status.carry);
    }
    fn bcs(self: *Self) void {
        self.branch(self.status.carry);
    }
    fn beq(self: *Self) void {
        self.branch(self.status.zero);
    }
    fn bne(self: *Self) void {
        self.branch(!self.status.zero);
    }
    fn bvc(self: *Self) void {
        self.branch(!self.status.overflow);
    }
    fn bvs(self: *Self) void {
        self.branch(self.status.overflow);
    }
    fn bpl(self: *Self) void {
        self.branch(!self.status.negative);
    }
    fn bmi(self: *Self) void {
        self.branch(self.status.negative);
    }

    test "bcc jump +5" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0x90, 0x05 });
        cpu.reset();
        const next = cpu.program_counter + 2;
        cpu.status.carry = false;
        _ = cpu.single_run();
        try std.testing.expectEqual(next + 0x05, cpu.program_counter);
    }
    test "bcc jump -10" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0x90, @bitCast(@as(i8, -0x05)) });
        cpu.reset();
        const next = cpu.program_counter + 2;
        cpu.status.carry = false;
        _ = cpu.single_run();
        try std.testing.expectEqual(next - 0x05, cpu.program_counter);
    }
    test "bcc no jump" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0x90, 0x05 });
        cpu.reset();
        const next = cpu.program_counter + 2;
        cpu.status.carry = true;
        _ = cpu.single_run();
        try std.testing.expectEqual(next, cpu.program_counter);
    }
    test "bcs jump" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0xB0, 0x05 });
        cpu.reset();
        const next = cpu.program_counter + 2;
        cpu.status.carry = true;
        _ = cpu.single_run();
        try std.testing.expectEqual(next + 0x05, cpu.program_counter);
    }
    test "beq jump +5" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0xF0, 0x05 });
        cpu.reset();
        const next = cpu.program_counter + 2;
        cpu.status.zero = true;
        _ = cpu.single_run();
        try std.testing.expectEqual(next + 0x05, cpu.program_counter);
    }
    test "bne jump +5" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0xD0, 0x05 });
        cpu.reset();
        const next = cpu.program_counter + 2;
        cpu.status.zero = false;
        _ = cpu.single_run();
        try std.testing.expectEqual(next + 0x05, cpu.program_counter);
    }
    test "bvc jump +5" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0x50, 0x05 });
        cpu.reset();
        const next = cpu.program_counter + 2;
        cpu.status.overflow = false;
        _ = cpu.single_run();
        try std.testing.expectEqual(next + 0x05, cpu.program_counter);
    }
    test "bvs jump +5" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0x70, 0x05 });
        cpu.reset();
        const next = cpu.program_counter + 2;
        cpu.status.overflow = true;
        _ = cpu.single_run();
        try std.testing.expectEqual(next + 0x05, cpu.program_counter);
    }
    test "bpl jump +5" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0x10, 0x05 });
        cpu.reset();
        const next = cpu.program_counter + 2;
        cpu.status.negative = false;
        _ = cpu.single_run();
        try std.testing.expectEqual(next + 0x05, cpu.program_counter);
    }
    test "bmi jump +5" {
        var cpu = CPU.init();
        cpu.load(&[_]u8{ 0x30, 0x05 });
        cpu.reset();
        const next = cpu.program_counter + 2;
        cpu.status.negative = true;
        _ = cpu.single_run();
        try std.testing.expectEqual(next + 0x05, cpu.program_counter);
    }

    fn rts(self: *Self) void {
        const addr = self.stack_pop_u16();
        std.log.debug("RTS: return to: 0x{x} ", .{addr});
        self.program_counter = addr + 1;
    }

    fn cmp(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        const mem = self.mem_read(addr);
        self.status.carry = self.register_a >= mem;
        self.update_zero_and_negative_flag(self.register_a -% mem);
    }

    test "cmp with carry" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xC9, 0x01, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_a = 0x01;
            }
        }.f);
        try std.testing.expect(cpu.status.carry);
        try std.testing.expect(cpu.status.zero);
    }

    test "cmp not carry" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0xC9, 0x01, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_a = 0x00;
            }
        }.f);
        try std.testing.expect(!cpu.status.carry);
    }

    // A = M | A
    fn ora(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        const b = self.mem_read(addr);
        self.register_a = self.register_a | b;
        self.update_zero_and_negative_flag(self.register_a);
    }

    test "ora" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x09, 0b1000_0000, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_a = 0b0000_1111;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_a, 0b1000_1111);
    }

    // A = M & A
    fn andexec(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        const b = self.mem_read(addr);
        self.register_a = self.register_a & b;
        self.update_zero_and_negative_flag(self.register_a);
    }

    test "andexec" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x29, 0b1000_0000, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_a = 0b1000_1111;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_a, 0b1000_0000);
    }

    // A = M ^ A
    fn eor(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        const b = self.mem_read(addr);
        self.register_a = self.register_a ^ b;
        self.update_zero_and_negative_flag(self.register_a);
    }

    test "eor" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x49, 0b1000_0000, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_a = 0b1000_1111;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_a, 0b0000_1111);
    }

    // // A << left
    fn rol(self: *Self, _: AddressingMode) void {
        // flag carry from bit 7 of A
        const carry = self.register_a & 0b1000_0000 == 0b1000_0000;
        self.register_a <<= 1;
        if (self.status.carry) {
            self.register_a |= 0b0000_0001;
        }
        self.status.carry = carry;
        self.update_zero_and_negative_flag(self.register_a);
    }

    test "rol" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x2A, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.status.carry = false;
                _cpu.register_a = 0b0000_1111;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_a, 0b0001_1110);
        try std.testing.expect(!cpu.status.carry);
    }
    test "rol with carry" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x2A, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.status.carry = true;
                _cpu.register_a = 0b0000_1111;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_a, 0b0001_1111);
        try std.testing.expect(!cpu.status.carry);
    }
    test "rol set carry" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x2A, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.status.carry = false;
                _cpu.register_a = 0b1000_1111;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_a, 0b0001_1110);
        try std.testing.expect(cpu.status.carry);
    }

    //
    // ASL shift to left and flag carry from bit 7 of A
    //
    fn asl_accumulator(self: *Self) void {
        const carry = self.register_a & 0b1000_0000 == 0b1000_0000;
        self.register_a <<= 1;
        self.status.carry = carry;
        self.update_zero_and_negative_flag(self.register_a);
    }
    fn asl(self: *Self, mode: AddressingMode) void {
        const addr = self.get_operand_address(mode);
        const d = self.mem_read(addr);
        const carry = d & 0b1000_0000 == 0b1000_0000;
        self.mem_write(addr, d << 1);
        self.status.carry = carry;
        self.update_zero_and_negative_flag(self.register_a);
    }
    test "asl" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x0A, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_a = 0b0000_1111;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_a, 0b0001_1110);
        try std.testing.expect(!cpu.status.carry);
    }
    test "asl set carry" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x0A, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.register_a = 0b1000_1111;
            }
        }.f);
        try std.testing.expectEqual(cpu.register_a, 0b0001_1110);
        try std.testing.expect(cpu.status.carry);
    }
    test "asl accumulator" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x06, 0x10, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.mem_write(0x10, 0b0000_1111);
            }
        }.f);
        try std.testing.expectEqual(cpu.mem_read(0x10), 0b0001_1110);
        try std.testing.expect(!cpu.status.carry);
    }
    test "asl accumulator set carry" {
        var cpu = CPU.init();
        cpu.test_for_single_run(&[_]u8{ 0x06, 0x10, 0x00 }, struct {
            fn f(_cpu: *CPU) void {
                _cpu.mem_write(0x10, 0b1000_1111);
            }
        }.f);
        try std.testing.expectEqual(cpu.mem_read(0x10), 0b0001_1110);
        try std.testing.expect(cpu.status.carry);
    }

    fn update_zero_and_negative_flag(self: *Self, result: u8) void {
        // check zero flag
        self.status.zero = result == 0;

        // check negative flag
        self.status.negative = (self.register_a & 0b1000_0000) != 0;
    }

    ///
    /// control memory
    ///
    pub fn mem_read(self: Self, addr: u16) u8 {
        if (addr == 0xFF) {
            std.log.info("check keydown: 0x{x}", .{self.memory[addr]});
        }
        return self.memory[addr];
    }

    fn mem_read_u16(self: Self, pos: u16) u16 {
        const lower = @as(u16, self.mem_read(pos));
        const upper = @as(u16, self.mem_read(pos + 1));
        return (upper << 8) | lower;
    }

    pub fn mem_write(self: *Self, addr: u16, data: u8) void {
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
    fn stack_push_u16(self: *Self, data: u16) void {
        // little endian
        const lower: u8 = @truncate(data & 0x00FF);
        const upper: u8 = @truncate(data >> 8);
        self.stack_push(lower);
        self.stack_push(upper);
    }
    fn stack_push(self: *Self, data: u8) void {
        self.stack_pointer -= 1;
        const p = 0x0100 | @as(u16, self.stack_pointer);
        std.log.debug("Stack Push 0x{x}, 0x{x}", .{ p, data });
        self.mem_write(p, data);
    }

    fn stack_pop(self: *Self) u8 {
        const p = 0x0100 | @as(u16, self.stack_pointer);
        self.stack_pointer += 1;
        std.log.debug("Stack Pop 0x{x}, 0x{x}", .{ p, self.mem_read(p) });
        return self.mem_read(p);
    }
    fn stack_pop_u16(self: *Self) u16 {
        // little endian
        const upper = @as(u16, self.stack_pop());
        const lower = @as(u16, self.stack_pop());
        return (upper << 8) | lower;
    }

    test "stack pop push" {
        var cpu = CPU.init();
        cpu.stack_push(1);
        try std.testing.expectEqual(cpu.stack_pointer, 0xff - 1);
        cpu.stack_push(2);
        try std.testing.expectEqual(cpu.stack_pointer, 0xff - 2);
        cpu.stack_push(3);
        try std.testing.expectEqual(cpu.stack_pointer, 0xff - 3);

        try std.testing.expectEqual(cpu.stack_pop(), 3);
        try std.testing.expectEqual(cpu.stack_pointer, 0xff - 2);
        try std.testing.expectEqual(cpu.stack_pop(), 2);
        try std.testing.expectEqual(cpu.stack_pointer, 0xff - 1);
        try std.testing.expectEqual(cpu.stack_pop(), 1);
        try std.testing.expectEqual(cpu.stack_pointer, 0xff);
    }
};

test "lda immediate" {
    var cpu = CPU.init();
    cpu.test_for_single_run(&[_]u8{ 0xA9, 0x05, 0x00 }, struct {
        fn f(_: *CPU) void {}
    }.f);
    try std.testing.expectEqual(cpu.register_a, 0x05);
    try std.testing.expect(!cpu.status.zero);
    try std.testing.expect(!cpu.status.negative);
}

test "lda from memory" {
    var cpu = CPU.init();
    cpu.test_for_single_run(&[_]u8{ 0xa5, 0x10, 0x00 }, struct {
        fn f(_cpu: *CPU) void {
            _cpu.mem_write(0x10, 0x55);
        }
    }.f);
    try std.testing.expectEqual(cpu.register_a, 0x55);
}

test "lda zero flag" {
    var cpu = CPU.init();
    cpu.test_for_single_run(&[_]u8{ 0xA9, 0x00 }, struct {
        fn f(_: *CPU) void {}
    }.f);
    try std.testing.expectEqual(cpu.register_a, 0x00);
    try std.testing.expect(cpu.status.zero);
}
