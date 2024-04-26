const std = @import("std");

pub const Codes: [std.math.maxInt(u8)]Code = init: {
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
        Code.init(0x90, CodeType.BCC, 2, AddressingMode.None),
        Code.init(0xB0, CodeType.BCS, 2, AddressingMode.None),
        Code.init(0xF0, CodeType.BEQ, 2, AddressingMode.None),
        Code.init(0x24, CodeType.BIT, 3, AddressingMode.ZeroPage),
        Code.init(0x2C, CodeType.BIT, 4, AddressingMode.Absolute),
        Code.init(0x30, CodeType.BMI, 2, AddressingMode.None),
        Code.init(0xD0, CodeType.BNE, 2, AddressingMode.None),
        Code.init(0x10, CodeType.BPL, 2, AddressingMode.None),
        Code.init(0x00, CodeType.BRK, 7, AddressingMode.None),
        Code.init(0x50, CodeType.BVC, 2, AddressingMode.None),
        Code.init(0x70, CodeType.BVS, 2, AddressingMode.None),
        Code.init(0x18, CodeType.CLC, 2, AddressingMode.None),
        Code.init(0xD8, CodeType.CLD, 2, AddressingMode.None),
        Code.init(0x58, CodeType.CLI, 2, AddressingMode.None),
        Code.init(0xB8, CodeType.CLV, 2, AddressingMode.None),
        Code.init(0xC9, CodeType.CMP, 2, AddressingMode.Immediate),
        Code.init(0xC5, CodeType.CMP, 2, AddressingMode.ZeroPage),
        Code.init(0xD5, CodeType.CMP, 4, AddressingMode.ZeroPageX),
        Code.init(0xCD, CodeType.CMP, 4, AddressingMode.Absolute),
        Code.init(0xDD, CodeType.CMP, 4, AddressingMode.AbsoluteX),
        Code.init(0xD9, CodeType.CMP, 4, AddressingMode.AbsoluteY),
        Code.init(0xC1, CodeType.CMP, 6, AddressingMode.IndirectX),
        Code.init(0xD1, CodeType.CMP, 5, AddressingMode.IndirectY),

        Code.init(0xE0, CodeType.CPX, 2, AddressingMode.Immediate),
        Code.init(0xE4, CodeType.CPX, 3, AddressingMode.ZeroPage),
        Code.init(0xEC, CodeType.CPX, 4, AddressingMode.Absolute),
        Code.init(0xC0, CodeType.CPY, 2, AddressingMode.Immediate),
        Code.init(0xC4, CodeType.CPY, 3, AddressingMode.ZeroPage),
        Code.init(0xCC, CodeType.CPY, 4, AddressingMode.Absolute),

        Code.init(0xC6, CodeType.DEC, 5, AddressingMode.ZeroPage),
        Code.init(0xD6, CodeType.DEC, 6, AddressingMode.ZeroPageX),
        Code.init(0xCE, CodeType.DEC, 6, AddressingMode.Absolute),
        Code.init(0xDE, CodeType.DEC, 7, AddressingMode.AbsoluteX),

        Code.init(0xCA, CodeType.DEX, 2, AddressingMode.None),
        Code.init(0x88, CodeType.DEY, 2, AddressingMode.None),

        Code.init(0x49, CodeType.EOR, 2, AddressingMode.Immediate),
        Code.init(0x45, CodeType.EOR, 3, AddressingMode.ZeroPage),
        Code.init(0x55, CodeType.EOR, 4, AddressingMode.ZeroPageX),
        Code.init(0x4D, CodeType.EOR, 4, AddressingMode.Absolute),
        Code.init(0x5D, CodeType.EOR, 4, AddressingMode.AbsoluteX),
        Code.init(0x59, CodeType.EOR, 4, AddressingMode.AbsoluteY),
        Code.init(0x41, CodeType.EOR, 6, AddressingMode.IndirectX),
        Code.init(0x51, CodeType.EOR, 5, AddressingMode.IndirectY),

        Code.init(0xE6, CodeType.INC, 5, AddressingMode.ZeroPage),
        Code.init(0xF6, CodeType.INC, 6, AddressingMode.ZeroPageX),
        Code.init(0xEE, CodeType.INC, 6, AddressingMode.Absolute),
        Code.init(0xFE, CodeType.INC, 7, AddressingMode.AbsoluteX),

        Code.init(0xE8, CodeType.INX, 2, AddressingMode.None),
        Code.init(0xC8, CodeType.INY, 2, AddressingMode.None),

        Code.init(0x4C, CodeType.JMP, 3, AddressingMode.Absolute),
        // Code.init(0x6C, CodeType.JMP, 5, AddressingMode.Indirect),

        Code.init(0x20, CodeType.JSR, 6, AddressingMode.Absolute),

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
        Code.init(0x4A, CodeType.LSR, 2, AddressingMode.None), //Accumulator
        Code.init(0x46, CodeType.LSR, 5, AddressingMode.ZeroPage),
        Code.init(0x56, CodeType.LSR, 6, AddressingMode.ZeroPageX),
        Code.init(0x4E, CodeType.LSR, 6, AddressingMode.Absolute),
        Code.init(0x5E, CodeType.LSR, 7, AddressingMode.AbsoluteX),

        Code.init(0xEA, CodeType.NOP, 2, AddressingMode.None),

        Code.init(0x09, CodeType.ORA, 2, AddressingMode.Immediate),
        Code.init(0x05, CodeType.ORA, 3, AddressingMode.ZeroPage),
        Code.init(0x15, CodeType.ORA, 4, AddressingMode.ZeroPageX),
        Code.init(0x0D, CodeType.ORA, 4, AddressingMode.Absolute),
        Code.init(0x1D, CodeType.ORA, 4, AddressingMode.AbsoluteX),
        Code.init(0x19, CodeType.ORA, 4, AddressingMode.AbsoluteY),
        Code.init(0x01, CodeType.ORA, 6, AddressingMode.IndirectX),
        Code.init(0x11, CodeType.ORA, 5, AddressingMode.IndirectY),
        Code.init(0x48, CodeType.PHA, 3, AddressingMode.None), //implied
        Code.init(0x08, CodeType.PHP, 3, AddressingMode.None), //implied
        Code.init(0x68, CodeType.PLA, 4, AddressingMode.None), //implied
        Code.init(0x28, CodeType.PLP, 4, AddressingMode.None), //implied
        Code.init(0x2A, CodeType.ROL, 2, AddressingMode.None), // Accumulator
        Code.init(0x26, CodeType.ROL, 5, AddressingMode.ZeroPage),
        Code.init(0x36, CodeType.ROL, 6, AddressingMode.ZeroPageX),
        Code.init(0x2E, CodeType.ROL, 6, AddressingMode.Absolute),
        Code.init(0x3E, CodeType.ROL, 7, AddressingMode.AbsoluteX),
        Code.init(0x6A, CodeType.ROR, 2, AddressingMode.None), // Accumulator
        Code.init(0x66, CodeType.ROR, 5, AddressingMode.ZeroPage),
        Code.init(0x75, CodeType.ROR, 6, AddressingMode.ZeroPageX),
        Code.init(0x6E, CodeType.ROR, 6, AddressingMode.Absolute),
        Code.init(0x7E, CodeType.ROR, 7, AddressingMode.AbsoluteX),
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

    var ordered_codes = [_]Code{Code.init(0x00, CodeType.NOP, 0, AddressingMode.None)} ** std.math.maxInt(u8);
    for (codes) |code| {
        ordered_codes[code.opecode] = code;
    }
    break :init ordered_codes;
};

pub const AddressingMode = enum {
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

pub const CodeType = enum {
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
