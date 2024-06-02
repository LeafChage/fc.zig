const std = @import("std");
const Mirroring = @import("./mirroring.zig").Mirroring;

pub const Rom = struct {
    // 0x8000 ~ 0xFFFF addressing space for ProgramROM in CPU Memory Map
    const ProgramRomSize: usize = 0xFFFF - 0x8000;

    const NesTag: [4]u8 = [_]u8{ 'N', 'E', 'S', 0x1a };

    const ProgramRomPageSize: usize = 0x4000;
    const CharacterRomPageSize: usize = 0x2000;

    program_rom: [ProgramRomSize]u8,
    // character_rom: []const u8,
    mapper: u8,
    screen_mirroring: Mirroring,

    const Self = @This();
    pub fn init(raw: []const u8) error{ NotInesFormat, NotSupportedVersion }!Self {
        if (!std.mem.eql(u8, raw[0..4], &NesTag)) {
            return error.NotInesFormat;
        }

        const mapper = (raw[7] & 0b1111_1000) | (raw[6] >> 4);
        const ines_ver = (raw[7] >> 2) & 0b11;
        if (ines_ver != 0) {
            return error.NotSupportedVersion;
        }

        const four_screen = raw[6] & 0b1000 != 0;
        const vertical_mirroring = raw[6] & 0b1 != 0;
        const screen_mirroring = if (four_screen) Mirroring.FourScreen else if (vertical_mirroring) Mirroring.Vertical else Mirroring.Horizontal;
        const program_rom_size = raw[4] * ProgramRomPageSize;
        // const character_rom_size =raw[5] *  CharacterRomPageSize;

        const skip_trainer = raw[6] & 0b100 != 0;
        const program_rom_start = 16 + if (skip_trainer) 512 else @as(usize, 0);
        // const character_rom_start = program_rom_start + program_rom_size;

        var program_rom = [_]u8{0} ** ProgramRomSize;
        @memcpy(program_rom[0..program_rom_size], raw[program_rom_start..(program_rom_start + program_rom_size)]);
        return Rom{
            .program_rom = program_rom,
            // .character_rom = raw[character_rom_start..(character_rom_start + character_rom_size)],
            .mapper = mapper,
            .screen_mirroring = screen_mirroring,
        };
    }
};

pub fn testRom(programs: []const u8) Rom {
    _ = std.testing.expect;
    var program_rom = [_]u8{0} ** Rom.ProgramRomSize;
    const length = programs.len;
    @memcpy(program_rom[0..length], programs[0..length]);
    return Rom{
        .program_rom = program_rom,
        .mapper = 0,
        .screen_mirroring = Mirroring.FourScreen,
    };
}
