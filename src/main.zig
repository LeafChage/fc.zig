const std = @import("std");
const sdl = @cImport(@cInclude("SDL2/SDL.h"));
const mysdl = @import("./sdl.zig");
const CPU = @import("./cpu.zig").CPU;
const CPUCallback = @import("./cpu.zig").CPUCallback;
const Bus = @import("./bus.zig").Bus;
const Rom = @import("./rom.zig").Rom;

pub fn main() !void {
    if (sdl.SDL_Init(sdl.SDL_INIT_EVERYTHING) != 0) {
        sdl.SDL_Log("Unable to initialize SDL: %s", sdl.SDL_GetError());
        return error.SDLInitializationFailed;
    }
    defer sdl.SDL_Quit();

    const window = sdl.SDL_CreateWindow("Game", sdl.SDL_WINDOWPOS_CENTERED, sdl.SDL_WINDOWPOS_CENTERED, 32 * 10, 32 * 10, 0) orelse {
        sdl.SDL_Log("Unable to initialize window: %s", sdl.SDL_GetError());
        return error.SDLInitializationFailed;
    };
    defer sdl.SDL_DestroyWindow(window);

    const renderer = sdl.SDL_CreateRenderer(window, -1, sdl.SDL_RENDERER_ACCELERATED) orelse {
        sdl.SDL_Log("Unable to initialize renderer: %s", sdl.SDL_GetError());
        return error.SDLInitializationFailed;
    };
    defer sdl.SDL_DestroyRenderer(renderer);
    const texture = sdl.SDL_CreateTexture(renderer, sdl.SDL_PIXELFORMAT_RGB24, sdl.SDL_TEXTUREACCESS_STREAMING, 32, 32) orelse {
        sdl.SDL_Log("Unable to initialize renderer: %s", sdl.SDL_GetError());
        return error.SDLInitializationFailed;
    };
    defer sdl.SDL_DestroyTexture(texture);

    const Game = struct {
        state: [(32 * 3 * 32)]u8,
        renderer: ?*sdl.SDL_Renderer,
        texture: ?*sdl.SDL_Texture,

        pub fn init(
            r: ?*sdl.SDL_Renderer,
            t: ?*sdl.SDL_Texture,
        ) @This() {
            return @This(){
                .state = [_]u8{0} ** (32 * 3 * 32),
                .renderer = r,
                .texture = t,
            };
        }

        pub fn call(ctx: *anyopaque, cpu: *CPU) void {
            const self: *@This() = @ptrCast(@alignCast(ctx));
            handle_user_input(cpu);

            std.log.debug("{s}", .{trace(cpu.*)});
            cpu.mem_write(0xFE, std.crypto.random.intRangeAtMost(u8, 1, 16));
            if (read_screen_state(cpu, &self.state)) {
                if (sdl.SDL_UpdateTexture(self.texture, null, &self.state, 32 * 3) == 0 and
                    sdl.SDL_RenderCopy(self.renderer, self.texture, null, null) == 0)
                {
                    sdl.SDL_RenderPresent(self.renderer);
                }
            }
            std.time.sleep(70000);
        }

        pub fn callback(self: *@This()) CPUCallback {
            return CPUCallback{ .ptr = @ptrCast(self), .impl = &.{ .call = call } };
        }
    };
    var game = Game.init(renderer, texture);
    const buffer = @embedFile("./nestest.nes");
    const rom = try Rom.init(buffer);
    var bus = Bus.init(rom);
    const mem = bus.memory();
    var cpu = CPU.init(mem);
    cpu.reset();
    cpu.run_with_callback(game.callback());
}

fn handle_user_input(cpu: *CPU) void {
    var event: sdl.SDL_Event = undefined;
    if (sdl.SDL_PollEvent(&event) != 0) {
        switch (event.type) {
            sdl.SDL_QUIT => {
                std.process.exit(0);
            },
            sdl.SDL_KEYDOWN => {
                switch (event.key.keysym.sym) {
                    sdl.SDLK_ESCAPE => {
                        std.log.debug("keydown escape", .{});
                        std.process.exit(0);
                    },
                    sdl.SDLK_w => {
                        std.log.debug("keydown w", .{});
                        cpu.mem_write(0xff, 0x77);
                    },
                    sdl.SDLK_s => {
                        std.log.debug("keydown s", .{});
                        cpu.mem_write(0xff, 0x73);
                    },
                    sdl.SDLK_a => {
                        std.log.debug("keydown a", .{});
                        cpu.mem_write(0xff, 0x61);
                    },
                    sdl.SDLK_d => {
                        std.log.debug("keydown d", .{});
                        cpu.mem_write(0xff, 0x64);
                    },
                    else => {},
                }
            },
            else => {},
        }
    }
}

fn color(byte: u8) mysdl.Color {
    return switch (byte) {
        0 => mysdl.Color.black,
        1 => mysdl.Color.white,
        2, 9 => mysdl.Color.gray,
        3, 10 => mysdl.Color.red,
        4, 11 => mysdl.Color.green,
        5, 12 => mysdl.Color.blue,
        6, 13 => mysdl.Color.magenta,
        7, 14 => mysdl.Color.yellow,
        else => mysdl.Color.cyan,
    };
}

fn read_screen_state(cpu: *CPU, frame: *[32 * 3 * 32]u8) bool {
    var frame_idx: usize = 0;
    var update = false;
    for (0x0200..0x0600) |i| {
        const color_idx = cpu.mem_read(@intCast(i));
        const rgb = color(color_idx).rgb();
        if (frame[frame_idx] != rgb[0] or
            frame[frame_idx + 1] != rgb[1] or
            frame[frame_idx + 2] != rgb[2])
        {
            frame[frame_idx] = rgb[0];
            frame[frame_idx + 1] = rgb[1];
            frame[frame_idx + 2] = rgb[2];
            update = true;
        }
        frame_idx += 3;
    }
    return update;
}

fn trace(cpu: CPU) []const u8 {
    var buffer: [200]u8 = undefined;
    return std.fmt.bufPrint(&buffer, "{}", .{cpu}) catch unreachable;
}
//
// const DebugTrace = struct {
//     const Self = @This();
//     logs: std.ArrayList([]const u8),
//
//     pub fn init(alloc: std.mem.Allocator) Self {
//         return DebugTrace{
//             .logs = std.ArrayList([]const u8).init(alloc),
//         };
//     }
//
//     pub fn deinit(self: *Self) void {
//         self.logs.deinit();
//     }
//
//     pub fn result(self: Self) [][]const u8 {
//         return self.logs.items;
//     }
//
//     pub fn call(ctx: *anyopaque, cpu: *CPU) void {
//         const self: *@This() = @ptrCast(@alignCast(ctx));
//         const log = trace(cpu.*);
//         std.log.err("{s}", .{log});
//         self.logs.append(log) catch unreachable;
//     }
//
//     pub fn callback(self: *@This()) CPUCallback {
//         return CPUCallback{ .ptr = @ptrCast(self), .impl = &.{ .call = call } };
//     }
// };
//
// test "test format trace" {
//     var debug = DebugTrace.init(std.testing.allocator);
//     defer debug.deinit();
//
//     const buffer = @embedFile("./nestest.nes");
//     const rom = try Rom.init(buffer);
//     var bus = Bus.init(rom);
//     var mem = bus.memory();
//     mem.mem_write(100, 0xa2);
//     mem.mem_write(101, 0x01);
//     mem.mem_write(102, 0xca);
//     mem.mem_write(103, 0x88);
//     mem.mem_write(104, 0x00);
//     var cpu = CPU.init(mem);
//     cpu.program_counter = 0x64;
//     cpu.register_a = 1;
//     cpu.register_x = 2;
//     cpu.register_y = 3;
//     cpu.run_with_callback(debug.callback());
//
//     const result = debug.result();
//     try std.testing.expectEqual("0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD", result[0]);
//     try std.testing.expectEqual("0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD", result[1]);
//     try std.testing.expectEqual("0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD", result[2]);
// }
//
// test "test format mem access" {
//     var debug = DebugTrace.init(std.testing.allocator);
//     defer debug.deinit();
//
//     const buffer = @embedFile("./nestest.nes");
//     const rom = try Rom.init(buffer);
//     var bus = Bus.init(rom);
//     var mem = bus.memory();
//     mem.mem_write(100, 0x11);
//     mem.mem_write(101, 0x33);
//
//     mem.mem_write(0x33, 0x00);
//     mem.mem_write(0x34, 0x04);
//
//     mem.mem_write(0x400, 0xAA);
//     var cpu = CPU.init(mem);
//     cpu.program_counter = 0x64;
//     cpu.register_y = 0;
//     cpu.run_with_callback(debug.callback());
//
//     const result = debug.result();
//     try std.testing.expectEqual("0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD", result[0]);
// }
