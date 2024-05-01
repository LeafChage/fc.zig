const std = @import("std");
const sdl = @cImport(@cInclude("SDL2/SDL.h"));
const mysdl = @import("./sdl.zig");
const CPU = @import("./cpu.zig").CPU;
const CPUCallback = @import("./cpu.zig").CPUCallback;

const game_code = [_]u8{
    0x20, 0x06, 0x06, 0x20, 0x38, 0x06, 0x20, 0x0d, 0x06, 0x20, 0x2a, 0x06, 0x60, 0xa9, 0x02, 0x85,
    0x02, 0xa9, 0x04, 0x85, 0x03, 0xa9, 0x11, 0x85, 0x10, 0xa9, 0x10, 0x85, 0x12, 0xa9, 0x0f, 0x85,
    0x14, 0xa9, 0x04, 0x85, 0x11, 0x85, 0x13, 0x85, 0x15, 0x60, 0xa5, 0xfe, 0x85, 0x00, 0xa5, 0xfe,
    0x29, 0x03, 0x18, 0x69, 0x02, 0x85, 0x01, 0x60, 0x20, 0x4d, 0x06, 0x20, 0x8d, 0x06, 0x20, 0xc3,
    0x06, 0x20, 0x19, 0x07, 0x20, 0x20, 0x07, 0x20, 0x2d, 0x07, 0x4c, 0x38, 0x06, 0xa5, 0xff, 0xc9,
    0x77, 0xf0, 0x0d, 0xc9, 0x64, 0xf0, 0x14, 0xc9, 0x73, 0xf0, 0x1b, 0xc9, 0x61, 0xf0, 0x22, 0x60,
    0xa9, 0x04, 0x24, 0x02, 0xd0, 0x26, 0xa9, 0x01, 0x85, 0x02, 0x60, 0xa9, 0x08, 0x24, 0x02, 0xd0,
    0x1b, 0xa9, 0x02, 0x85, 0x02, 0x60, 0xa9, 0x01, 0x24, 0x02, 0xd0, 0x10, 0xa9, 0x04, 0x85, 0x02,
    0x60, 0xa9, 0x02, 0x24, 0x02, 0xd0, 0x05, 0xa9, 0x08, 0x85, 0x02, 0x60, 0x60, 0x20, 0x94, 0x06,
    0x20, 0xa8, 0x06, 0x60, 0xa5, 0x00, 0xc5, 0x10, 0xd0, 0x0d, 0xa5, 0x01, 0xc5, 0x11, 0xd0, 0x07,
    0xe6, 0x03, 0xe6, 0x03, 0x20, 0x2a, 0x06, 0x60, 0xa2, 0x02, 0xb5, 0x10, 0xc5, 0x10, 0xd0, 0x06,
    0xb5, 0x11, 0xc5, 0x11, 0xf0, 0x09, 0xe8, 0xe8, 0xe4, 0x03, 0xf0, 0x06, 0x4c, 0xaa, 0x06, 0x4c,
    0x35, 0x07, 0x60, 0xa6, 0x03, 0xca, 0x8a, 0xb5, 0x10, 0x95, 0x12, 0xca, 0x10, 0xf9, 0xa5, 0x02,
    0x4a, 0xb0, 0x09, 0x4a, 0xb0, 0x19, 0x4a, 0xb0, 0x1f, 0x4a, 0xb0, 0x2f, 0xa5, 0x10, 0x38, 0xe9,
    0x20, 0x85, 0x10, 0x90, 0x01, 0x60, 0xc6, 0x11, 0xa9, 0x01, 0xc5, 0x11, 0xf0, 0x28, 0x60, 0xe6,
    0x10, 0xa9, 0x1f, 0x24, 0x10, 0xf0, 0x1f, 0x60, 0xa5, 0x10, 0x18, 0x69, 0x20, 0x85, 0x10, 0xb0,
    0x01, 0x60, 0xe6, 0x11, 0xa9, 0x06, 0xc5, 0x11, 0xf0, 0x0c, 0x60, 0xc6, 0x10, 0xa5, 0x10, 0x29,
    0x1f, 0xc9, 0x1f, 0xf0, 0x01, 0x60, 0x4c, 0x35, 0x07, 0xa0, 0x00, 0xa5, 0xfe, 0x91, 0x00, 0x60,
    0xa6, 0x03, 0xa9, 0x00, 0x81, 0x10, 0xa2, 0x00, 0xa9, 0x01, 0x81, 0x10, 0x60, 0xa2, 0x00, 0xea,
    0xea, 0xca, 0xd0, 0xfb, 0x60,
};

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

            cpu.mem_write(0xFE, 15);
            if (read_screen_state(cpu, &self.state)) {
                if (sdl.SDL_UpdateTexture(self.texture, null, &self.state, 32 * 3) == 0 and
                    sdl.SDL_RenderCopy(self.renderer, self.texture, null, null) == 0)
                {
                    sdl.SDL_RenderPresent(self.renderer);
                }
            }
            sdl.SDL_Delay(170);
        }

        pub fn callback(self: *@This()) CPUCallback {
            return CPUCallback{ .ptr = @ptrCast(self), .impl = &.{ .call = call } };
        }
    };
    var game = Game.init(renderer, texture);
    var cpu = CPU.init();
    cpu.load(&game_code);
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

    // // rendering test
    // frame_idx = 0;
    // for (0x0200..0x0600) |_| {
    //     const color_idx = std.crypto.random.intRangeAtMost(u8, 0, 14);
    //     const rgb = color(color_idx).rgb();
    //     frame[frame_idx] = rgb[0];
    //     frame[frame_idx + 1] = rgb[1];
    //     frame[frame_idx + 2] = rgb[2];
    //     update = true;
    //     frame_idx += 3;
    // }

    return update;
}
