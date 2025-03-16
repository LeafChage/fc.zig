const sdl = @cImport(@cInclude("SDL3/SDL.h"));

pub const Color = struct {
    raw: sdl.SDL_Color,
    pub const black = color(0, 0, 0);
    pub const white = color(255, 255, 255);
    pub const red = color(255, 0, 0);
    pub const green = color(0, 255, 0);
    pub const blue = color(0, 0, 255);
    pub const gray = color(125, 125, 125);
    pub const magenta = color(255, 0, 255);
    pub const yellow = color(255, 255, 0);
    pub const cyan = color(0, 255, 255);

    const Self = @This();
    pub fn color(r: u8, g: u8, b: u8) Self {
        return Self{ .raw = sdl.SDL_Color{
            .r = r,
            .g = g,
            .b = b,
        } };
    }

    pub fn rgb(self: Self) [3]u8 {
        return [_]u8{
            self.raw.r,
            self.raw.g,
            self.raw.b,
        };
    }
};
