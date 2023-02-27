const std = @import("std");
const sdl = @cImport(@cInclude("SDL.h"));
const System = @import("bus.zig");

var window: ?*sdl.SDL_Window = null;
var renderer: ?*sdl.SDL_Renderer = null;

pub fn init() !void {
    std.debug.print("ZeNES Starting!\n", .{});

    if (sdl.SDL_Init(sdl.SDL_INIT_EVERYTHING) < 0) {
        @panic("SDL Initialization Failed!");
    }

    window = sdl.SDL_CreateWindow("ZeNES", sdl.SDL_WINDOWPOS_CENTERED, sdl.SDL_WINDOWPOS_CENTERED, 768, 720, 0);
    if (window == null) {
        @panic("SDL Window Creation Failed!");
    }

    renderer = sdl.SDL_CreateRenderer(window, -1, 0);

    if (renderer == null) {
        var err = sdl.SDL_GetError();
        std.debug.print("{s}\n", .{err});

        @panic("SDL Renderer Initialization Failed!");
    }
}

pub fn deinit() void {
    sdl.SDL_DestroyWindow(window);
    window = null;

    sdl.SDL_Quit();
    std.debug.print("ZeNES Quitting!\n", .{});
}

pub fn main() !void {
    try init();
    defer deinit();

    var sys: System = undefined;
    try sys.init();

    var keep_open = true;
    while (keep_open) {
        var e: sdl.SDL_Event = undefined;
        while (sdl.SDL_PollEvent(&e) > 0) {
            switch (e.type) {
                sdl.SDL_QUIT => keep_open = false,
                else => {},
            }
        }

        _ = sdl.SDL_RenderClear(renderer);
        _ = sdl.SDL_RenderPresent(renderer);
    }
}
