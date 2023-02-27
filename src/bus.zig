const std = @import("std");
const Mos6502 = @import("6502.zig");

/// CPU Device on Bus
cpu: Mos6502 = undefined,

/// Memory (RAM) 64kb or 0xFFFF
ram: [0xFFFF]u8 = undefined,

const Self = @This();

pub fn init(self: *Self) !void {
    self.cpu.bus = self;
    try self.cpu.init();
}

pub fn deinit(self: *Self) void {
    self.cpu.deinit();
}

pub fn write(self: *Self, addr: u16, data: u8) void {
    if (addr >= 0x0000 and addr <= 0xFFFF)
        self.ram[addr] = data;
}

pub fn read(self: *Self, addr: u16, readOnly: bool) u8 {
    _ = readOnly;
    return if (addr >= 0x0000 and addr <= 0xFFFF) self.ram[addr] else 0xAA;
}
