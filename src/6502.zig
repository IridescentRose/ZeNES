const std = @import("std");
const Bus = @import("bus.zig");

const Self = @This();

/// Accumulator Register
rA: u8 = 0,

/// X Register
rX: u8 = 0,

/// Y Register
rY: u8 = 0,

/// Stack Pointer (SP)
sp: u8 = 0,

/// Instruction Pointer (IP)
ip: u16 = 0,

/// Flag register
status: u8 = 0,

/// Reference to the system Bus
bus: *Bus = undefined,

/// Byte fetched from memory
byte_fetched: u8 = 0x00,

/// Absolute address
addr_absolute: u16 = 0x0000,

/// Relative address
addr_relative: u16 = 0x0000,

/// Opcode
opcode: u8 = 0x00,

/// Remaining cycles in operation
cycles: u8 = 0x00,

instruction_table: [256]Instruction = undefined,

/// Flags Enum
const Flags = enum(u3) {
    C = 0, // Carry Flag
    Z = 1, // Zero Flag
    I = 2, // Disable Interrupt Flag
    D = 3, // Decimal Mode Flag
    B = 4, // Break Flag
    U = 5, // Unused
    V = 6, // Overflow Flag
    N = 7, // Negative Flag
};

/// Instruction Enum
const Instruction = struct {
    name: []const u8,
    operation: Operation,
    address_mode: AddressMode,
    cycles: u8 = 0,
};

/// Operations Enum
const Operation = enum(u8) { ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS, CLC, CLD, CLI, CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP, JSR, LDA, LDX, LDY, LSR, NOP, ORA, PHA, PHP, PLA, PLP, ROL, ROR, RTI, RTS, SBC, SEC, SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, TXS, TYA, XXX };

/// Address Mode Enum
const AddressMode = enum(u8) { IMP, IMM, ZP0, ZPX, ZPY, REL, ABS, ABX, ABY, IND, IZX, IZY, XXX };

// Implied
fn IMP(self: *Self) u8 {
    self.byte_fetched = self.rA;
    return 0;
}

// Immediate (next byte = value)
fn IMM(self: *Self) u8 {
    self.addr_absolute = self.ip;
    self.ip += 1;
    return 0;
}

//Zero Page (absolute address in first 0xFF of range)
fn ZP0(self: *Self) u8 {
    self.addr_absolute = self.read(self.ip, false);
    self.ip += 1;
    self.addr_absolute &= 0x00FF;

    return 0;
}

// Zero Page with X Offset
fn ZPX(self: *Self) u8 {
    self.addr_absolute = (self.read(self.ip, false) + self.rX);
    self.ip += 1;
    self.addr_absolute &= 0x00FF;
    return 0;
}

// Zero Page with Y Offset
fn ZPY(self: *Self) u8 {
    self.addr_absolute = (self.read(self.ip, false) + self.rY);
    self.ip += 1;
    self.addr_absolute &= 0x00FF;
    return 0;
}

//Relative Branch
fn REL(self: *Self) u8 {
    self.addr_relative = self.read(self.ip, false);
    self.ip += 1;

    if (self.addr_relative & 0x80)
        self.addr_relative |= 0xFF00;

    return 0;
}

//Absolute
fn ABS(self: *Self) u8 {
    var lo: u16 = self.read(self.ip, false);
    self.ip += 1;
    var hi: u16 = self.read(self.ip, false);
    self.ip += 1;

    self.addr_absolute = (hi << 8) | (lo & 0xFF);

    return 0;
}

// Absolute with X Offset
fn ABX(self: *Self) u8 {
    var lo: u16 = self.read(self.ip, false);
    self.ip += 1;
    var hi: u16 = self.read(self.ip, false);
    self.ip += 1;

    self.addr_absolute = (hi << 8) | (lo & 0xFF);
    self.addr_absolute += self.rX;

    return if ((self.addr_absolute & 0xFF00) != (hi << 8)) 1 else 0;
}

// Absolute with Y Offset
fn ABY(self: *Self) u8 {
    var lo: u16 = self.read(self.ip, false);
    self.ip += 1;
    var hi: u16 = self.read(self.ip, false);
    self.ip += 1;

    self.addr_absolute = (hi << 8) | (lo & 0xFF);
    self.addr_absolute += self.rY;

    return if ((self.addr_absolute & 0xFF00) != (hi << 8)) 1 else 0;
}

// Indirect
fn IND(self: *Self) u8 {
    var ptr_lo: u16 = self.read(self.ip, false);
    self.ip += 1;
    var ptr_hi: u16 = self.read(self.ip, false);
    self.ip += 1;

    var ptr = (ptr_hi << 8) | (ptr_lo & 0xFF);

    if (ptr_lo == 0x00FF) // Simulate page boundary hardware bug
    {
        self.addr_absolute = (self.read(ptr & 0xFF00, false) << 8) | self.read(ptr + 0, false);
    } else // Behave normally
    {
        self.addr_absolute = (self.read(ptr + 1, false) << 8) | self.read(ptr + 0, false);
    }

    return 0;
}

//Indirect X
fn IZX(self: *Self) u8 {
    var t: u16 = self.read(self.ip, false);
    self.ip += 1;

    var lo: u16 = self.read((t + self.rX) & 0x00FF, false);
    var hi: u16 = self.read((t + self.rX + 1) & 0x00FF, false);

    self.addr_absolute = (hi << 8) | (lo & 0xFF);

    return 0;
}

//Indirect Y
fn IZY(self: *Self) u8 {
    var t: u16 = self.read(self.ip, false);
    self.ip += 1;

    var lo: u16 = self.read(t & 0x00FF, false);
    var hi: u16 = self.read((t + 1) & 0x00FF, false);

    self.addr_absolute = (hi << 8) | (lo & 0xFF);
    self.addr_absolute += self.rY;

    return if ((self.addr_absolute & 0xFF00) != (hi << 8)) 1 else 0;
}

pub fn init(self: *Self) !void {
    self.instruction_table = .{
        Instruction{ .name = "BRK", .operation = Operation.BRK, .address_mode = AddressMode.IMM, .cycles = 7 }, Instruction{ .name = "ORA", .operation = Operation.ORA, .address_mode = AddressMode.IZX, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 8 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 3 }, Instruction{ .name = "ORA", .operation = Operation.ORA, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "ASL", .operation = Operation.ASL, .address_mode = AddressMode.ZP0, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 5 }, Instruction{ .name = "PHP", .operation = Operation.PHP, .address_mode = AddressMode.IMP, .cycles = 3 }, Instruction{ .name = "ORA", .operation = Operation.ORA, .address_mode = AddressMode.IMM, .cycles = 2 }, Instruction{ .name = "ASL", .operation = Operation.ASL, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "ORA", .operation = Operation.ORA, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "ASL", .operation = Operation.ASL, .address_mode = AddressMode.ABS, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 },
        Instruction{ .name = "BPL", .operation = Operation.BPL, .address_mode = AddressMode.REL, .cycles = 2 }, Instruction{ .name = "ORA", .operation = Operation.ORA, .address_mode = AddressMode.IZY, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 8 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "ORA", .operation = Operation.ORA, .address_mode = AddressMode.ZPX, .cycles = 4 }, Instruction{ .name = "ASL", .operation = Operation.ASL, .address_mode = AddressMode.ZPX, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 }, Instruction{ .name = "CLC", .operation = Operation.CLC, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "ORA", .operation = Operation.ORA, .address_mode = AddressMode.ABY, .cycles = 4 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 7 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "ORA", .operation = Operation.ORA, .address_mode = AddressMode.ABX, .cycles = 4 }, Instruction{ .name = "ASL", .operation = Operation.ASL, .address_mode = AddressMode.ABX, .cycles = 7 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 7 },
        Instruction{ .name = "JSR", .operation = Operation.JSR, .address_mode = AddressMode.ABS, .cycles = 6 }, Instruction{ .name = "AND", .operation = Operation.AND, .address_mode = AddressMode.IZX, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 8 }, Instruction{ .name = "BIT", .operation = Operation.BIT, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "AND", .operation = Operation.AND, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "ROL", .operation = Operation.ROL, .address_mode = AddressMode.ZP0, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 5 }, Instruction{ .name = "PLP", .operation = Operation.PLP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "AND", .operation = Operation.AND, .address_mode = AddressMode.IMM, .cycles = 2 }, Instruction{ .name = "ROL", .operation = Operation.ROL, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "BIT", .operation = Operation.BIT, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "AND", .operation = Operation.AND, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "ROL", .operation = Operation.ROL, .address_mode = AddressMode.ABS, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 },
        Instruction{ .name = "BMI", .operation = Operation.BMI, .address_mode = AddressMode.REL, .cycles = 2 }, Instruction{ .name = "AND", .operation = Operation.AND, .address_mode = AddressMode.IZY, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 8 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "AND", .operation = Operation.AND, .address_mode = AddressMode.ZPX, .cycles = 4 }, Instruction{ .name = "ROL", .operation = Operation.ROL, .address_mode = AddressMode.ZPX, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 }, Instruction{ .name = "SEC", .operation = Operation.SEC, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "AND", .operation = Operation.AND, .address_mode = AddressMode.ABY, .cycles = 4 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 7 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "AND", .operation = Operation.AND, .address_mode = AddressMode.ABX, .cycles = 4 }, Instruction{ .name = "ROL", .operation = Operation.ROL, .address_mode = AddressMode.ABX, .cycles = 7 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 7 },
        Instruction{ .name = "RTI", .operation = Operation.RTI, .address_mode = AddressMode.IMP, .cycles = 6 }, Instruction{ .name = "EOR", .operation = Operation.EOR, .address_mode = AddressMode.IZX, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 8 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 3 }, Instruction{ .name = "EOR", .operation = Operation.EOR, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "LSR", .operation = Operation.LSR, .address_mode = AddressMode.ZP0, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 5 }, Instruction{ .name = "PHA", .operation = Operation.PHA, .address_mode = AddressMode.IMP, .cycles = 3 }, Instruction{ .name = "EOR", .operation = Operation.EOR, .address_mode = AddressMode.IMM, .cycles = 2 }, Instruction{ .name = "LSR", .operation = Operation.LSR, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "JMP", .operation = Operation.JMP, .address_mode = AddressMode.ABS, .cycles = 3 }, Instruction{ .name = "EOR", .operation = Operation.EOR, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "LSR", .operation = Operation.LSR, .address_mode = AddressMode.ABS, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 },
        Instruction{ .name = "BVC", .operation = Operation.BVC, .address_mode = AddressMode.REL, .cycles = 2 }, Instruction{ .name = "EOR", .operation = Operation.EOR, .address_mode = AddressMode.IZY, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 8 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "EOR", .operation = Operation.EOR, .address_mode = AddressMode.ZPX, .cycles = 4 }, Instruction{ .name = "LSR", .operation = Operation.LSR, .address_mode = AddressMode.ZPX, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 }, Instruction{ .name = "CLI", .operation = Operation.CLI, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "EOR", .operation = Operation.EOR, .address_mode = AddressMode.ABY, .cycles = 4 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 7 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "EOR", .operation = Operation.EOR, .address_mode = AddressMode.ABX, .cycles = 4 }, Instruction{ .name = "LSR", .operation = Operation.LSR, .address_mode = AddressMode.ABX, .cycles = 7 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 7 },
        Instruction{ .name = "RTS", .operation = Operation.RTS, .address_mode = AddressMode.IMP, .cycles = 6 }, Instruction{ .name = "ADC", .operation = Operation.ADC, .address_mode = AddressMode.IZX, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 8 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 3 }, Instruction{ .name = "ADC", .operation = Operation.ADC, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "ROR", .operation = Operation.ROR, .address_mode = AddressMode.ZP0, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 5 }, Instruction{ .name = "PLA", .operation = Operation.PLA, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "ADC", .operation = Operation.ADC, .address_mode = AddressMode.IMM, .cycles = 2 }, Instruction{ .name = "ROR", .operation = Operation.ROR, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "JMP", .operation = Operation.JMP, .address_mode = AddressMode.IND, .cycles = 5 }, Instruction{ .name = "ADC", .operation = Operation.ADC, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "ROR", .operation = Operation.ROR, .address_mode = AddressMode.ABS, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 },
        Instruction{ .name = "BVS", .operation = Operation.BVS, .address_mode = AddressMode.REL, .cycles = 2 }, Instruction{ .name = "ADC", .operation = Operation.ADC, .address_mode = AddressMode.IZY, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 8 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "ADC", .operation = Operation.ADC, .address_mode = AddressMode.ZPX, .cycles = 4 }, Instruction{ .name = "ROR", .operation = Operation.ROR, .address_mode = AddressMode.ZPX, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 }, Instruction{ .name = "SEI", .operation = Operation.SEI, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "ADC", .operation = Operation.ADC, .address_mode = AddressMode.ABY, .cycles = 4 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 7 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "ADC", .operation = Operation.ADC, .address_mode = AddressMode.ABX, .cycles = 4 }, Instruction{ .name = "ROR", .operation = Operation.ROR, .address_mode = AddressMode.ABX, .cycles = 7 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 7 },
        Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "STA", .operation = Operation.STA, .address_mode = AddressMode.IZX, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 }, Instruction{ .name = "STY", .operation = Operation.STY, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "STA", .operation = Operation.STA, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "STX", .operation = Operation.STX, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 3 }, Instruction{ .name = "DEY", .operation = Operation.DEY, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "TXA", .operation = Operation.TXA, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "STY", .operation = Operation.STY, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "STA", .operation = Operation.STA, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "STX", .operation = Operation.STX, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 4 },
        Instruction{ .name = "BCC", .operation = Operation.BCC, .address_mode = AddressMode.REL, .cycles = 2 }, Instruction{ .name = "STA", .operation = Operation.STA, .address_mode = AddressMode.IZY, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 }, Instruction{ .name = "STY", .operation = Operation.STY, .address_mode = AddressMode.ZPX, .cycles = 4 }, Instruction{ .name = "STA", .operation = Operation.STA, .address_mode = AddressMode.ZPX, .cycles = 4 }, Instruction{ .name = "STX", .operation = Operation.STX, .address_mode = AddressMode.ZPY, .cycles = 4 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "TYA", .operation = Operation.TYA, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "STA", .operation = Operation.STA, .address_mode = AddressMode.ABY, .cycles = 5 }, Instruction{ .name = "TXS", .operation = Operation.TXS, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 5 }, Instruction{ .name = "STA", .operation = Operation.STA, .address_mode = AddressMode.ABX, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 5 },
        Instruction{ .name = "LDY", .operation = Operation.LDY, .address_mode = AddressMode.IMM, .cycles = 2 }, Instruction{ .name = "LDA", .operation = Operation.LDA, .address_mode = AddressMode.IZX, .cycles = 6 }, Instruction{ .name = "LDX", .operation = Operation.LDX, .address_mode = AddressMode.IMM, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 }, Instruction{ .name = "LDY", .operation = Operation.LDY, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "LDA", .operation = Operation.LDA, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "LDX", .operation = Operation.LDX, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 3 }, Instruction{ .name = "TAY", .operation = Operation.TAY, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "LDA", .operation = Operation.LDA, .address_mode = AddressMode.IMM, .cycles = 2 }, Instruction{ .name = "TAX", .operation = Operation.TAX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "LDY", .operation = Operation.LDY, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "LDA", .operation = Operation.LDA, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "LDX", .operation = Operation.LDX, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 4 },
        Instruction{ .name = "BCS", .operation = Operation.BCS, .address_mode = AddressMode.REL, .cycles = 2 }, Instruction{ .name = "LDA", .operation = Operation.LDA, .address_mode = AddressMode.IZY, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 5 }, Instruction{ .name = "LDY", .operation = Operation.LDY, .address_mode = AddressMode.ZPX, .cycles = 4 }, Instruction{ .name = "LDA", .operation = Operation.LDA, .address_mode = AddressMode.ZPX, .cycles = 4 }, Instruction{ .name = "LDX", .operation = Operation.LDX, .address_mode = AddressMode.ZPY, .cycles = 4 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "CLV", .operation = Operation.CLV, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "LDA", .operation = Operation.LDA, .address_mode = AddressMode.ABY, .cycles = 4 }, Instruction{ .name = "TSX", .operation = Operation.TSX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "LDY", .operation = Operation.LDY, .address_mode = AddressMode.ABX, .cycles = 4 }, Instruction{ .name = "LDA", .operation = Operation.LDA, .address_mode = AddressMode.ABX, .cycles = 4 }, Instruction{ .name = "LDX", .operation = Operation.LDX, .address_mode = AddressMode.ABY, .cycles = 4 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 4 },
        Instruction{ .name = "CPY", .operation = Operation.CPY, .address_mode = AddressMode.IMM, .cycles = 2 }, Instruction{ .name = "CMP", .operation = Operation.CMP, .address_mode = AddressMode.IZX, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 8 }, Instruction{ .name = "CPY", .operation = Operation.CPY, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "CMP", .operation = Operation.CMP, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "DEC", .operation = Operation.DEC, .address_mode = AddressMode.ZP0, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 5 }, Instruction{ .name = "INY", .operation = Operation.INY, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "CMP", .operation = Operation.CMP, .address_mode = AddressMode.IMM, .cycles = 2 }, Instruction{ .name = "DEX", .operation = Operation.DEX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "CPY", .operation = Operation.CPY, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "CMP", .operation = Operation.CMP, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "DEC", .operation = Operation.DEC, .address_mode = AddressMode.ABS, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 },
        Instruction{ .name = "BNE", .operation = Operation.BNE, .address_mode = AddressMode.REL, .cycles = 2 }, Instruction{ .name = "CMP", .operation = Operation.CMP, .address_mode = AddressMode.IZY, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 8 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "CMP", .operation = Operation.CMP, .address_mode = AddressMode.ZPX, .cycles = 4 }, Instruction{ .name = "DEC", .operation = Operation.DEC, .address_mode = AddressMode.ZPX, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 }, Instruction{ .name = "CLD", .operation = Operation.CLD, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "CMP", .operation = Operation.CMP, .address_mode = AddressMode.ABY, .cycles = 4 }, Instruction{ .name = "NOP", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 7 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "CMP", .operation = Operation.CMP, .address_mode = AddressMode.ABX, .cycles = 4 }, Instruction{ .name = "DEC", .operation = Operation.DEC, .address_mode = AddressMode.ABX, .cycles = 7 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 7 },
        Instruction{ .name = "CPX", .operation = Operation.CPX, .address_mode = AddressMode.IMM, .cycles = 2 }, Instruction{ .name = "SBC", .operation = Operation.SBC, .address_mode = AddressMode.IZX, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 8 }, Instruction{ .name = "CPX", .operation = Operation.CPX, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "SBC", .operation = Operation.SBC, .address_mode = AddressMode.ZP0, .cycles = 3 }, Instruction{ .name = "INC", .operation = Operation.INC, .address_mode = AddressMode.ZP0, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 5 }, Instruction{ .name = "INX", .operation = Operation.INX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "SBC", .operation = Operation.SBC, .address_mode = AddressMode.IMM, .cycles = 2 }, Instruction{ .name = "NOP", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.SBC, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "CPX", .operation = Operation.CPX, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "SBC", .operation = Operation.SBC, .address_mode = AddressMode.ABS, .cycles = 4 }, Instruction{ .name = "INC", .operation = Operation.INC, .address_mode = AddressMode.ABS, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 },
        Instruction{ .name = "BEQ", .operation = Operation.BEQ, .address_mode = AddressMode.REL, .cycles = 2 }, Instruction{ .name = "SBC", .operation = Operation.SBC, .address_mode = AddressMode.IZY, .cycles = 5 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 8 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "SBC", .operation = Operation.SBC, .address_mode = AddressMode.ZPX, .cycles = 4 }, Instruction{ .name = "INC", .operation = Operation.INC, .address_mode = AddressMode.ZPX, .cycles = 6 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 6 }, Instruction{ .name = "SED", .operation = Operation.SED, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "SBC", .operation = Operation.SBC, .address_mode = AddressMode.ABY, .cycles = 4 }, Instruction{ .name = "NOP", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 2 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 7 }, Instruction{ .name = "???", .operation = Operation.NOP, .address_mode = AddressMode.IMP, .cycles = 4 }, Instruction{ .name = "SBC", .operation = Operation.SBC, .address_mode = AddressMode.ABX, .cycles = 4 }, Instruction{ .name = "INC", .operation = Operation.INC, .address_mode = AddressMode.ABX, .cycles = 7 }, Instruction{ .name = "???", .operation = Operation.XXX, .address_mode = AddressMode.IMP, .cycles = 7 },
    };

    self.reset();
}

pub fn deinit(self: *Self) void {
    self.reset();
}

fn write(self: *Self, addr: u16, data: u8) void {
    self.bus.write(addr, data);
}

fn read(self: *Self, addr: u16, readOnly: bool) u8 {
    return self.bus.read(addr, readOnly);
}

fn getFlag(self: *Self, flag: Flags) u8 {
    return (self.status >> flag) & 1;
}

fn setFlag(self: *Self, flag: Flags, value: bool) void {
    var v = @boolToInt(value);
    self.status ^= (-v ^ self.status) & (1 << flag);
}

pub fn reset(self: *Self) void {
    // Reset program counter
    self.addr_absolute = 0xFFFC;
    var lo: u16 = self.read(self.addr_absolute + 0, false);
    var hi: u16 = self.read(self.addr_absolute + 0, false);

    self.ip = (hi << 8) | (lo & 0xFF);

    self.rA = 0x00;
    self.rX = 0x00;
    self.rY = 0x00;

    self.sp = 0xFD;
    self.status = @as(u8, 0x00) | @enumToInt(Flags.U);

    self.addr_absolute = 0x0000;
    self.addr_relative = 0x0000;

    self.byte_fetched = 0x00;

    // Takes 8 cycles to perform reset.
    self.cycles = 8;
}

fn fetch(self: *Self) u8 {
    if (!(self.instruction_table[self.opcode].address_mode == AddressMode.IMP))
        self.fetched = self.read(self.addr_absolute, false);

    return self.fetched;
}

pub fn irq(self: *Self) void {
    if (self.getFlag(Flags.I) != 0)
        return;

    // Push the program counter to the stack.
    self.write(0x0100 + self.sp, (self.ip >> 8) & 0x00FF);
    self.sp -= 1;
    self.write(0x0100 + self.sp, self.ip & 0x00FF);
    self.sp -= 1;

    // Then Push the status register to the stack
    self.setFlag(Flags.B, false);
    self.setFlag(Flags.U, true);
    self.setFlag(Flags.I, true);
    self.write(0x0100 + self.sp, self.status);
    self.sp -= 1;

    // Read new program counter location from fixed address
    self.addr_absolute = 0xFFFE;
    var lo: u16 = self.read(self.addr_absolute + 0);
    var hi: u16 = self.read(self.addr_absolute + 1);

    self.ip = (hi << 8) | (lo & 0xFF);
    self.cycles = 7;
}

pub fn nmi(self: *Self) void {
    self.write(0x0100 + self.sp, (self.ip >> 8) & 0x00FF);
    self.sp -= 1;
    write(0x0100 + self.sp, self.ip & 0x00FF);
    self.sp -= 1;

    self.setFlag(Flags.B, false);
    self.setFlag(Flags.U, true);
    self.setFlag(Flags.I, true);
    self.write(0x0100 + self.sp, self.status);
    self.sp -= 1;

    self.addr_absolute = 0xFFFA;
    var lo: u16 = read(self.addr_absolute + 0);
    var hi: u16 = read(self.addr_absolute + 1);
    self.ip = (hi << 8) | (lo & 0xFF);

    self.cycles = 8;
}

pub fn clock(self: *Self) void {
    if (self.cycles == 0) {
        //Do next operation
        self.opcode = self.read(self.ip, false);
        self.setFlag(Flags.U, true);

        self.ip += 1;

        // Calculate number of cycles
        var addressing_cycles: u8 = self.performAddressing();

        if (addressing_cycles == 255) {
            @panic("Addressing Mode is invalid!");
        }

        var operation_cycles: u8 = self.performOperation();

        if (operation_cycles == 255) {
            @panic("Addressing Mode is invalid!");
        }

        self.cycles = self.instruction_table[self.opcode].cycles + addressing_cycles + operation_cycles;
    }

    self.cycles -= 1;
}

pub fn performAddressing(self: *Self) u8 {
    var addrMode = self.instruction_table[self.opcode].address_mode;

    return switch (addrMode) {
        .IMP => self.IMP(),
        .IMM => self.IMM(),
        .ZP0 => self.ZP0(),
        .ZPX => self.ZPX(),
        .ZPY => self.ZPY(),
        .REL => self.REL(),
        .ABS => self.ABS(),
        .ABX => self.ABX(),
        .ABY => self.ABY(),
        .IND => self.IND(),
        .IZX => self.IZX(),
        .IZY => self.IZY(),
        else => 255,
    };
}

pub fn performOperation(self: *Self) u8 {
    var operation = self.instruction_table[self.opcode].operation;

    return switch (operation) {
        .ADC => self.ADC(),
        .AND => self.AND(),
        .ASL => self.ASL(),
        .BCC => self.BCC(),
        .BCS => self.BCS(),
        .BEQ => self.BEQ(),
        .BIT => self.BIT(),
        .BMI => self.BMI(),
        .BNE => self.BNE(),
        .BPL => self.BPL(),
        .BRK => self.BRK(),
        .BVC => self.BVC(),
        .BVS => self.BVS(),
        .CLC => self.CLC(),
        .CLD => self.CLD(),
        .CLI => self.CLI(),
        .CLV => self.CLV(),
        .CMP => self.CMP(),
        .CPX => self.CPX(),
        .CPY => self.CPY(),
        .DEC => self.DEC(),
        .DEX => self.DEX(),
        .DEY => self.DEY(),
        .EOR => self.EOR(),
        .INC => self.INC(),
        .INX => self.INX(),
        .INY => self.INY(),
        .JMP => self.JMP(),
        .JSR => self.JSR(),
        .LDA => self.LDA(),
        .LDX => self.LDX(),
        .LDY => self.LDY(),
        .LSR => self.LSR(),
        .NOP => self.NOP(),
        .ORA => self.ORA(),
        .PHA => self.PHA(),
        .PHP => self.PHP(),
        .PLA => self.PLA(),
        .PLP => self.PLP(),
        .ROL => self.ROL(),
        .ROR => self.ROR(),
        .RTI => self.RTI(),
        .RTS => self.RTS(),
        .SBC => self.SBC(),
        .SEC => self.SEC(),
        .SED => self.SED(),
        .SEI => self.SEI(),
        .STA => self.STA(),
        .STX => self.STX(),
        .STY => self.STY(),
        .TAX => self.TAX(),
        .TAY => self.TAY(),
        .TSX => self.TSX(),
        .TXA => self.TXA(),
        .TXS => self.TXS(),
        .TYA => self.TYA(),
        else => 255,
    };
}

fn ADC(self: *Self) u8 {
    _ = self;    
}

fn AND(self: *Self) u8 {
    _ = self;    
}

fn ASL(self: *Self) u8 {
    _ = self;    
}

fn BCC(self: *Self) u8 {
    _ = self;    
}

fn BCS(self: *Self) u8 {
    _ = self;    
}

fn BEQ(self: *Self) u8 {
    _ = self;    
}

fn BIT(self: *Self) u8 {
    _ = self;    
}

fn BMI(self: *Self) u8 {
    _ = self;    
}

fn BNE(self: *Self) u8 {
    _ = self;    
}

fn BPL(self: *Self) u8 {
    _ = self;    
}

fn BRK(self: *Self) u8 {
    _ = self;    
}

fn BVC(self: *Self) u8 {
    _ = self;    
}

fn BVS(self: *Self) u8 {
    _ = self;    
}

fn CLC(self: *Self) u8 {
    _ = self;    
}

fn CLD(self: *Self) u8 {
    _ = self;    
}

fn CLI(self: *Self) u8 {
    _ = self;    
}

fn CLV(self: *Self) u8 {
    _ = self;    
}

fn CMP(self: *Self) u8 {
    _ = self;    
}

fn CPX(self: *Self) u8 {
    _ = self;    
}

fn CPY(self: *Self) u8 {
    _ = self;    
}

fn DEC(self: *Self) u8 {
    _ = self;    
}

fn DEX(self: *Self) u8 {
    _ = self;    
}

fn DEY(self: *Self) u8 {
    _ = self;    
}

fn EOR(self: *Self) u8 {
    _ = self;    
}

fn INC(self: *Self) u8 {
    _ = self;    
}

fn INX(self: *Self) u8 {
    _ = self;    
}

fn INY(self: *Self) u8 {
    _ = self;    
}

fn JMP(self: *Self) u8 {
    _ = self;    
}

fn JSR(self: *Self) u8 {
    _ = self;    
}

fn LDA(self: *Self) u8 {
    _ = self;    
}

fn LDX(self: *Self) u8 {
    _ = self;    
}

fn LDY(self: *Self) u8 {
    _ = self;    
}

fn LSR(self: *Self) u8 {
    _ = self;    
}

fn NOP(self: *Self) u8 {
    _ = self;    
}

fn ORA(self: *Self) u8 {
    _ = self;    
}

fn PHA(self: *Self) u8 {
    _ = self;    
}

fn PHP(self: *Self) u8 {
    _ = self;    
}

fn PLA(self: *Self) u8 {
    _ = self;    
}

fn PLP(self: *Self) u8 {
    _ = self;    
}

fn ROL(self: *Self) u8 {
    _ = self;    
}

fn ROR(self: *Self) u8 {
    _ = self;    
}

fn RTI(self: *Self) u8 {
    _ = self;    
}

fn RTS(self: *Self) u8 {
    _ = self;    
}

fn SBC(self: *Self) u8 {
    _ = self;    
}

fn SEC(self: *Self) u8 {
    _ = self;    
}

fn SED(self: *Self) u8 {
    _ = self;    
}

fn SEI(self: *Self) u8 {
    _ = self;    
}

fn STA(self: *Self) u8 {
    _ = self;    
}

fn STX(self: *Self) u8 {
    _ = self;    
}

fn STY(self: *Self) u8 {
    _ = self;    
}

fn TAX(self: *Self) u8 {
    _ = self;    
}

fn TAY(self: *Self) u8 {
    _ = self;    
}

fn TSX(self: *Self) u8 {
    _ = self;    
}

fn TXA(self: *Self) u8 {
    _ = self;    
}

fn TXS(self: *Self) u8 {
    _ = self;    
}

fn TYA(self: *Self) u8 {
    _ = self;    
}
