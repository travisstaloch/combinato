pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);
    if (args.len < 2) return error.MissingJsonFileArg;
    const f = try std.fs.cwd().openFile(args[1], .{});
    defer f.close();
    const input = try f.readToEndAllocOptions(alloc, std.math.maxInt(u32), null, 1, 0);
    defer alloc.free(input);
    // var buf: [1_000_000]u8 = undefined;
    // const amt = try f.read(&buf);
    // buf[amt] = 0;
    // const input = buf[0..amt :0];

    var ctx = try ParseCtx.init(alloc);
    defer ctx.deinit();
    const r = try json.element.run(&json.element, &ctx, input);
    if (r.len != 0) return error.ParseFailure;
}

const ParseCtx = struct {
    alloc: std.mem.Allocator,
    arena: *std.heap.ArenaAllocator,
    stack: std.ArrayList(Value),
    root: ?Value,
    /// when true, the last number parsed has a fraction component and should
    /// be parsed as a float.
    has_fraction: bool = false,
    strings: std.StringHashMapUnmanaged(void) = .{},

    pub fn init(alloc: mem.Allocator) !ParseCtx {
        const arena = try alloc.create(std.heap.ArenaAllocator);
        arena.* = std.heap.ArenaAllocator.init(alloc);
        return .{
            .stack = std.ArrayList(Value).init(alloc),
            .alloc = alloc,
            .arena = arena,
            .root = null,
        };
    }

    pub fn deinit(ctx: *ParseCtx) void {
        ctx.arena.deinit();
        ctx.alloc.destroy(ctx.arena);
        ctx.stack.deinit();
    }

    pub fn push(ctx: *ParseCtx, v: Value) !void {
        if (ctx.root) |r| {
            try ctx.stack.append(r);
        }
        ctx.root = v;
    }

    pub fn pop(ctx: *ParseCtx) void {
        if (ctx.stack.popOrNull()) |l| ctx.root = l;
    }

    pub fn intern(ctx: *ParseCtx, s: []const u8) ![]const u8 {
        const a = ctx.arena.allocator();
        const gop = try ctx.strings.getOrPut(a, s);
        if (!gop.found_existing) gop.key_ptr.* = try a.dupe(u8, s);
        return gop.key_ptr.*;
    }

    pub fn format(ctx: ParseCtx, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        if (ctx.root) |r|
            try writer.print("root {}\n", .{json.Fmt{ .v = r }})
        else
            try writer.print("root null\n", .{});
        try writer.print("stack:\n", .{});
        for (ctx.stack.items, 0..) |v, i| {
            try writer.print("{} {}\n", .{ i, json.Fmt{ .v = v } });
        }
    }
};

const json = struct {
    pub const Error = combinato.ParseError || mem.Allocator.Error;

    // -- parsers -- following https://www.json.org/json-en.html
    const element = P.seq(&&.{ ws, P.ref(valueRef), ws });
    fn valueRef() P {
        return value;
    }
    const value = P.alt(&&.{
        object,
        array,
        number.then(onNumber),
        string.then(onString),
        P.string(&"true").then(onConstant(.{ .bool = true })),
        P.string(&"false").then(onConstant(.{ .bool = false })),
        P.string(&"null").then(onConstant(.null)),
    });
    const ws = P.many_whitespace;
    // object
    const object = P.seq(&&.{
        P.char(&'{').then(onObject),
        ws,
        members,
        ws,
        P.char(&'}'),
    });
    const members = P.sepBy(&.{ member, P.char(&',') });
    const member = P.seq(&&.{
        ws,
        string.then(onObjectKey),
        ws,
        P.char(&':'),
        element.then(onObjectValue),
    });
    // array
    const array = P.seq(&&.{
        P.char(&'[').then(onArray),
        ws,
        elements,
        ws,
        P.char(&']'),
    });
    const elements = P.sepBy(&.{ element.then(onArrayElement), P.char(&',') });
    // string
    const string = P.seq(&&.{
        P.char(&'"'),
        characters,
        P.char(&'"'),
    });
    const characters = P.alt(&&.{
        P.seq(&&.{ P.anychar(&"\"\\").not(), P.codepoint }),
        P.seq(&&.{ P.char(&'\\'), escape }),
    })
        .many();
    const escape = P.alt(&&.{
        P.anychar(&"\"\\/bfnrt"),
        P.seq(&&.{ P.char(&'u'), hex, hex, hex, hex }),
    });
    // number
    const hex = P.alt(&&.{
        digit,
        P.range(&"AF".*),
        P.range(&"af".*),
    });
    const number = P.seq(&&.{ integer, fraction, exponent });
    const integer = P.seq(&&.{ P.char(&'-').opt(), integer0 });
    const integer0 = P.alt(&&.{ P.seq(&&.{ onenine, digit.many() }), digit });
    const digits = digit.some();
    const digit = P.range(&"09".*);
    const onenine = P.range(&"19".*);
    const fraction = P.seq(&&.{ P.char(&'.'), digits.then(onFraction) })
        .opt();
    const exponent = P.seq(&&.{ P.anychar(&"Ee"), sign, digits })
        .opt();
    const sign = P.anychar(&"-+").opt();

    // -- callbacks --
    fn onObject(ctx: *ParseCtx, _: []const u8) Error!void {
        trace("-- beginObject\n", .{});
        try ctx.push(.{ .object = std.json.ObjectMap.init(ctx.arena.allocator()) });
    }
    fn onObjectKey(ctx: *ParseCtx, bytes: []const u8) Error!void {
        trace("-- onObjectKey\n{}", .{ctx});
        try ctx.push(.{ .string = try ctx.intern(bytes[1 .. bytes.len - 1]) });
    }
    fn onObjectValue(ctx: *ParseCtx, _: []const u8) Error!void {
        trace("-- onObjectValue\n{}", .{ctx});
        const key = ctx.stack.pop().string;
        try ctx.stack.items[ctx.stack.items.len - 1].object.put(key, ctx.root.?);
        ctx.pop();
    }
    fn onArray(ctx: *ParseCtx, _: []const u8) Error!void {
        trace("-- onArray\n", .{});
        try ctx.push(.{ .array = std.json.Array.init(ctx.arena.allocator()) });
    }
    fn onArrayElement(ctx: *ParseCtx, _: []const u8) Error!void {
        trace("-- onArrayElement\n{}", .{ctx});
        try ctx.stack.items[ctx.stack.items.len - 1].array.append(ctx.root.?);
        ctx.pop();
    }
    fn onFraction(ctx: *ParseCtx, _: []const u8) Error!void {
        ctx.has_fraction = true;
    }
    fn onNumber(ctx: *ParseCtx, bytes: []const u8) Error!void {
        trace("-- num {s}\n{}", .{ bytes, ctx });
        defer ctx.has_fraction = false;
        if (ctx.has_fraction) {
            try ctx.push(if (std.fmt.parseFloat(f64, bytes)) |f|
                .{ .float = f }
            else |_|
                .{ .number_string = bytes });
        } else {
            try ctx.push(if (std.fmt.parseInt(i64, bytes, 10)) |i|
                .{ .integer = i }
            else |_|
                .{ .number_string = bytes });
        }
    }
    fn onString(ctx: *ParseCtx, bytes: []const u8) Error!void {
        trace("-- str {s}\n", .{bytes});
        try ctx.push(.{ .string = try ctx.intern(bytes[1 .. bytes.len - 1]) });
    }
    fn onConstant(comptime v: Value) fn (*ParseCtx, _: []const u8) Error!void {
        return struct {
            fn func(ctx: *ParseCtx, _: []const u8) Error!void {
                trace("-- {s}\n", .{@tagName(v)});
                try ctx.push(v);
            }
        }.func;
    }

    const Fmt = struct {
        v: Value,
        pub fn format(f: Fmt, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            try std.json.stringify(f.v, .{ .whitespace = .indent_2 }, writer);
        }
    };
};

fn expectParsed(p: *const P, input: [:0]const u8) !void {
    var ctx = try ParseCtx.init(testing.allocator);
    defer ctx.deinit();
    const r = try p.run(p, &ctx, input);
    trace("rest '{s}'\n", .{r});
    trace("ctx {}\n", .{ctx});
    trace("json: {}\n", .{json.Fmt{ .v = ctx.root.? }});
    try testing.expectFmt(input, "{}", .{json.Fmt{ .v = ctx.root.? }});
    try testing.expectEqual(0, r.len);
}

test "json1" {
    try expectParsed(&json.value, "800");
    try expectParsed(&json.value,
        \\{
        \\  "Width": 800
        \\}
    );
    try expectParsed(&json.value,
        \\{
        \\  "Width": 800,
        \\  "Height": 600
        \\}
    );
    try expectParsed(&json.value,
        \\[]
    );
    try expectParsed(&json.value,
        \\[
        \\  1,
        \\  2,
        \\  3
        \\]
    );
    try expectParsed(&json.value,
        \\{
        \\  "Width": 800,
        \\  "o": {
        \\    "a": null
        \\  },
        \\  "array": [
        \\    116,
        \\    943,
        \\    234
        \\  ],
        \\  "foo": 1
        \\}
    );
}

var trace_count: usize = 0;
fn trace(comptime fmt: []const u8, args: anytype) void {
    _ = fmt; // autofix
    _ = args; // autofix
    // std.debug.print("{d: <5} " ++ fmt, .{trace_count} ++ args);
    // trace_count += 1;
}

const combinato = @import("combinato");
const P = combinato.Parser(json.Error, .{ .UserData = *ParseCtx });
const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const Value = std.json.Value;
