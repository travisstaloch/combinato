test {
    _ = @import("json.zig");
}

const P = combinato.Parser(mem.Allocator.Error ||
    std.fmt.ParseIntError ||
    std.io.FixedBufferStream([]u8).WriteError);

fn expectResult(
    parser: *const P,
    input: [:0]const u8,
    expected_rest: anyerror![:0]const u8,
) !void {
    try expectResultImpl(parser, input, expected_rest, null);
}

fn expectResultWithData(
    parser: *const P,
    input: [:0]const u8,
    expected_rest: anyerror![:0]const u8,
    userdata: ?*anyopaque,
) !void {
    try expectResultImpl(parser, input, expected_rest, userdata);
}

fn expectResultImpl(
    parser: *const P,
    input: [:0]const u8,
    // expected_written: []const u8,
    expected_result: anyerror![:0]const u8,
    userdata: ?*anyopaque,
) !void {
    const result = parser.run(parser, userdata, input);
    if (expected_result) |er| {
        try testing.expectEqualStrings(er, result catch
            return error.UnexpectedResult);
    } else |e| {
        try testing.expectError(e, result);
    }
}

test "epsilon" {
    const p = &P.epsilon;
    try expectResult(p, "", "");
    try expectResult(p, "a", "a");
}

test "eos" {
    const p = &P.eos;
    try expectResult(p, "", "");
    try expectResult(p, "a", error.ParseFailure);
}

test "rest" {
    const p = &P.rest;
    try expectResult(p, "", "");
    try expectResult(p, "a", "");
}

test "char" {
    const a = &P.char(&'a');
    try expectResult(a, "", error.ParseFailure);
    try expectResult(a, "b", error.ParseFailure);
    try expectResult(a, "a", "");
    try expectResult(a, "aa", "a");
}

test "range" {
    const lower = &P.lowercase;
    try expectResult(lower, "", error.ParseFailure);
    try expectResult(lower, "a", "");
    try expectResult(lower, "z", "");
    try expectResult(lower, &.{'a' - 1}, error.ParseFailure);
    try expectResult(lower, &.{'z' + 1}, error.ParseFailure);
}

test "func" {
    const ws = &P.func(std.ascii.isWhitespace);
    try expectResult(ws, "", error.ParseFailure);
    try expectResult(ws, " ", "");
    try expectResult(ws, "\t", "");
    try expectResult(ws, "a", error.ParseFailure);
}

test "anychar" {
    const vowels = &P.anychar(&"aeiou");
    try expectResult(vowels, "", error.ParseFailure);
    try expectResult(vowels, " ", error.ParseFailure);
    try expectResult(vowels, "a", "");
    const anychar = comptime blk: {
        var all_u8s: [256]u8 = undefined;
        for (0..256) |i| all_u8s[i] = @intCast(i);
        break :blk P.anychar(&&all_u8s);
    };
    try expectResult(&anychar, "\x00", "");
    try expectResult(&anychar, "\xff", "");
}

test "charset" {
    var set = CharSet.initEmpty();
    set.set('a');
    const a = &P.charset(&set);
    try expectResult(a, "", error.ParseFailure);
    try expectResult(a, " ", error.ParseFailure);
    try expectResult(a, "a", "");
}

test "codepoint" {
    const cp = &P.codepoint;
    try expectResult(cp, "", error.ParseFailure);
    try expectResult(cp, "a", "");
    try expectResult(cp, "Āā", "ā");
    try expectResult(cp, "Ā", "");
}

test "amp" {
    const comment = &P.seq(&&.{
        P.string(&"//"),
        P.amp(&P.alt(&&.{ P.char(&'!'), P.char(&'/') })),
    });
    try expectResult(comment, "", error.ParseFailure);
    try expectResult(comment, "//", error.ParseFailure);
    try expectResult(comment, "// ", error.ParseFailure);
    try expectResult(comment, "//!", "!");
    try expectResult(comment, "///", "/");
}

test "not" {
    const comment = &P.seq(&&.{
        P.string(&"//"),
        P.alt(&&.{ P.char(&'!'), P.char(&'/') })
            .not(),
    });
    try expectResult(comment, "", error.ParseFailure);
    try expectResult(comment, "//", "");
    try expectResult(comment, "// ", " ");
    try expectResult(comment, "//!", error.ParseFailure);
    try expectResult(comment, "///", error.ParseFailure);
}

test "string" {
    const a = &P.string(&"aa");
    try expectResult(a, "", error.ParseFailure);
    try expectResult(a, "a", error.ParseFailure);
    try expectResult(a, "aa", "");
    try expectResult(a, "b", error.ParseFailure);
}

test "counted" {
    const as = &P.counted(&P.string(&"a"), .{ .min = 2, .max = 4 });
    try expectResult(as, "", error.ParseFailure);
    try expectResult(as, "a", error.ParseFailure);
    try expectResult(as, "aa", "");
    try expectResult(as, "aab", "b");
    try expectResult(as, "aaaaa", "a");
    try expectResult(as, "b", error.ParseFailure);
}

test "many" {
    const as = &P.string(&"a").many();
    try expectResult(as, "", "");
    try expectResult(as, "a", "");
    try expectResult(as, "aa", "");
    try expectResult(as, "aab", "b");
    try expectResult(as, "b", "b");
}

test "atMost" {
    const as = &P.string(&"a").atMost(1);
    try expectResult(as, "", "");
    try expectResult(as, "a", "");
    try expectResult(as, "aa", "a");
    try expectResult(as, "aab", "ab");
    try expectResult(as, "b", "b");
}

test "some" {
    const as = &P.string(&"a").some();
    try expectResult(as, "", error.ParseFailure);
    try expectResult(as, "a", "");
    try expectResult(as, "aa", "");
    try expectResult(as, "aab", "b");
    try expectResult(as, "b", error.ParseFailure);
}

test "opt" {
    const opta = &P.string(&"a").opt();
    try expectResult(opta, "", "");
    try expectResult(opta, "a", "");
    try expectResult(opta, "aa", "a");
    try expectResult(opta, "b", "b");
}

test "seq" {
    const ab = &P.seq(&&.{ P.string(&"a"), P.string(&"b") });
    try expectResult(ab, "", error.ParseFailure);
    try expectResult(ab, "a", error.ParseFailure);
    try expectResult(ab, "aa", error.ParseFailure);
    try expectResult(ab, "ab", "");
    try expectResult(ab, "abc", "c");
}

test "alt" {
    const ab = &P.alt(&&.{ P.char(&'a'), P.char(&'b') });
    try expectResult(ab, "", error.ParseFailure);
    try expectResult(ab, "a", "");
    try expectResult(ab, "b", "");
    try expectResult(ab, "c", error.ParseFailure);
    try expectResult(ab, "ab", "b");
    try expectResult(ab, "ba", "a");
}

test "ref" {
    const Scope = struct {
        const digit = P.range(&"19".*);
        const digits = P.alt(&&.{
            P.seq(&&.{ digit, P.ref(digitsRef) }),
            digit,
        });
        fn digitsRef() P {
            return digits;
        }
    };
    try expectResult(&Scope.digits, "123", "");
}

test "sepBy/then" {
    const digits = P.digit(10).some().then(struct {
        fn action(userdata: ?*anyopaque, bytes: []const u8) !void {
            const l: *std.ArrayList(i32) = @ptrCast(@alignCast(userdata));
            try l.append(try std.fmt.parseInt(i32, bytes, 10));
        }
    }.action);
    const ws = P.many_whitespace;
    const items = P.sepBy(&.{
        P.seq(&&.{ ws, digits }),
        P.seq(&&.{ ws, P.char(&',') }),
    });
    const listp = &P.seq(&&.{ P.char(&'['), ws, items, ws, P.char(&']') });
    var list = std.ArrayList(i32).init(testing.allocator);
    defer list.deinit();
    try expectResultWithData(listp, "[]", "", &list);
    try testing.expectEqualSlices(i32, &.{}, list.items);
    list.clearRetainingCapacity();
    try expectResultWithData(listp, "[ ]", "", &list);
    try testing.expectEqualSlices(i32, &.{}, list.items);
    list.clearRetainingCapacity();
    try expectResultWithData(listp, "[ 1 ]", "", &list);
    try testing.expectEqualSlices(i32, &.{1}, list.items);
    list.clearRetainingCapacity();
    try expectResultWithData(listp, "[1 ]", "", &list);
    try testing.expectEqualSlices(i32, &.{1}, list.items);
    list.clearRetainingCapacity();
    try expectResultWithData(listp, "[ 1]", "", &list);
    try testing.expectEqualSlices(i32, &.{1}, list.items);
    list.clearRetainingCapacity();
    try expectResultWithData(listp, "[1,]", error.ParseFailure, &list);
    try testing.expectEqualSlices(i32, &.{1}, list.items);
    list.clearRetainingCapacity();
    try expectResultWithData(listp, "[ 1 , 2 ]", "", &list);
    try testing.expectEqualSlices(i32, &.{ 1, 2 }, list.items);
    list.clearRetainingCapacity();
    try expectResultWithData(listp, "[ 1 , 2, ]", error.ParseFailure, &list);
    try testing.expectEqualSlices(i32, &.{ 1, 2 }, list.items);
}

test "then" {
    // -- then --
    const onInt = struct {
        fn action(userdata: ?*anyopaque, _: []const u8) !void {
            const i: *i32 = @ptrCast(@alignCast(userdata orelse
                return error.MissingUserdata));
            i.* += 1;
        }
    }.action;

    const int10 = P.digit(10).some();
    const map_int = int10.then(onInt);
    const map_bin = P.digit(2).some().then(onInt);
    try testing.expectError(error.MissingUserdata, map_int.run(&map_int, null, "123"));
    var call_count: i32 = 0;
    try expectResultWithData(&map_int, "123", "", &call_count);
    try expectResultWithData(&map_bin, "101", "", &call_count);
    try testing.expectEqual(2, call_count);
    call_count = 0;
    {
        const map_int2 = int10.some().then(P.Action.integer(u8, .{}));
        var x: u8 = undefined;
        try expectResultWithData(&map_int2, "123", "", &x);
        try testing.expectEqual(123, x);
    }
    {
        const E = enum { foo, bar };
        const map_enum = P.lowercase.some().then(P.Action.enumeration(E));
        var x: E = undefined;
        try expectResultWithData(&map_enum, "bar  ", "  ", &x);
        try testing.expectEqual(.bar, x);
    }
    {
        const map_float = P.alt(&&.{ int10, P.char(&'.') })
            .some()
            .then(P.Action.float(f32));
        var x: f32 = undefined;
        try expectResultWithData(&map_float, "123.45", "", &x);
        try testing.expectEqual(123.45, x);
    }
    {
        const map_bool = P.lowercase.some().then(P.Action.boolean);
        var x: bool = true;
        try expectResultWithData(&map_bool, "false ", " ", &x);
        try testing.expectEqual(false, x);
    }

    const ws = P.char(&' ');
    const pt_seq = P.seq(&&.{ map_int, ws, map_int });
    try expectResultWithData(&pt_seq, "1 2", "", &call_count);
    try testing.expectEqual(2, call_count);
    call_count = 0;

    const lowers = P.lowercase.some().then(onInt);
    const boolp = P.alt(&&.{ P.string(&"false"), P.string(&"true") }).then(onInt);
    const misc_seq = P.seq(&&.{ lowers, ws, lowers, ws, map_int, ws, map_int, ws, boolp });
    try expectResultWithData(&misc_seq, "a b 0 0 true", "", &call_count);
    try testing.expectEqual(5, call_count);
}

test "bytes to hex" {
    var list = std.ArrayList(u8).init(testing.allocator);
    defer list.deinit();
    try expectResultWithData(&P.any.then(struct {
        fn action(userdata: ?*anyopaque, bytes: []const u8) !void {
            const l: *std.ArrayList(u8) = @ptrCast(@alignCast(userdata));
            for (bytes) |c| {
                const buf: [1]u8 = .{c};
                const hex = std.fmt.bytesToHex(buf, .lower);
                try l.appendSlice(&hex);
            }
        }
    }.action), "\xab", "", &list);
    try testing.expectEqualStrings("ab", list.items);
}

test "enumeration" {
    const e = P.enumeration(enum { a, b, aa });
    try expectResult(&e, "c", error.ParseFailure);
    try expectResult(&e, "a", "");
    try expectResult(&e, "b", "");
    try expectResult(&e, "aa", "");
    try expectResult(&e, "ab", "b");
}

test "write directly to struct bytes" {
    var s: extern struct { x: i32, y: i32 } = undefined;
    var fbs = std.io.fixedBufferStream(mem.asBytes(&s));
    const ws = P.char(&' ');
    const int10 = P.digit(10).some().then(struct {
        fn action(userdata: ?*anyopaque, bytes: []const u8) !void {
            const fb: *std.io.FixedBufferStream([]u8) = @ptrCast(@alignCast(userdata));
            const i = try std.fmt.parseInt(i32, bytes, 10);
            try fb.writer().writeInt(i32, i, .little);
        }
    }.action);
    const pt_seq = P.seq(&&.{ int10, ws, int10 });
    const r = try pt_seq.run(&pt_seq, &fbs, "1 2");
    try testing.expectEqual(0, r.len);
    try testing.expectEqual(1, s.x);
    try testing.expectEqual(2, s.y);
}

test "write directly to array bytes" {
    var a: [2]i32 = undefined;
    var fbs = std.io.fixedBufferStream(mem.asBytes(&a));
    const ws = P.char(&' ');
    const int10 = P.digit(10).some().then(struct {
        fn action(userdata: ?*anyopaque, bytes: []const u8) !void {
            const fb: *std.io.FixedBufferStream([]u8) =
                @ptrCast(@alignCast(userdata));
            const i = try std.fmt.parseInt(i32, bytes, 10);
            try fb.writer().writeInt(i32, i, .little);
        }
    }.action);
    const pt_seq = P.seq(&&.{ int10, ws, int10 });
    const r = try pt_seq.run(&pt_seq, &fbs, "1 2");
    try testing.expectEqual(0, r.len);
    try testing.expectEqual(1, a[0]);
    try testing.expectEqual(2, a[1]);
}

test "isNullable" {
    try testing.expect(P.epsilon.isNullable());
    try testing.expect(P.eos.isNullable());
    try testing.expect(P.rest.isNullable());

    const ps = struct {
        const a = P.char(&'a');
    };
    const a = ps.a;
    try testing.expect(!a.isNullable());
    try testing.expect(!P.lowercase.isNullable());
    try testing.expect(!P.whitespace.isNullable());
    try testing.expect(!P.string(&"a").isNullable());

    try testing.expect(!P.amp(&a).isNullable());
    try testing.expect(!P.not(&a).isNullable());
    try testing.expect(P.opt(&a).isNullable());
    try testing.expect(a.many().isNullable());
    try testing.expect(a.atMost(1).isNullable());
    try testing.expect(!a.some().isNullable());
    try testing.expect(!a.counted(.{ .min = 2 }).isNullable());
    try testing.expect(!P.ref(struct {
        fn f() P.Self {
            return ps.a;
        }
    }.f).isNullable());
    try testing.expect(!P.sepBy1(&.{ a, a }).isNullable());
    try testing.expect(P.sepBy(&.{ a, a }).isNullable());
    const int = P.digit(10).some();
    try testing.expect(!int.isNullable());

    try testing.expect(P.seq(&&.{ a.many(), a.many() }).isNullable());
    try testing.expect(!P.seq(&&.{ a.many(), a }).isNullable());
    try testing.expect(!P.seq(&&.{ a, a.many() }).isNullable());
    try testing.expect(!P.seq(&&.{ a, a }).isNullable());

    try testing.expect(P.alt(&&.{ a.many(), a.many() }).isNullable());
    try testing.expect(P.alt(&&.{ a.many(), a }).isNullable());
    try testing.expect(P.alt(&&.{ a, a.many() }).isNullable());
    try testing.expect(!P.alt(&&.{ a, a }).isNullable());
}

fn expectFirstSet(p: P.Self, expected: []const u8) !void {
    var actual = CharSet.initEmpty();
    p.firstSet(&actual);
    var iter = actual.iterator(.{});
    var i: usize = 0;
    while (iter.next()) |idx| : (i += 1) {
        try testing.expectEqual(expected[i], idx);
    }
    try testing.expectEqual(i, expected.len);
}

test "first sets" {
    try expectFirstSet(P.epsilon, "\x00");
    try expectFirstSet(P.eos, "\x00");
    try expectFirstSet(P.rest, "\x00");
    const ps = struct {
        const a = P.char(&'a');
    };
    const a = ps.a;
    try expectFirstSet(a, "a");
    try expectFirstSet(P.range(&"ab".*), "ab");
    try expectFirstSet(P.string(&"a"), "a");
    try expectFirstSet(P.anychar(&"abc"), "abc");
    var set = CharSet.initEmpty();
    set.set('a');
    set.set('b');
    set.set('c');
    try expectFirstSet(P.charset(&set), "abc");
    try expectFirstSet(
        P.whitespace,
        &.{ '\t', '\n', std.ascii.control_code.vt, std.ascii.control_code.ff, '\r', ' ' },
    );

    try expectFirstSet(P.amp(&a), "a");
    try expectFirstSet(P.not(&P.not(&a)), "a");
    try expectFirstSet(P.opt(&a), "\x00a");
    try expectFirstSet(a.many(), "\x00a");
    try expectFirstSet(a.atMost(2), "\x00a");
    try expectFirstSet(a.some(), "a");
    try expectFirstSet(a.counted(.{ .min = 2 }), "a");
    try expectFirstSet(P.ref(struct {
        fn f() P.Self {
            return ps.a;
        }
    }.f), "a");
    try expectFirstSet(P.sepBy1(&.{ a, a }), "a");
    try expectFirstSet(P.sepBy(&.{ a, a }), "\x00a");
    const int = P.digit(10).some();
    try expectFirstSet(int, "0123456789");

    // seq
    try expectFirstSet(P.seq(&&.{ P.epsilon, a }), "\x00a");
    try expectFirstSet(P.range(&.{ 0, 2 }), &.{ 0, 1, 2 });
    try expectFirstSet(P.not(&P.range(&.{ 3, 255 })), &.{ 0, 1, 2 });
    try expectFirstSet(P.seq(&&.{ P.not(&P.range(&.{ 2, 255 })), a }), &.{ 0, 1, 'a' });
    try expectFirstSet(P.seq(&&.{ P.amp(&P.range(&.{ 0, 1 })), a }), &.{ 0, 1, 'a' });
    // alt
    try expectFirstSet(P.alt(&&.{ P.epsilon, a }), "\x00a");
}

const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const combinato = @import("combinato");
const CharSet = combinato.CharSet;
