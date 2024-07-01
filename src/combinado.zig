//!
//! Most of the parsers found here are adaptations from
//! https://github.com/Hejsil/mecha/
//!
//! This library provides similar functionality to mecha but with less use of
//! comptime and by passing user defined data through each parser.  Here each
//! parser is a node in a graph consisting of only a tag, some arbitrary data,
//! and a run method.  As a result, parsers may be chained as in
//! 'uppercase.some().then(action)'.
//!

const Tag = enum {
    epsilon,
    eos,
    rest,

    char,
    range,
    func,
    string,
    charset,
    enumeration,

    seq,
    alt,
    amp,
    not,
    opt,
    counted,
    many,
    some,
    ref,
    sepby1,
    sepby,
    then,
    until,
};

pub const ParseError = error{ ParseFailure, MissingUserdata };

pub const CountedOptions = struct {
    max: u32 = std.math.maxInt(u32),
    min: u32 = 0,
};

pub const CharSet = std.StaticBitSet(256);
pub const CharSetFmt = struct {
    set: CharSet,
    pub fn format(f: CharSetFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        var iter = f.set.iterator(.{});
        _ = try writer.write("[");
        while (iter.next()) |i| {
            try writer.print("{'}", .{std.zig.fmtEscapes(&.{@as(u8, @intCast(i))})});
        }
        _ = try writer.write("]");
    }
};

/// Parser combinators which pass around user defined data.
///
/// Some parser methods are be marked inline to avoid returning dangling
/// pointers.
pub fn Parser(comptime Err: type) type {
    return struct {
        run: *const ParseFn,
        data: *const anyopaque,
        tag: Tag,

        pub const Error = Err || ParseError;
        pub const Ret = Error![:0]const u8;
        pub const ParseFn = fn (self: *const Self, userdata: ?*anyopaque, input: [:0]const u8) Ret;

        pub const Self = @This();

        fn dataAs(p: Self, comptime T: type) T {
            switch (p.tag) {
                .epsilon, .eos, .rest, .enumeration => unreachable,
                .opt,
                .not,
                .amp,
                .then,
                => assert(T == *const Self),
                .char => assert(T == *const u8),
                .range => assert(T == *const [2]u8),
                .func => assert(T == *const fn (u8) bool),
                .charset => assert(T == *const CharSet),
                .string => assert(T == *const []const u8),
                .seq, .alt => assert(T == *const []const Self),
                .counted, .some, .many => assert(T == *const Counted),
                .ref => assert(T == *const fn () Self),
                .sepby, .sepby1 => assert(T == *const [2]Self),
                .until => assert(T == *const Until),
            }
            return @ptrCast(@alignCast(p.data));
        }

        /// Always succeeds. Consumes nothing.
        pub const epsilon: Self = .{
            .run = struct {
                fn run(_: *const Self, _: ?*anyopaque, input: [:0]const u8) Ret {
                    return input;
                }
            }.run,
            .data = undefined,
            .tag = .epsilon,
        };

        /// Succeeds on when input length is zero. Consumes nothing.
        pub const eos: Self = .{
            .run = struct {
                fn run(_: *const Self, _: ?*anyopaque, input: [:0]const u8) Ret {
                    return if (input.len == 0) input else error.ParseFailure;
                }
            }.run,
            .data = undefined,
            .tag = .eos,
        };

        /// Always succeeds.  Consumes all input.
        pub const rest: Self = .{
            .run = struct {
                fn run(_: *const Self, _: ?*anyopaque, input: [:0]const u8) Ret {
                    return input[input.len..];
                }
            }.run,
            .data = undefined,
            .tag = .rest,
        };

        /// Consumes one byte if available.
        pub const any: Self = .{
            .run = struct {
                fn run(_: *const Self, _: ?*anyopaque, input: [:0]const u8) Ret {
                    if (input.len == 0) return error.ParseFailure;
                    return input[1..];
                }
            }.run,
            .data = undefined,
            .tag = .rest,
        };

        /// Consumes one codepoint if available.  Fails on invalid utf8.
        pub const codepoint: Self = .{
            .run = struct {
                fn run(_: *const Self, _: ?*anyopaque, input: [:0]const u8) Ret {
                    const cp_len = std.unicode.utf8ByteSequenceLength(input[0]) catch
                        return error.ParseFailure;
                    trace("cp_len {}\n", .{cp_len});
                    if (cp_len > input.len) return error.ParseFailure;
                    _ = std.unicode.utf8Decode(input[0..cp_len]) catch
                        return error.ParseFailure;
                    return input[cp_len..];
                }
            }.run,
            .data = undefined,
            .tag = .rest,
        };

        /// Succeeds if the string passed in starts with 'c'.  Consumes one
        /// byte on success.
        pub fn char(c: *const u8) Self {
            return .{
                .run = struct {
                    fn run(self: *const Self, _: ?*anyopaque, input: [:0]const u8) Ret {
                        const ch = self.dataAs(*const u8).*;
                        return if (ch == input[0])
                            input[1..]
                        else
                            error.ParseFailure;
                    }
                }.run,
                .data = c,
                .tag = .char,
            };
        }

        /// Succeeds if the string passed in starts with a byte
        /// between 'start' and 'end' inclusive.  Consumes one byte on
        /// success.
        pub fn range(rg: *const [2]u8) Self {
            assert(rg[0] < rg[1]);
            return .{
                .run = struct {
                    fn run(self: *const Self, _: ?*anyopaque, input: [:0]const u8) Ret {
                        const c = input[0];
                        const rgp = self.dataAs(*const [2]u8);
                        const start, const end = rgp.*;
                        return if (c -% start <= end - start)
                            input[1..]
                        else
                            error.ParseFailure;
                    }
                }.run,
                .data = rg,
                .tag = .range,
            };
        }

        /// Consumes one byte if 'f' succeeds.
        pub inline fn func(f: *const fn (u8) bool) Self {
            var set = CharSet.initEmpty();
            for (0..256) |c| set.setValue(c, f(@intCast(c)));
            const final = set;
            return .{
                .run = struct {
                    fn run(self: *const Self, _: ?*anyopaque, input: [:0]const u8) Ret {
                        const c = input[0];
                        return if (self.dataAs(*const CharSet).isSet(c))
                            input[1..]
                        else
                            error.ParseFailure;
                    }
                }.run,
                .data = @ptrCast(&final),
                .tag = .charset,
            };
        }

        /// Succeeds if the string passed in starts with 'c'.  Consumes one
        /// byte on success.
        pub inline fn anychar(cs: *const []const u8) Self {
            var set = CharSet.initEmpty();
            for (cs.*) |c| set.set(c);
            const final = set;
            return .{
                .run = struct {
                    fn run(self: *const Self, _: ?*anyopaque, input: [:0]const u8) Ret {
                        const cset = self.dataAs(*const CharSet).*;
                        return if (cset.isSet(input[0]))
                            input[1..]
                        else
                            error.ParseFailure;
                    }
                }.run,
                .data = @ptrCast(&final),
                .tag = .charset,
            };
        }

        /// Succeeds if the string passed in starts with any char in set.
        /// Consumes one byte on success.
        pub inline fn charset(set: *const CharSet) Self {
            return .{
                .run = struct {
                    fn run(self: *const Self, _: ?*anyopaque, input: [:0]const u8) Ret {
                        const cset = self.dataAs(*const CharSet).*;
                        return if (cset.isSet(input[0]))
                            input[1..]
                        else
                            error.ParseFailure;
                    }
                }.run,
                .data = @ptrCast(set),
                .tag = .charset,
            };
        }

        /// Succeeds if input starts with 's'.  Consumes 's' on
        /// success.
        pub fn string(s: *const []const u8) Self {
            assert(s.len != 0);
            return .{
                .run = struct {
                    fn run(self: *const Self, _: ?*anyopaque, input: [:0]const u8) Ret {
                        const str = self.dataAs(*const []const u8).*;
                        return if (mem.startsWith(u8, input, str))
                            input[str.len..]
                        else
                            error.ParseFailure;
                    }
                }.run,
                .data = @ptrCast(s),
                .tag = .string,
            };
        }

        /// Succeeds when all 'parsers' succeed feeding parser N's result
        /// into parser N+1.
        pub fn seq(parsers: *const []const Self) Self {
            return .{
                .run = struct {
                    fn run(self: *const Self, userdata: ?*anyopaque, input: [:0]const u8) Ret {
                        var in = input;
                        const ps = self.dataAs(*const []const Self);
                        for (ps.*) |*p| {
                            const mrest = p.run(p, userdata, in);
                            traceRest("seq", .{}, mrest);
                            in = try mrest;
                        }
                        return in;
                    }
                }.run,
                .data = @ptrCast(parsers),
                .tag = .seq,
            };
        }

        /// Succeeds if any of 'parsers' succeed returning the first success.
        pub fn alt(parsers: *const []const Self) Self {
            return .{
                .run = struct {
                    fn run(self: *const Self, userdata: ?*anyopaque, input: [:0]const u8) Ret {
                        const ps = self.dataAs(*const []const Self);
                        for (ps.*) |*p| {
                            const mrest = p.run(p, userdata, input);
                            traceRest("alt", .{}, mrest);
                            if (mrest) |r| return r else |_| {}
                        }
                        return error.ParseFailure;
                    }
                }.run,
                .data = @ptrCast(parsers),
                .tag = .alt,
            };
        }

        pub const Counted = struct {
            parser: Self,
            options: CountedOptions,
            pub fn init(parser: Self, coptions: CountedOptions) Counted {
                return .{ .parser = parser, .options = coptions };
            }
        };

        /// Repeatedly calls 'parser' up to 'data.max' times and succeeds
        /// if 'data.parser' succeeds 'data.min' times or more.
        pub fn counted(data: *const Counted) Self {
            return .{
                .run = struct {
                    fn run(self: *const Self, userdata: ?*anyopaque, input: [:0]const u8) Ret {
                        var count: u32 = 0;
                        var in = input;
                        const c = self.dataAs(*const Counted);
                        while (count < c.options.max) : (count += 1) {
                            const mrest = c.parser.run(&c.parser, userdata, in);
                            traceRest("cnt", .{}, mrest);
                            in = mrest catch break;
                        }
                        return if (count >= c.options.min) in else error.ParseFailure;
                    }
                }.run,
                .data = data,
                .tag = .counted,
            };
        }

        /// Always succeeds, repeatedly calling 'parser' until it fails.
        pub inline fn many(parser: Self) Self {
            var p = counted(&.{ .parser = parser, .options = .{} });
            p.tag = .many;
            return p;
        }

        /// Succeeds when 'parser' succeeds once.  Repeatedly calls 'parser'
        /// until it fails.
        pub inline fn some(parser: Self) Self {
            var p = counted(&.{ .parser = parser, .options = .{ .min = 1 } });
            p.tag = .some;
            return p;
        }

        /// Always succeeds returning the result of 'parser' when it
        /// succeeds or 'input' when it fails.
        pub fn opt(parser: *const Self) Self {
            return .{
                .run = struct {
                    fn run(self: *const Self, userdata: ?*anyopaque, input: [:0]const u8) Ret {
                        const p = self.dataAs(*const Self);
                        return p.run(p, userdata, input) catch input;
                    }
                }.run,
                .data = parser,
                .tag = .opt,
            };
        }

        /// Negative lookahead.  Inverts the result of 'parser' but consumes no
        /// input.
        pub fn not(parser: *const Self) Self {
            return .{
                .run = struct {
                    fn run(self: *const Self, userdata: ?*anyopaque, input: [:0]const u8) Ret {
                        const p = self.dataAs(*const Self);
                        return if (p.run(p, userdata, input)) |_|
                            error.ParseFailure
                        else |_|
                            input;
                    }
                }.run,
                .data = parser,
                .tag = .not,
            };
        }

        /// Positive lookahead. Returns the result of 'parser' but consumes no
        /// input.
        pub fn amp(parser: *const Self) Self {
            return .{
                .run = struct {
                    fn run(self: *const Self, userdata: ?*anyopaque, input: [:0]const u8) Ret {
                        const p = self.dataAs(*const Self);
                        _ = try p.run(p, userdata, input);
                        return input;
                    }
                }.run,
                .data = parser,
                .tag = .amp,
            };
        }

        /// Runs parser and captures output. returns 'captured' along with 'rest'.
        pub fn runAndCapture(
            p: *const Self,
            userdata: ?*anyopaque,
            input: [:0]const u8,
        ) !struct { captured: []const u8, rest: [:0]const u8 } {
            const r = try p.run(p, userdata, input);
            const len = @intFromPtr(r.ptr) - @intFromPtr(input.ptr);
            const captured = input[0..len];
            return .{ .captured = captured, .rest = r };
        }

        /// helpers for parsing int, float, enum and bool types or constants
        pub const Action = struct {
            pub const Fn = @TypeOf(nop);

            /// nop.  does nothing.
            pub fn nop(_: ?*anyopaque, _: []const u8) Error!void {}

            pub fn integer(comptime T: type, options: struct { base: u8 = 0 }) Fn {
                return struct {
                    pub fn action(userdata: ?*anyopaque, bytes: []const u8) Error!void {
                        const int = std.fmt.parseInt(T, bytes, options.base) catch
                            return error.ParseFailure;
                        trace("integer() {}\n", .{int});
                        const ud: *T = @ptrCast(userdata orelse
                            return error.MissingUserdata);
                        ud.* = int;
                    }
                }.action;
            }

            pub fn float(comptime F: type) Fn {
                return struct {
                    pub fn action(userdata: ?*anyopaque, bytes: []const u8) Error!void {
                        const e = std.fmt.parseFloat(F, bytes) catch
                            return error.ParseFailure;
                        trace("float() {}\n", .{e});
                        const ud: *F = @ptrCast(@alignCast(userdata orelse
                            return error.MissingUserdata));
                        ud.* = e;
                    }
                }.action;
            }

            pub fn enumeration(comptime E: type) Fn {
                return struct {
                    pub fn action(userdata: ?*anyopaque, bytes: []const u8) Error!void {
                        const e = std.meta.stringToEnum(E, bytes) orelse
                            return error.ParseFailure;
                        trace("enumeration() {}\n", .{e});
                        const ud: *E = @ptrCast(userdata orelse
                            return error.MissingUserdata);
                        ud.* = e;
                    }
                }.action;
            }

            pub fn boolean(userdata: ?*anyopaque, bytes: []const u8) Error!void {
                const E = enum { false, true };
                const e = std.meta.stringToEnum(E, bytes) orelse
                    return error.ParseFailure;
                trace("boolean() {}\n", .{e});
                const ud: *bool = @ptrCast(userdata orelse
                    return error.MissingUserdata);
                ud.* = @bitCast(@intFromEnum(e));
            }

            pub fn constant(comptime value: anytype) Fn {
                return struct {
                    pub fn action(userdata: ?*anyopaque, bytes: []const u8) Error!void {
                        trace("constant() {s} {}\n", .{ bytes, value });
                        const ud: *@TypeOf(value) = @ptrCast(@alignCast(userdata orelse
                            return error.MissingUserdata));
                        ud.* = value;
                    }
                }.action;
            }
        };

        /// Runs 'parser' and on success sends captured input to 'action'
        pub fn then(parser: *const Self, comptime action: *const Action.Fn) Self {
            return .{
                .run = struct {
                    fn run(self: *const Self, userdata: ?*anyopaque, input: [:0]const u8) Ret {
                        const w = try runAndCapture(self.dataAs(*const Self), userdata, input);
                        // trace("map captured {any} rest {s}\n", .{ w.captured, w.rest });
                        try action(userdata, w.captured);
                        return w.rest;
                    }
                }.run,
                .data = parser,
                .tag = .then,
            };
        }

        /// Succeeds when input starts with an enum tag name.  When some tags
        /// have a shared prefix and multiple matches are possible, such as with
        /// tags 'foo' and 'foobar', longer matches have precedence.  So given
        /// input 'foobar', the tag 'foobar' will be chosen.
        pub fn enumeration(comptime T: type) Self {
            return .{
                .run = struct {
                    const info = @typeInfo(T).Enum;

                    fn run(_: *const Self, _: ?*anyopaque, input: [:0]const u8) Ret {
                        // std.debug.print("fs {any} input '{s}'\n", .{ fs, input });
                        const smap = comptime blk: {
                            const EnumKV = struct { []const u8, T };
                            var kvs: [@typeInfo(T).Enum.fields.len]EnumKV = undefined;
                            for (@typeInfo(T).Enum.fields, 0..) |enumField, i| {
                                kvs[i] = .{ enumField.name, @field(T, enumField.name) };
                            }
                            break :blk std.StaticStringMap(T).initComptime(kvs);
                        };
                        const kv = smap.getLongestPrefix(input) orelse
                            return error.ParseFailure;
                        return input[kv.key.len..];
                    }
                }.run,
                .data = undefined,
                .tag = .enumeration,
            };
        }

        /// Calls a function to obtain its underlying parser.
        /// This introduces the indirection required for recursive grammars.
        ///
        /// ```
        /// test "ref" {
        ///    const Scope = struct {
        ///        const digit = P.range(&"19".*);
        ///        const digits = P.alt(&&.{
        ///            P.seq(&&.{ digit, P.ref(digitsRef) }),
        ///            digit,
        ///        });
        ///        fn digitsRef() P {
        ///            return digits;
        ///        }
        ///    };
        ///    try expectResult(&Scope.digits, "123", "");
        ///}
        /// ```
        pub inline fn ref(f: *const fn () Self) Self {
            return .{
                .run = struct {
                    fn run(self: *const Self, userdata: ?*anyopaque, input: [:0]const u8) Ret {
                        const fun = self.dataAs(*const fn () Self);
                        const p = fun();
                        return p.run(&p, userdata, input);
                    }
                }.run,
                .data = f,
                .tag = .ref,
            };
        }

        /// For parsing sequences with separators such as '[1,2,3]'.
        /// Returns a `seq([parser,  many(seq([sep, parser]))])` which must
        /// match 'parser' at least once.
        pub fn sepBy1(parsers: *const [2]Self) Self {
            return .{
                .run = struct {
                    fn run(self: *const Self, userdata: ?*anyopaque, input: [:0]const u8) Ret {
                        const ps = self.dataAs(*const [2]Self);
                        const parser, const sep = ps.*;
                        const p = seq(&&.{ parser, many(seq(&&.{ sep, parser })) });
                        return p.run(&p, userdata, input);
                    }
                }.run,
                .data = parsers,
                .tag = .sepby1,
            };
        }

        /// Returns an `opt(sepBy1(parser, sep))` which always succeeds.  This
        /// means it will succeed when no elements are matched.
        pub fn sepBy(parser_and_sepator: *const [2]Self) Self {
            return .{
                .run = struct {
                    fn run(self: *const Self, userdata: ?*anyopaque, input: [:0]const u8) Ret {
                        const ps = self.dataAs(*const [2]Self);
                        const p = &sepBy1(ps).opt();
                        return p.run(p, userdata, input);
                    }
                }.run,
                .data = parser_and_sepator,
                .tag = .sepby,
            };
        }

        const Until = struct {
            parser: Self,
            set: CharSet,
        };

        /// Returns input until 'parser' succeeds or eof.
        /// until() might be slow on inputs where matching characters
        /// are common in the input.  That is because it does a linear scan of
        /// input.  To compensate for this, it calculates 'parser's first set
        /// and uses that to decide when to stop scanning without needing to
        /// run 'parser'.
        pub inline fn until(parser: Self) Self {
            // calc first set once and store in data
            var set = CharSet.initEmpty();
            parser.firstSet(&set);
            const final = set;
            return .{
                .run = struct {
                    fn run(self: *const Self, userdata: ?*anyopaque, input: [:0]const u8) Ret {
                        const u = self.dataAs(*const Until);
                        trace("until set {}\n", .{CharSetFmt{ .set = u.set }});
                        var pos: usize = 0;
                        while (pos < input.len) : (pos += 1) {
                            const c = input[pos];
                            if (u.set.isSet(c)) {
                                if (u.parser.run(&u.parser, userdata, input[pos..])) |r| {
                                    trace("until '{s}'\n", .{r});
                                    break;
                                } else |_| {}
                            }
                        }
                        return input[pos..];
                    }
                }.run,
                .data = &Until{ .parser = parser, .set = final },
                .tag = .until,
            };
        }

        pub const Iterator = struct {
            input: [:0]const u8,
            parser: *const Self,
            userdata: ?*anyopaque,

            pub fn next(iter: *Iterator) ?[]const u8 {
                if (iter.input.len > 0) {
                    const p = iter.parser.until();
                    if (p.run(&p, iter.userdata, iter.input)) |r| {
                        const len = @intFromPtr(r.ptr) - @intFromPtr(iter.input.ptr);
                        const s = iter.input[0..len];
                        // parse rest with the separator parser
                        const r2 = iter.parser.run(iter.parser, iter.userdata, r) catch r;
                        const len2 = @intFromPtr(r2.ptr) - @intFromPtr(r.ptr);
                        // trace("len {} s '{s}' x '{s}'\n", .{ len, s, x });
                        iter.input = iter.input[len + len2 ..];
                        return s;
                    } else |_| {
                        iter.input = iter.input[iter.input.len..];
                    }
                }
                return null;
            }
        };

        pub fn iterator(
            parser: *const Self,
            userdata: ?*anyopaque,
            input: [:0]const u8,
        ) Iterator {
            return .{ .input = input, .parser = parser, .userdata = userdata };
        }

        pub fn isNullable(p: Self) bool {
            switch (p.tag) {
                .epsilon,
                .eos,
                .rest,
                .opt,
                .many,
                .sepby,
                .until,
                => return true,
                .not,
                .amp,
                .char,
                .charset,
                .range,
                .func,
                .string,
                .enumeration,
                => return false,
                .counted => {
                    const c = p.dataAs(*const Counted);
                    return c.options.min == 0 or c.parser.isNullable();
                },
                .some => {
                    return p.dataAs(*const Counted).parser.isNullable();
                },
                .ref => {
                    return p.dataAs(*const fn () Self)().isNullable();
                },
                .then => {
                    return p.dataAs(*const Self).isNullable();
                },
                .sepby1 => {
                    return p.dataAs(*const [2]Self)[0].isNullable();
                },
                .seq,
                => {
                    const ps = p.dataAs(*const []const Self);
                    for (ps.*) |sp| if (!sp.isNullable()) return false;
                    return true;
                },
                .alt,
                => {
                    const ps = p.dataAs(*const []const Self);
                    for (ps.*) |sp| if (sp.isNullable()) return true;
                    return false;
                },
            }
        }

        pub fn firstSet(p: Self, set: *CharSet) void {
            if (p.isNullable()) set.set(0);
            switch (p.tag) {
                .epsilon,
                .eos,
                .rest,
                => {},
                .enumeration => unreachable, // TODO
                .not,
                => {
                    var s = CharSet.initEmpty();
                    const sp = p.dataAs(*const Self);
                    sp.firstSet(&s);
                    s.toggleAll();
                    set.setUnion(s);
                },
                .amp,
                => {
                    p.dataAs(*const Self).firstSet(set);
                },
                .char => set.set(p.dataAs(*const u8).*),
                .range => {
                    const rg = p.dataAs(*const [2]u8).*;
                    set.setRangeValue(.{
                        .start = rg[0],
                        .end = @as(u16, rg[1]) + 1,
                    }, true);
                },
                .charset => {
                    const cset = p.dataAs(*const CharSet).*;
                    set.setUnion(cset);
                },
                .func => {
                    var i: u16 = 0;
                    const f = p.dataAs(*const fn (u8) bool);
                    while (i < 256) : (i += 1) {
                        if (f(@intCast(i))) set.set(i);
                    }
                },
                .string => set.set(p.dataAs(*const []const u8).*[0]),
                .until,
                => {
                    set.setUnion(p.dataAs(*const Until).set.complement());
                },
                .opt,
                => {
                    set.set(0);
                    const sp = p.dataAs(*const Self);
                    sp.firstSet(set);
                },
                .many, .some, .counted => {
                    const c = p.dataAs(*const Counted);
                    if (c.options.min == 0) set.set(0);
                    c.parser.firstSet(set);
                },
                .ref => {
                    p.dataAs(*const fn () Self)().firstSet(set);
                },
                .then => {
                    p.dataAs(*const Self).firstSet(set);
                },
                .sepby, .sepby1 => {
                    const ps = p.dataAs(*const [2]Self);
                    ps[0].firstSet(set);
                    if (ps[0].isNullable()) ps[1].firstSet(set);
                },
                .seq,
                => {
                    const ps = p.dataAs(*const []const Self);
                    for (ps.*) |sp| {
                        switch (sp.tag) {
                            .not => {
                                var s = CharSet.initEmpty();
                                sp.firstSet(&s);
                                set.setUnion(s);
                                continue;
                            },
                            .amp => {
                                sp.firstSet(set);
                                continue;
                            },
                            else => {},
                        }
                        sp.firstSet(set);
                        if (!sp.isNullable()) break;
                    }
                },
                .alt,
                => {
                    const ps = p.dataAs(*const []const Self);
                    for (ps.*) |sp| sp.firstSet(set);
                },
            }
        }

        pub const lowercase = range(&"az".*);
        pub const uppercase = range(&"AZ".*);
        pub inline fn digit(base: u8) Self {
            return range(&.{ '0', '0' - 1 + base });
        }
        pub const whitespace = blk: {
            @setEvalBranchQuota(4000);
            break :blk func(&std.ascii.isWhitespace);
        };
        pub const many_whitespace = whitespace.many();
    };
}

var trace_count: usize = 0;
fn trace(comptime fmt: []const u8, args: anytype) void {
    _ = fmt;
    _ = args;
    // std.debug.print("{d: <5} " ++ fmt, .{trace_count} ++ args);
    // trace_count += 1;
}

fn traceRest(comptime fmt: []const u8, args: anytype, mrest: anyerror![:0]const u8) void {
    const f = comptime mem.trimRight(u8, fmt, "\n");
    if (mrest) |rest| {
        const len = @min(15, rest.len);
        trace(f ++ " '{}'\n", args ++ .{std.zig.fmtEscapes(rest[0..len])});
    } else |e| {
        trace(f ++ " '{s}'\n", args ++ .{@errorName(e)});
    }
}

const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;
