const std = @import("std");

pub fn build(b: *std.Build) void {
    const mod = b.addModule("combinado", .{
        .root_source_file = b.path("src/combinado.zig"),
    });

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // tests
    const tests = b.addTest(.{
        .root_source_file = b.path("src/tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    tests.root_module.addImport("combinado", mod);
    const test_filter = b.option([]const u8, "test-filter", "");
    tests.filters = if (test_filter) |tf|
        b.allocator.dupe([]const u8, &.{tf}) catch @panic("OOM")
    else
        &.{};
    const install_test = b.addInstallArtifact(tests, .{});
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&install_test.step);
    b.getInstallStep().dependOn(&install_test.step);
    const run_tests = b.addRunArtifact(tests);
    run_tests.has_side_effects = true;
    test_step.dependOn(&run_tests.step);

    // json
    const json = b.addExecutable(.{
        .name = "json",
        .root_source_file = b.path("src/json.zig"),
        .target = target,
        .optimize = optimize,
    });
    json.root_module.addImport("combinado", mod);
    b.installArtifact(json);
    const json_run = b.addRunArtifact(json);
    if (b.args) |args| json_run.addArgs(args);
    const json_run_step = b.step("json", "Run the json parser");
    json_run_step.dependOn(&json_run.step);
}
