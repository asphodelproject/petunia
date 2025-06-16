const AstExprs = @import("../parse/ast.zig");
const TokenKind = @import("../tokenize/token_kind.zig").TokenKind;
const std = @import("std");
const Analyzer = @import("../analyze/analyzer.zig").Analyzer;

const Petunia = @import("../petunia.zig").Petunia;

pub const Transpiler = struct {
    exprs: std.ArrayList(AstExprs.Statement),

    pub fn new(exprs: std.ArrayList(AstExprs.Statement)) Transpiler {
        return Transpiler{
            .exprs = exprs,
        };
    }

    pub fn compile(self: *Transpiler) !void {
        const cwd = std.fs.cwd();
        const file = try cwd.createFile("build/out.c", .{ .truncate = true });
        defer file.close();

        const writer = file.writer();
        try writer.writeAll(Petunia.compilerGeneratedMessage);

        try writer.writeAll("\n#include <stdio.h>\n\n");

        for (self.exprs.items) |expr| {
            try self.compileStmt(expr, writer, 0);
        }
    }

    fn compileExpr(self: *Transpiler, expr: AstExprs.Expression, writer: anytype) !void {
        _ = self;

        switch (expr) {
            .intExpr => |int| {
                try writer.print("{}", .{int.value});
            },
            else => {},
        }
    }

    fn compileStmt(self: *Transpiler, stmt: AstExprs.Statement, writer: anytype, indent: usize) !void {
        try writer.print("\n", .{});
        for (0..indent) |_| {
            try writer.print("\t", .{});
        }

        switch (stmt) {
            .function => |func| {
                if (!func.isPublic) {
                    try writer.print("static ", .{});
                }
                for (func.attributes.items) |attr| {
                    switch (attr) {
                        .inlineAttribute => {
                            try writer.print("inline ", .{});
                        },
                        else => {},
                    }
                }
                try writer.print("{s} {s}() {{", .{ Analyzer.mapToCType(func.returnType), func.name });

                for (func.body.items) |funcStmt| {
                    try self.compileStmt(funcStmt, writer, indent + 1);
                }

                try writer.print("\n}}", .{});
            },
            .badStmt => {},
            .attribute => |_| {},
            .variable => |variable| {
                try writer.print("{s} ", .{Analyzer.mapToCType(variable.typeAnnotation)});
                try writer.print("{s} ", .{variable.name});

                try writer.print("= ", .{});

                try self.compileExpr(variable.initializer, writer);

                try writer.print(";\n", .{});
            },
        }
    }
};
