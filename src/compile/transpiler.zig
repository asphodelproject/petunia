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

        try writer.writeAll("\n#include <stdio.h>\n");
        try writer.writeAll("#include <stdbool.h>\n");

        for (self.exprs.items) |expr| {
            try self.compileStmt(expr, writer, 0);
        }
    }

    fn compileExpr(self: *Transpiler, expr: AstExprs.Expression, writer: anytype) !void {
        switch (expr) {
            .intExpr => |int| {
                try writer.print("{}", .{int.value});
            },
            .binary => |bin| {
                try self.compileExpr(bin.left.*, writer);
                try writer.print(" {s} ", .{Analyzer.mapOperator(bin.op)});

                try self.compileExpr(bin.right.*, writer);
            },
            .unary => |unary| {
                try writer.print(" {s}", .{Analyzer.mapOperator(unary.op)});
                try self.compileExpr(unary.right.*, writer);
            },
            .idExpr => |id| {
                try writer.print("{s}", .{id.name});
            },
            .callExpr => |call| {
                try writer.print("{s}()", .{call.identifier});
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
            .variable => |variable| {
                try writer.print("{s} ", .{Analyzer.mapToCType(variable.typeAnnotation)});
                for (0..variable.pointerLevel) |_| {
                    try writer.print("*", .{});
                }
                try writer.print("{s} ", .{variable.name});

                try writer.print("= ", .{});

                try self.compileExpr(variable.initializer, writer);

                try writer.print(";", .{});
            },
            .embed => |embed| {
                try writer.print("\n// start embed", .{});

                try writer.print("{s} ", .{embed.value});

                try writer.print("// end embed\n", .{});
            },
            .returnStmt => |ret| {
                try writer.print("return ", .{});

                try self.compileExpr(ret.value, writer);

                try writer.print(";", .{});
            },
            .callStmt => |callStmt| {
                try writer.print("{s}() ", .{callStmt.call.identifier});
                try writer.print(";", .{});
            },
            .assign => |assign| {
                try writer.print("{s} = ", .{assign.identifier});

                try self.compileExpr(assign.value, writer);

                try writer.print(";", .{});
            },
            .badStmt => {},
            .attribute => |_| {},
        }
    }
};
