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

    fn compile(self: *Transpiler) !void {
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

    pub fn compileAndRun(self: *Transpiler, allocator: std.mem.Allocator) !void {
        try self.compile();

        var gcc = std.process.Child.init(
            &[_][]const u8{ "gcc", "build/out.c", "-o", "build/out" },
            allocator,
        );
        _ = try gcc.spawnAndWait();

        var run = std.process.Child.init(
            &[_][]const u8{"./build/out"},
            allocator,
        );
        _ = try run.spawnAndWait();
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
            .stringExpr => |str| {
                try writer.print("\"{s}\"", .{str.value});
            },
            .charExpr => |c| {
                try writer.print("\'{s}\'", .{c.value});
            },
            .unary => |unary| {
                try writer.print(" {s}", .{Analyzer.mapOperator(unary.op)});
                try self.compileExpr(unary.right.*, writer);
            },
            .idExpr => |id| {
                try writer.print("{s}", .{id.name});
            },
            .callExpr => |call| {
                try writer.print("{s}( ", .{call.identifier});

                const len = call.arguments.items.len;
                for (call.arguments.items, 0..) |arg, i| {
                    try self.compileExpr(arg, writer);
                    if (i < len - 1) {
                        try writer.print(", ", .{});
                    }
                }

                try writer.print(")", .{});
            },

            else => {},
        }
    }

    fn printIndent(writer: anytype, indent: usize) !void {
        for (0..indent) |_| {
            try writer.print("    ", .{});
        }
    }

    fn compileStmt(self: *Transpiler, stmt: AstExprs.Statement, writer: anytype, indent: usize) !void {
        try writer.print("\n", .{});
        try printIndent(writer, indent);

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
                try self.compileStmt(func.returnType.*, writer, indent);
                try writer.print("{s}", .{func.name});
                try writer.print("( ", .{});

                const len = func.parameters.items.len;
                for (func.parameters.items, 0..) |item, i| {
                    try self.compileStmt(item.funcParam.typeSignature.*, writer, indent);
                    try writer.print("{s}", .{item.funcParam.identifier});

                    if (i < len - 1) {
                        try writer.print(", ", .{});
                    }
                }

                try writer.print(") ", .{});
                try writer.print("{{", .{});

                for (func.body.items) |funcStmt| {
                    try self.compileStmt(funcStmt, writer, indent + 1);
                }

                try writer.print("\n}}\n", .{});
            },
            .typeSig => |typeSig| {
                try writer.print("{s} ", .{Analyzer.mapToCType(typeSig.identifier)});
                for (0..typeSig.pointerLevel) |_| {
                    try writer.print("*", .{});
                }
            },
            .variable => |variable| {
                try self.compileStmt(variable.typeSignature.*, writer, indent);
                try writer.print("{s} ", .{variable.name});

                try writer.print("= ", .{});

                try self.compileExpr(variable.initializer, writer);

                try writer.print(";", .{});
            },
            .embed => |embed| {
                try writer.print("// start embed", .{});

                try writer.print("{s} ", .{embed.value});

                try writer.print("// end embed", .{});
            },
            .returnStmt => |ret| {
                try writer.print("return ", .{});

                try self.compileExpr(ret.value, writer);

                try writer.print(";", .{});
            },
            .callStmt => |callStmt| {
                try self.compileExpr(callStmt.call, writer);
                try writer.print(";", .{});
            },
            .assign => |assign| {
                for (0..assign.pointerLevel) |_| {
                    try writer.print("*", .{});
                }
                try writer.print("{s} = ", .{assign.identifier});

                try self.compileExpr(assign.value, writer);

                try writer.print(";", .{});
            },
            .whileStmt => |whileStmt| {
                try writer.print("while (", .{});
                try self.compileExpr(whileStmt.condition, writer);
                try writer.print(") ", .{});

                try writer.print("{{\n", .{});

                for (whileStmt.body.items) |x| {
                    try self.compileStmt(x, writer, indent + 1);
                }

                try self.compileStmt(whileStmt.alteration.*, writer, indent + 1);

                try writer.print("\n", .{});
                try printIndent(writer, indent);
                try writer.print("}}", .{});
            },
            .doStmt => |do| {
                try writer.print("for (long i = 0; i < ", .{});
                try self.compileExpr(do.expression, writer);
                try writer.print(" ;", .{});
                try writer.print("  i++) {{\n", .{});

                for (do.body.items) |item| {
                    try self.compileStmt(item, writer, indent + 1);
                }

                try writer.print("\n", .{});
                try printIndent(writer, indent);
                try writer.print("}}", .{});
            },
            .nextStmt => |_| {
                try writer.print("continue;", .{});
            },
            .stopStmt => |_| {
                try writer.print("break;", .{});
            },
            .ifStmt => |ifStmt| {
                try writer.print("if (", .{});
                try self.compileExpr(ifStmt.condition, writer);
                try writer.print(")", .{});

                try writer.print("{{\n", .{});

                for (ifStmt.body.items) |item| {
                    try self.compileStmt(item, writer, indent + 1);
                }

                try writer.print("}}", .{});
            },
            .funcParam => |param| {
                try self.compileStmt(param.typeSignature.*, writer, indent + 1);
                try writer.print("{s}", .{param.identifier});
                try writer.print(", ", .{});
            },
            .structDecl => |structDecl| {
                try writer.print("struct {s} {{\n", .{structDecl.identifier});
                if (structDecl.members.items.len == 0) {
                    try printIndent(writer, indent + 1);
                    try writer.print("int x;\n", .{});
                }

                for (structDecl.members.items) |member| {
                    try self.compileStmt(member, writer, indent + 1);
                }

                try writer.print("\n", .{});
                try printIndent(writer, indent);
                try writer.print("}};", .{});
            },
            .structMem => |member| {
                try self.compileStmt(member.typeSignature.*, writer, indent);
                try writer.print("{s};", .{member.identifier});
            },
            .badStmt => {},
            .none => {},
            .attribute => |_| {},
        }
    }
};
