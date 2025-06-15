const AstExprs = @import("../parse/ast.zig");
const TokenKind = @import("../tokenize/token_kind.zig").TokenKind;
const std = @import("std");

const Petunia = @import("../petunia.zig").Petunia;

pub const Transpiler = struct {
    exprs: std.ArrayList(AstExprs.Expression),

    pub fn new(exprs: std.ArrayList(AstExprs.Expression)) Transpiler {
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

        for (self.exprs.items) |expr| {
            try self.compileExpr(expr, writer);
        }
    }

    fn compileExpr(self: *Transpiler, expr: AstExprs.Expression, writer: anytype) !void {
        switch (expr) {
            .intExpr => |intExpr| {
                try writer.print("{}", .{intExpr.value});
            },
            .binExpr => |binExpr| {
                try self.compileExpr(binExpr.left.*, writer);
                try writer.print(" {s} ", .{TokenKind.binOpToStr(binExpr.op)});
                try self.compileExpr(binExpr.right.*, writer);
            },
            else => {},
        }
    }
};
