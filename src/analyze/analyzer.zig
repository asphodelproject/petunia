const std = @import("std");
const AstExprs = @import("../parse/ast.zig");

pub const Analyzer = struct {
    exprs: std.ArrayList(AstExprs.Expression),

    pub fn new(exprs: std.ArrayList(AstExprs.Expression)) Analyzer {
        return Analyzer{ .exprs = exprs };
    }

    pub fn analyze(self: *Analyzer) void {
        for (self.exprs.items) |expr| {
            self.analyzeExpr(expr);
        }
    }

    pub fn analyzeExpr(self: *Analyzer, expr: AstExprs.Expression) void {
        _ = self;

        switch (expr) {
            .intExpr => |_| {},
            .idExpr => |_| {},
            .binExpr => |_| {},
            else => {},
        }
    }
};
