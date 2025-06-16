const std = @import("std");
const AstExprs = @import("../parse/ast.zig");

pub const Analyzer = struct {
    exprs: std.ArrayList(AstExprs.Statement),

    pub fn new(exprs: std.ArrayList(AstExprs.Statement)) Analyzer {
        return Analyzer{ .exprs = exprs };
    }

    pub fn analyze(self: *Analyzer) void {
        self.validateEntryPoint();

        for (self.exprs.items) |expr| {
            self.analyzeExpr(expr);
        }
    }

    fn validateEntryPoint(self: *Analyzer) void {
        _ = self;
    }

    fn analyzeExpr(self: *Analyzer, expr: AstExprs.Statement) void {
        _ = self;

        switch (expr) {
            else => {},
        }
    }

    pub fn mapToCType(datatype: []const u8) []const u8 {
        if (std.mem.eql(u8, datatype, "u0")) {
            return "void";
        } else if (std.mem.eql(u8, datatype, "bool")) {
            return "_Bool";
        } else if (std.mem.eql(u8, datatype, "u8")) {
            return "unsigned char";
        } else if (std.mem.eql(u8, datatype, "i8")) {
            return "signed char";
        } else if (std.mem.eql(u8, datatype, "u16")) {
            return "unsigned short";
        } else if (std.mem.eql(u8, datatype, "i16")) {
            return "signed short";
        } else if (std.mem.eql(u8, datatype, "i32")) {
            return "signed int";
        } else if (std.mem.eql(u8, datatype, "u32")) {
            return "unsigned int";
        } else if (std.mem.eql(u8, datatype, "i64")) {
            return "signed long";
        } else if (std.mem.eql(u8, datatype, "i32")) {
            return "unsigned long";
        } else if (std.mem.eql(u8, datatype, "f32")) {
            return "float";
        } else if (std.mem.eql(u8, datatype, "f64")) {
            return "double";
        } else if (std.mem.eql(u8, datatype, "size")) {
            return "size_t";
        }

        return "";
    }
};
