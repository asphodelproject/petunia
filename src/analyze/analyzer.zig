const std = @import("std");
const AstExprs = @import("../parse/ast.zig");

const Token = @import("../tokenize/token.zig").Token;
const TokenKind = @import("../tokenize/token_kind.zig").TokenKind;

pub const Analyzer = struct {
    exprs: std.ArrayList(AstExprs.Statement),

    pub fn new(exprs: std.ArrayList(AstExprs.Statement)) Analyzer {
        return Analyzer{
            .exprs = exprs,
        };
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

    // not used yet..
    pub fn inferNumberSizeToCType(number: i64) []const u8 {
        return if (number >= -128 and number <= 127)
            "signed char"
        else if (number >= -32768 and number <= 32767)
            "signed short"
        else if (number >= -2147483648 and number <= 2147483647)
            "signed int"
        else
            "signed long";
    }

    pub fn mapOperator(op: TokenKind) []const u8 {
        return switch (op) {
            TokenKind.PLUS => "+",
            TokenKind.MINUS => "-",
            TokenKind.STAR => "*",
            TokenKind.SLASH => "/",
            TokenKind.MODULO => "%",
            TokenKind.GREATER_THAN => ">",
            TokenKind.LESS_THAN => "<",
            TokenKind.LESS_THAN_EQUALS => "<=",
            TokenKind.GREATER_THAN_EQUALS => ">=",
            TokenKind.DOUBLE_EQUALS => "==",
            TokenKind.NOT_EQUALS => "!=",
            TokenKind.EXCLAMATION => "!",
            TokenKind.NOT => "!",
            TokenKind.AND => "&&",
            TokenKind.OR => "||",
            TokenKind.AMPERSAND => "&",
            TokenKind.TILDE => "~",
            TokenKind.CARET => "^",
            TokenKind.PIPE => "|",
            TokenKind.LEFT_SHIFT => "<<",
            TokenKind.RIGHT_SHIFT => ">>",
            else => "",
        };
    }

    pub fn mapTokenToCType(token: Token) []const u8 {
        return switch (token.kind) {
            TokenKind.INTEGER => "signed int",
            else => "",
        };
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
