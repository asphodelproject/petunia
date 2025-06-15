const TokenKind = @import("../tokenize//token_kind.zig").TokenKind;

pub const Ast = struct {
    program: []const Expression,
};

pub const Statement = union(enum) {
    function: FunctionDefinition,
};

pub const FunctionDefinition = struct {
    name: []const u8,

    pub fn new(name: []const u8) Statement {
        return Statement{
            .function = FunctionDefinition{
                .name = name,
            },
        };
    }
};

pub const Expression = union(enum) {
    intExpr: IntegerExpression,
    idExpr: IdentifierExpression,
    binExpr: BinaryExpression,
    unknown: UnknownExpression,
};

pub const BinaryExpression = struct {
    left: *const Expression,
    op: TokenKind,
    right: *const Expression,

    pub fn new(left: *const Expression, op: TokenKind, right: *const Expression) Expression {
        return Expression{
            .binExpr = BinaryExpression{
                .left = left,
                .op = op,
                .right = right,
            },
        };
    }
};

pub const IdentifierExpression = struct {
    name: []const u8,

    pub fn new(name: []const u8) Expression {
        return Expression{
            .idExpr = IdentifierExpression{
                .name = name,
            },
        };
    }
};

pub const IntegerExpression = struct {
    value: i64,

    pub fn new(value: i64) Expression {
        return Expression{
            .intExpr = IntegerExpression{
                .value = value,
            },
        };
    }
};

pub const UnknownExpression = struct {};
