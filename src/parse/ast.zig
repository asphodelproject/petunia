const std = @import("std");
const TokenKind = @import("../tokenize//token_kind.zig").TokenKind;

pub const Ast = struct {
    program: []const Expression,
};

pub const Statement = union(enum) { function: FunctionDeclaration, badStmt: BadStatement, attribute: AttributeStatement, variable: VariableDeclaration };

pub const AttributeStatement = union(enum) {
    inlineAttribute: void,
    entryAttribute: void,
    cTypeAttribute: []const u8,
};

pub const BadStatement = struct {
    pub fn new() Statement {
        return Statement{
            .badStmt = BadStatement{},
        };
    }
};

pub const VariableDeclaration = struct {
    name: []const u8,
    typeAnnotation: []const u8,
    initializer: Expression,
    isConstant: bool,

    pub fn new(name: []const u8, typeAnnotation: []const u8, initializer: Expression, isConstant: bool) Statement {
        return Statement{
            .variable = VariableDeclaration{
                .name = name,
                .initializer = initializer,
                .typeAnnotation = typeAnnotation,
                .isConstant = isConstant,
            },
        };
    }
};

pub const FunctionDeclaration = struct {
    name: []const u8,
    returnType: []const u8,
    isPublic: bool,
    attributes: std.ArrayList(AttributeStatement),
    body: std.ArrayList(Statement),

    pub fn new(
        name: []const u8,
        returnType: []const u8,
        isPublic: bool,
        attributes: std.ArrayList(AttributeStatement),
        body: std.ArrayList(Statement),
    ) Statement {
        return Statement{
            .function = FunctionDeclaration{
                .name = name,
                .returnType = returnType,
                .isPublic = isPublic,
                .attributes = attributes,
                .body = body,
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
