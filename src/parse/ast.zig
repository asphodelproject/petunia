const std = @import("std");
const TokenKind = @import("../tokenize//token_kind.zig").TokenKind;

pub const Statement = union(enum) {
    function: FunctionDeclaration,
    badStmt: BadStatement,
    attribute: AttributeStatement,
    variable: VariableDeclaration,
    embed: EmbedStatement,
};

pub const AttributeStatement = union(enum) {
    inlineAttribute: void,
    entryAttribute: void,
    cTypeAttribute: []const u8,
};

pub const EmbedStatement = struct {
    value: []const u8,

    pub fn new(value: []const u8) Statement {
        return Statement{
            .embed = EmbedStatement{
                .value = value,
            },
        };
    }
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
    pointerLevel: u8,

    pub fn new(name: []const u8, typeAnnotation: []const u8, initializer: Expression, isConstant: bool, pointerLevel: u8) Statement {
        return Statement{
            .variable = VariableDeclaration{
                .name = name,
                .initializer = initializer,
                .typeAnnotation = typeAnnotation,
                .isConstant = isConstant,
                .pointerLevel = pointerLevel,
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
    binary: BinaryExpression,
    unary: UnaryExpression,
    unknown: UnknownExpression,
    boolExpr: BooleanExpression,
};

pub const BinaryExpression = struct {
    left: *const Expression,
    op: TokenKind,
    right: *const Expression,

    pub fn new(left: *const Expression, op: TokenKind, right: *const Expression) Expression {
        return Expression{
            .binary = BinaryExpression{
                .left = left,
                .op = op,
                .right = right,
            },
        };
    }
};

pub const UnaryExpression = struct {
    op: TokenKind,
    right: *const Expression,

    pub fn new(op: TokenKind, right: *const Expression) Expression {
        return Expression{
            .unary = UnaryExpression{
                .op = op,
                .right = right,
            },
        };
    }
};

pub const BooleanExpression = struct {
    isTrue: bool,

    pub fn new(isTrue: bool) Expression {
        return Expression{
            .boolExpr = BooleanExpression{
                .isTrue = isTrue,
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
