const std = @import("std");
const TokenKind = @import("../tokenize//token_kind.zig").TokenKind;

pub const Statement = union(enum) {
    function: FunctionDeclaration,
    badStmt: BadStatement,
    attribute: AttributeStatement,
    variable: VariableDeclaration,
    embed: EmbedStatement,
    returnStmt: ReturnStatement,
    callStmt: FunctionCallStatement,
    assign: AssignmentStatement,
};

pub const Expression = union(enum) {
    intExpr: IntegerExpression,
    idExpr: IdentifierExpression,
    binary: BinaryExpression,
    unary: UnaryExpression,
    unknown: UnknownExpression,
    boolExpr: BooleanExpression,
    callExpr: FunctionCallExpression,
};

pub const AttributeStatement = union(enum) {
    inlineAttribute: void,
    entryAttribute: void,
    cTypeAttribute: []const u8,
};

pub const AssignmentStatement = struct {
    identifier: []const u8,
    value: Expression,

    pub fn new(identifier: []const u8, value: Expression) Statement {
        return Statement{
            .assign = AssignmentStatement{
                .identifier = identifier,
                .value = value,
            },
        };
    }
};

pub const ReturnStatement = struct {
    value: Expression,

    pub fn new(value: Expression) Statement {
        return Statement{
            .returnStmt = ReturnStatement{
                .value = value,
            },
        };
    }
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

pub const FunctionCallStatement = struct {
    call: FunctionCallExpression,

    pub fn new(identifier: []const u8) Statement {
        return Statement{
            .callStmt = FunctionCallStatement{
                .call = FunctionCallExpression{
                    .identifier = identifier,
                },
            },
        };
    }
};

pub const FunctionCallExpression = struct {
    identifier: []const u8,

    pub fn new(identifier: []const u8) Expression {
        return Expression{
            .callExpr = FunctionCallExpression{
                .identifier = identifier,
            },
        };
    }
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

pub const BadStatement = struct {
    pub fn new() Statement {
        return Statement{
            .badStmt = BadStatement{},
        };
    }
};
