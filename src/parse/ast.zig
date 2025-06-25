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
    whileStmt: WhileStatement,
    none: NoneStatement,
    typeSig: TypeSignature,
    nextStmt: NextStatement,
    stopStmt: StopStatement,
    funcParam: FunctionParameter,
    ifStmt: IfStatement,
    doStmt: DoStatement,
    structMem: StructMember,
    structDecl: StructDeclaration,
    structAlloc: StructAllocation,
    structParam: StructParameter,
};

pub const Expression = union(enum) {
    intExpr: IntegerExpression,
    idExpr: IdentifierExpression,
    stringExpr: StringExpression,
    charExpr: CharExpression,
    binary: BinaryExpression,
    unary: UnaryExpression,
    unknown: UnknownExpression,
    boolExpr: BooleanExpression,
    callExpr: FunctionCallExpression,
    propAccess: PropertyAccessExpression,
};

pub const AttributeStatement = union(enum) {
    inlineAttribute: void,
    entryAttribute: void,
    interfaceAttribute: void,
    selfAttribute: void,
    cTypeAttribute: []const u8,
};

pub const PropertyAccessExpression = struct {
    object: *Expression,
    property: []const u8,
    accessor: []const u8,

    pub fn new(object: *Expression, property: []const u8, accessor: []const u8) Expression {
        return Expression{
            .propAccess = PropertyAccessExpression{
                .object = object,
                .property = property,
                .accessor = accessor,
            },
        };
    }
};

pub const StructParameter = struct {
    identifier: []const u8,
    expression: *Expression,

    pub fn new(identifier: []const u8, expression: *Expression) Statement {
        return Statement{
            .structParam = StructParameter{
                .identifier = identifier,
                .expression = expression,
            },
        };
    }
};

pub const StructAllocation = struct {
    identifier: []const u8,
    typeSignature: *Statement,
    parameters: std.ArrayList(Statement),

    pub fn new(identifier: []const u8, typeSignature: *Statement, parameters: std.ArrayList(Statement)) Statement {
        return Statement{
            .structAlloc = StructAllocation{
                .identifier = identifier,
                .typeSignature = typeSignature,
                .parameters = parameters,
            },
        };
    }
};

pub const StructDeclaration = struct {
    identifier: []const u8,
    isPublic: bool,
    members: std.ArrayList(Statement),

    pub fn new(identifier: []const u8, isPublic: bool, members: std.ArrayList(Statement)) Statement {
        return Statement{
            .structDecl = StructDeclaration{
                .identifier = identifier,
                .isPublic = isPublic,
                .members = members,
            },
        };
    }
};

pub const StructMember = struct {
    identifier: []const u8,
    typeSignature: *Statement,

    pub fn new(identifier: []const u8, typeSignature: *Statement) Statement {
        return Statement{
            .structMem = StructMember{
                .identifier = identifier,
                .typeSignature = typeSignature,
            },
        };
    }
};

pub const DoStatement = struct {
    expression: Expression,
    body: std.ArrayList(Statement),

    pub fn new(expression: Expression, body: std.ArrayList(Statement)) Statement {
        return Statement{
            .doStmt = DoStatement{
                .body = body,
                .expression = expression,
            },
        };
    }
};

pub const IfStatement = struct {
    condition: Expression,
    body: std.ArrayList(Statement),

    pub fn new(condition: Expression, body: std.ArrayList(Statement)) Statement {
        return Statement{
            .ifStmt = IfStatement{
                .condition = condition,
                .body = body,
            },
        };
    }
};

pub const FunctionParameter = struct {
    identifier: []const u8,
    typeSignature: *Statement,

    pub fn new(identifier: []const u8, typeSignature: *Statement) Statement {
        return Statement{
            .funcParam = FunctionParameter{
                .identifier = identifier,
                .typeSignature = typeSignature,
            },
        };
    }
};

pub const TypeSignature = struct {
    identifier: []const u8,
    pointerLevel: u8,

    pub fn new(identifier: []const u8, pointerLevel: u8) Statement {
        return Statement{
            .typeSig = TypeSignature{
                .identifier = identifier,
                .pointerLevel = pointerLevel,
            },
        };
    }
};

pub const NextStatement = struct {
    pub fn new() Statement {
        return Statement{
            .nextStmt = NextStatement{},
        };
    }
};

pub const StopStatement = struct {
    pub fn new() Statement {
        return Statement{
            .stopStmt = StopStatement{},
        };
    }
};

pub const WhileStatement = struct {
    condition: Expression,
    alteration: *const Statement,
    body: std.ArrayList(Statement),

    pub fn new(condition: Expression, body: std.ArrayList(Statement), alteration: *const Statement) Statement {
        return Statement{
            .whileStmt = WhileStatement{
                .condition = condition,
                .body = body,
                .alteration = alteration,
            },
        };
    }
};

pub const AssignmentStatement = struct {
    identifier: []const u8,
    pointerLevel: u8,
    value: Expression,

    pub fn new(identifier: []const u8, pointerLevel: u8, value: Expression) Statement {
        return Statement{
            .assign = AssignmentStatement{
                .identifier = identifier,
                .pointerLevel = pointerLevel,
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
    initializer: Expression,
    isConstant: bool,
    typeSignature: *Statement,

    pub fn new(name: []const u8, initializer: Expression, isConstant: bool, typeSignature: *Statement) Statement {
        return Statement{
            .variable = VariableDeclaration{
                .name = name,
                .initializer = initializer,
                .isConstant = isConstant,
                .typeSignature = typeSignature,
            },
        };
    }
};

pub const FunctionDeclaration = struct {
    name: []const u8,
    returnType: *Statement,
    isPublic: bool,
    attributes: std.ArrayList(AttributeStatement),
    body: std.ArrayList(Statement),
    parameters: std.ArrayList(Statement),

    pub fn new(
        name: []const u8,
        returnType: *Statement,
        isPublic: bool,
        attributes: std.ArrayList(AttributeStatement),
        body: std.ArrayList(Statement),
        parameters: std.ArrayList(Statement),
    ) Statement {
        return Statement{
            .function = FunctionDeclaration{
                .name = name,
                .returnType = returnType,
                .isPublic = isPublic,
                .attributes = attributes,
                .body = body,
                .parameters = parameters,
            },
        };
    }
};

pub const FunctionCallStatement = struct {
    call: Expression,

    pub fn new(identifier: []const u8, arguments: std.ArrayList(Expression)) Statement {
        return Statement{
            .callStmt = FunctionCallStatement{
                .call = Expression{
                    .callExpr = FunctionCallExpression{
                        .identifier = identifier,
                        .arguments = arguments,
                    },
                },
            },
        };
    }
};

pub const FunctionCallExpression = struct {
    identifier: []const u8,
    arguments: std.ArrayList(Expression),

    pub fn new(identifier: []const u8, arguments: std.ArrayList(Expression)) Expression {
        return Expression{
            .callExpr = FunctionCallExpression{
                .identifier = identifier,
                .arguments = arguments,
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

pub const CharExpression = struct {
    value: []const u8,

    pub fn new(value: []const u8) Expression {
        return Expression{
            .charExpr = CharExpression{
                .value = value,
            },
        };
    }
};

pub const StringExpression = struct {
    value: []const u8,

    pub fn new(value: []const u8) Expression {
        return Expression{
            .stringExpr = StringExpression{
                .value = value,
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

pub const NoneStatement = struct {
    pub fn new() Statement {
        return Statement{
            .none = NoneStatement{},
        };
    }
};

pub const BadStatement = struct {
    lexeme: []const u8,

    pub fn new(lexeme: []const u8) Statement {
        return Statement{
            .badStmt = BadStatement{
                .lexeme = lexeme,
            },
        };
    }
};
