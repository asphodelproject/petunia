const AstExprs = @import("ast.zig");

const Token = @import("../tokenize/token.zig").Token;
const TokenKind = @import("../tokenize/token_kind.zig").TokenKind;

const std = @import("std");

const ParseError = error{
    Expected,
    UnexpectedToken,
} || std.mem.Allocator.Error;

pub const Parser = struct {
    current: usize,
    tokens: std.ArrayList(Token),
    exprs: std.ArrayList(AstExprs.Statement),
    allocator: std.mem.Allocator,

    pub fn new(tokens: std.ArrayList(Token), allocator: std.mem.Allocator) ParseError!Parser {
        return Parser{
            .current = 0,
            .tokens = tokens,
            .exprs = std.ArrayList(AstExprs.Statement).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn print(self: *Parser) void {
        std.debug.print("\n", .{});
        for (self.exprs.items) |expr| {
            self.printStmt(expr, 0);
            std.debug.print("\n", .{});
        }
    }

    fn printIndent(indent: usize) void {
        for (0..indent) |_| std.debug.print(" ", .{});
    }

    fn printStmt(self: *Parser, expr: AstExprs.Statement, indent: usize) void {
        std.debug.print("\n", .{});
        printIndent(indent);

        switch (expr) {
            // .intExpr => |intExpr| {
            //     std.debug.print("{}\n", .{intExpr.value});
            // },
            // .idExpr => |idExpr| {
            //     std.debug.print("{s}\n", .{idExpr.name});
            // },
            // .binExpr => |binExpr| {
            //     std.debug.print("Binary Expression:\n", .{});

            //     printIndent(indent);
            //     std.debug.print("Operator: {s}\n", .{@tagName(binExpr.op)});

            //     printIndent(indent);
            //     std.debug.print("Left:\n", .{});
            //     self.printExpr(binExpr.left.*, indent + 2);

            //     printIndent(indent);
            //     std.debug.print("Right:\n", .{});
            //     self.printExpr(binExpr.right.*, indent + 2);
            // },
            .function => |func| {
                std.debug.print("Function Definition:\n", .{});
                printIndent(indent + 2);
                std.debug.print("Name: {s}\n", .{func.name});
                printIndent(indent + 2);
                std.debug.print("Is Public: {}\n", .{func.isPublic});

                printIndent(indent + 2);
                std.debug.print("Attributes: \n", .{});
                for (func.attributes.items) |attr| {
                    printIndent(indent + 4);
                    switch (attr) {
                        .cTypeAttribute => |ctype| {
                            std.debug.print("ctype: {s}\n", .{ctype});
                        },
                        .entryAttribute => |_| {
                            std.debug.print("entry\n", .{});
                        },
                        .inlineAttribute => |_| {
                            std.debug.print("inline\n", .{});
                        },
                        .interfaceAttribute => |_| {
                            std.debug.print("interface\n", .{});
                        },
                        .selfAttribute => |_| {
                            std.debug.print("interface\n", .{});
                        },
                    }
                }

                for (func.body.items) |stmt| {
                    self.printStmt(stmt, indent + 2);
                }
            },
            .badStmt => |bad| {
                std.debug.print("Bad Statement: '{s}'", .{bad.lexeme});
            },
            .attribute => |_| {
                std.debug.print("Attribute", .{});
            },
            .returnStmt => |_| {
                std.debug.print("Return Statement", .{});
            },
            .variable => |_| {
                std.debug.print("Variable Declaration", .{});
            },
            .embed => |_| {
                std.debug.print("Embed Statement", .{});
            },
            .callStmt => |_| {
                std.debug.print("Call Statement", .{});
            },
            .assign => |_| {
                std.debug.print("Assignment Statement", .{});
            },
            .whileStmt => |_| {
                std.debug.print("While Statement", .{});
            },
            .nextStmt => |_| {
                std.debug.print("Next Statement", .{});
            },
            .stopStmt => |_| {
                std.debug.print("Stop Statement", .{});
            },
            .funcParam => |_| {
                std.debug.print("Func Parameter", .{});
            },
            .ifStmt => |_| {
                std.debug.print("If Statement", .{});
            },
            .doStmt => |_| {
                std.debug.print("Do Statement", .{});
            },
            .structDecl => |_| {
                std.debug.print("Struct Declaration", .{});
            },
            .structAlloc => |_| {
                std.debug.print("Struct Allocation", .{});
            },
            .structParam => |_| {
                std.debug.print("Struct Parameter", .{});
            },
            .structMem => |_| {
                std.debug.print("Struct Member", .{});
            },
            .typeSig => |_| {
                std.debug.print("Type Signature", .{});
            },
            .none => |_| {
                std.debug.print("None", .{});
            },
        }
    }

    pub fn constructAst(self: *Parser) ParseError!std.ArrayList(AstExprs.Statement) {
        while (!self.isEnd()) {
            const stmt = try self.parseStatement();
            switch (stmt.*) {
                .badStmt => return self.exprs,
                else => {
                    try self.exprs.append(stmt.*);
                },
            }
        }

        return self.exprs;
    }

    fn parseStatement(self: *Parser) ParseError!*AstExprs.Statement {
        while (self.match(TokenKind.NEWLINE)) {
            self.advance();
            continue;
        }

        var attributes = std.ArrayList(AstExprs.AttributeStatement).init(self.allocator);

        while (self.match(TokenKind.AT)) {
            const attr = try self.parseAttribute();
            try attributes.append(attr.*.attribute);
        }

        const stmt = switch (self.currentToken().kind) {
            TokenKind.FN => try self.parseFunction(attributes),
            TokenKind.PUB => try self.parsePub(attributes),
            TokenKind.LET => try self.parseLet(),
            TokenKind.EMBED => try self.parseEmbed(),
            TokenKind.RETURN => try self.parseReturn(),
            TokenKind.IDENTIFIER => try self.parseIdentifier(),
            TokenKind.STAR => try self.parseAssignExpr(),
            TokenKind.WHILE => try self.parseWhile(),
            TokenKind.NEXT => try self.parseNext(),
            TokenKind.STOP => try self.parseStop(),
            TokenKind.IF => try self.parseIf(),
            TokenKind.DO => try self.parseDo(),
            TokenKind.STRUCT => try self.parseStruct(),
            else => try self.badStmt(),
        };

        return stmt;
    }

    fn parseStruct(self: *Parser) ParseError!*AstExprs.Statement {
        var isPub: bool = false;

        if (self.match(TokenKind.PUB)) {
            self.advance();
            isPub = true;
        }

        self.advance();

        const idToken = self.currentToken();
        self.advance();

        if (!self.expect(TokenKind.NEWLINE)) {
            return self.badStmt();
        }

        var members = std.ArrayList(AstExprs.Statement).init(self.allocator);
        while (!self.match(TokenKind.END)) {
            const memberIdentifier = self.currentToken();
            self.advance();

            self.advance(); // :

            const typeSignature = try self.parseTypeSignature();

            const member = try self.new_stmt();
            member.* = AstExprs.StructMember.new(memberIdentifier.lexeme, typeSignature);
            try members.append(member.*);

            self.advance();
        }

        if (!self.expect(TokenKind.END)) {
            return self.badStmt();
        }

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.StructDeclaration.new(idToken.lexeme, isPub, members);
        return stmt;
    }

    fn parseDo(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        const expression = try self.parseExpression();

        if (!self.expect(TokenKind.NEWLINE)) {
            return self.badStmt();
        }

        var body = std.ArrayList(AstExprs.Statement).init(self.allocator);
        while (!self.match(TokenKind.END)) {
            const stmt = try self.parseStatement();
            try body.append(stmt.*);
        }

        if (!self.expect(TokenKind.END)) {
            return self.badStmt();
        }

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.DoStatement.new(expression.*, body);
        return stmt;
    }

    fn parseIf(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        const condition = try self.parseExpression();

        if (!self.expect(TokenKind.THEN)) {
            return self.badStmt();
        }

        if (!self.expect(TokenKind.NEWLINE)) {
            return self.badStmt();
        }

        var body = std.ArrayList(AstExprs.Statement).init(self.allocator);
        while (!self.match(TokenKind.END)) {
            const stmt = try self.parseStatement();
            try body.append(stmt.*);
        }

        if (!self.expect(TokenKind.END)) {
            return self.badStmt();
        }

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.IfStatement.new(condition.*, body);

        return stmt;
    }

    fn parseNext(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.NextStatement.new();
        return stmt;
    }

    fn parseStop(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.StopStatement.new();
        return stmt;
    }

    fn parseWhile(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        const condition = try self.parseExpression();

        var alteration = try self.new_stmt();
        alteration.* = AstExprs.NoneStatement.new();

        if (self.match(TokenKind.COLON)) {
            self.advance();
            const stmt = try self.parseStatement();
            alteration = stmt;
        }

        if (!self.expect(TokenKind.NEWLINE)) {
            return self.badStmt();
        }

        var body = std.ArrayList(AstExprs.Statement).init(self.allocator);
        while (!self.match(TokenKind.END)) {
            const stmt = try self.parseStatement();
            try body.append(stmt.*);
        }
        self.advance();

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.WhileStatement.new(condition.*, body, alteration);
        return stmt;
    }

    fn parseIdentifier(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        const next = self.currentToken();
        self.recede();

        const stmt = switch (next.kind) {
            TokenKind.LEFT_PAREN => self.parseCallExprStmt(),
            TokenKind.SINGLE_EQUALS => self.parseAssignExpr(),
            else => self.badStmt(),
        };

        return stmt;
    }

    fn parseCallExpr(self: *Parser) ParseError!*AstExprs.Expression {
        const identifier = self.currentToken().lexeme;
        self.advance();

        self.advance(); // (

        var arguments = std.ArrayList(AstExprs.Expression).init(self.allocator);
        while (!self.match(TokenKind.RIGHT_PAREN)) {
            const expr = try self.parseExpression();
            try arguments.append(expr.*);

            if (self.match(TokenKind.RIGHT_PAREN)) {
                break;
            }

            self.advance();
        }

        self.advance(); // )
        const stmt = try self.new_expr();
        stmt.* = AstExprs.FunctionCallExpression.new(identifier, arguments);
        return stmt;
    }

    fn parseCallExprStmt(self: *Parser) ParseError!*AstExprs.Statement {
        const identifier = self.currentToken().lexeme;
        self.advance();

        self.advance(); // (

        var arguments = std.ArrayList(AstExprs.Expression).init(self.allocator);
        while (!self.match(TokenKind.RIGHT_PAREN)) {
            const expr = try self.parseExpression();
            try arguments.append(expr.*);

            if (self.match(TokenKind.RIGHT_PAREN)) {
                break;
            }

            self.advance();
        }

        self.advance(); // )

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.FunctionCallStatement.new(identifier, arguments);
        return stmt;
    }

    fn parseAssignExpr(self: *Parser) ParseError!*AstExprs.Statement {
        var pointerLevel: u8 = 0;
        while (self.match(TokenKind.STAR)) {
            self.advance();
            pointerLevel += 1;
        }

        const identifier = self.currentToken().lexeme;
        self.advance();

        self.advance(); // =

        const expr = try self.parseExpression();

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.AssignmentStatement.new(identifier, pointerLevel, expr.*);
        return stmt;
    }

    fn parseReturn(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        const expr = try self.parseExpression();

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.ReturnStatement.new(expr.*);
        return stmt;
    }

    fn parseEmbed(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        const next = self.currentToken();
        if (next.kind != TokenKind.EMBED_BLOCK) {
            return self.badStmt();
        }

        self.advance();
        if (!self.expect(TokenKind.END)) {
            return self.badStmt();
        }

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.EmbedStatement.new(next.lexeme);
        return stmt;
    }

    fn parseTypeSignature(self: *Parser) ParseError!*AstExprs.Statement {
        var pointerLevel: u8 = 0;
        while (self.match(TokenKind.STAR)) {
            self.advance();
            pointerLevel += 1;
        }

        const typeAnnotationToken = self.currentToken();
        if (!self.match(TokenKind.IDENTIFIER)) {
            return self.badStmt();
        }
        self.advance();

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.TypeSignature.new(typeAnnotationToken.lexeme, pointerLevel);
        return stmt;
    }

    fn parseLet(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        // skip identifier
        self.advance();

        // skip colon and type
        var hasType = false;
        var pointerLevel: u8 = 0;
        if (self.match(TokenKind.COLON)) {
            self.advance();

            while (self.match(TokenKind.STAR)) {
                pointerLevel += 1;
                self.advance();
            }

            self.advance();
            hasType = true;
        }

        self.advance(); // =

        const token = self.currentToken();

        self.current -= 3;
        if (hasType) self.current -= 2;
        for (0..pointerLevel) |_| self.recede();

        if (token.kind == TokenKind.NEW) {
            return self.parseNew();
        } else {
            return self.parseVariable();
        }
    }

    fn parseNew(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        // const isConstant = true;

        const nameToken = self.currentToken();
        if (!self.match(TokenKind.IDENTIFIER)) return self.badStmt();
        self.advance();

        if (!self.expect(TokenKind.COLON)) {
            return self.badStmt();
        }

        const typeSignature = try self.parseTypeSignature();

        if (!self.expect(TokenKind.SINGLE_EQUALS)) {
            return self.badStmt();
        }

        if (!self.expect(TokenKind.NEW)) {
            return self.badStmt();
        }

        if (!self.expect(TokenKind.LEFT_PAREN)) {
            return self.badStmt();
        }

        if (!self.expect(TokenKind.NEWLINE)) {
            return self.badStmt();
        }

        var parameters = std.ArrayList(AstExprs.Statement).init(self.allocator);
        while (!self.match(TokenKind.RIGHT_PAREN)) {
            const identifierToken = self.currentToken();
            self.advance();

            if (!self.expect(TokenKind.COLON)) {
                return self.badStmt();
            }

            const expression = try self.parseExpression();

            if (!self.expect(TokenKind.NEWLINE)) {
                return self.badStmt();
            }

            const stmt = try self.new_stmt();
            stmt.* = AstExprs.StructParameter.new(identifierToken.lexeme, expression);
            try parameters.append(stmt.*);
        }

        if (!self.expect(TokenKind.RIGHT_PAREN)) {
            return self.badStmt();
        }

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.StructAllocation.new(nameToken.lexeme, typeSignature, parameters);
        return stmt;
    }

    fn parseVariable(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        const isConstant = true;

        const nameToken = self.currentToken();
        if (!self.match(TokenKind.IDENTIFIER)) return self.badStmt();
        self.advance();

        if (!self.expect(TokenKind.COLON)) {
            return self.badStmt();
        }

        const typeSignature = try self.parseTypeSignature();

        if (!self.expect(TokenKind.SINGLE_EQUALS)) {
            return self.badStmt();
        }

        const expr = try self.parseExpression();

        if (!self.expect(TokenKind.NEWLINE)) {
            return self.badStmt();
        }

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.VariableDeclaration.new(nameToken.lexeme, expr.*, isConstant, typeSignature);
        return stmt;
    }

    fn parseAttribute(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        return switch (self.currentToken().kind) {
            TokenKind.ENTRY => self.parseEntryAttribute(),
            TokenKind.INLINE => self.parseInlineAttribute(),
            TokenKind.C_TYPE => self.parseCTypeAttribute(),
            TokenKind.INTERFACE => self.parseInterfaceAttribute(),
            TokenKind.SELF => self.parseSelfAttribute(),
            else => self.badStmt(),
        };
    }

    fn parseSelfAttribute(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        if (!self.expect(TokenKind.NEWLINE)) {
            return self.badStmt();
        }

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.Statement{ .attribute = AstExprs.AttributeStatement{ .selfAttribute = {} } };
        return stmt;
    }

    fn parseInterfaceAttribute(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        if (!self.expect(TokenKind.NEWLINE)) {
            return self.badStmt();
        }

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.Statement{ .attribute = AstExprs.AttributeStatement{ .interfaceAttribute = {} } };
        return stmt;
    }

    fn parseCTypeAttribute(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        const cTypeToken = self.currentToken();
        if (!self.match(TokenKind.IDENTIFIER)) {
            return self.badStmt();
        }

        self.advance();

        if (!self.expect(TokenKind.NEWLINE)) {
            return self.badStmt();
        }

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.Statement{ .attribute = AstExprs.AttributeStatement{ .cTypeAttribute = cTypeToken.lexeme } };
        return stmt;
    }

    fn parseInlineAttribute(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        if (!self.expect(TokenKind.NEWLINE)) {
            return self.badStmt();
        }

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.Statement{ .attribute = AstExprs.AttributeStatement{ .inlineAttribute = {} } };
        return stmt;
    }

    fn parseEntryAttribute(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        if (!self.expect(TokenKind.NEWLINE)) {
            return self.badStmt();
        }

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.Statement{ .attribute = AstExprs.AttributeStatement{ .entryAttribute = {} } };
        return stmt;
    }

    fn badExpr(self: *Parser) ParseError!*AstExprs.Expression {
        const expr = try self.new_expr();

        expr.* = AstExprs.Expression{ .unknown = AstExprs.UnknownExpression{} };
        return expr;
    }

    fn badStmt(self: *Parser) ParseError!*AstExprs.Statement {
        const stmt = try self.new_stmt();

        stmt.* = AstExprs.BadStatement.new(self.currentToken().lexeme);
        return stmt;
    }

    fn parsePub(self: *Parser, attributes: std.ArrayList(AstExprs.AttributeStatement)) ParseError!*AstExprs.Statement {
        self.advance();

        if (self.match(TokenKind.FN)) {
            self.recede();
            return self.parseFunction(attributes);
        } else if (self.match(TokenKind.STRUCT)) {
            self.recede();
            return self.parseStruct();
        }

        return self.badStmt();
    }

    fn parseFunction(self: *Parser, attributes: std.ArrayList(AstExprs.AttributeStatement)) ParseError!*AstExprs.Statement {
        var isPub: bool = false;

        if (self.match(TokenKind.PUB)) {
            self.advance();
            isPub = true;
        }

        self.advance();

        const nameToken = self.currentToken();
        self.advance();

        if (!self.expect(TokenKind.LEFT_PAREN)) {
            return self.badStmt();
        }

        var parameters = std.ArrayList(AstExprs.Statement).init(self.allocator);
        if (self.match(TokenKind.IDENTIFIER)) {
            while (!self.match(TokenKind.RIGHT_PAREN)) {
                const identifier = self.currentToken().lexeme;
                self.advance();

                self.advance(); // :

                const typeSignature = try self.parseTypeSignature();

                const param = try self.new_stmt();
                param.* = AstExprs.FunctionParameter.new(identifier, typeSignature);
                try parameters.append(param.*);

                if (self.match(TokenKind.COMMA)) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        if (!self.expect(TokenKind.RIGHT_PAREN)) {
            return self.badStmt();
        }

        if (!self.expect(TokenKind.COLON)) {
            return self.badStmt();
        }

        const returnType = try self.parseTypeSignature();

        if (!self.expect(TokenKind.NEWLINE)) {
            return self.badStmt();
        }

        var body = std.ArrayList(AstExprs.Statement).init(self.allocator);
        while (!self.match(TokenKind.END)) {
            while (self.match(TokenKind.NEWLINE)) {
                self.advance();
            }
            if (self.match(TokenKind.END)) break;

            const stmt = try self.parseStatement();
            try body.append(stmt.*);
        }

        if (!self.expect(TokenKind.END)) {
            return self.badStmt();
        }

        const stmt = try self.new_stmt();

        stmt.* = AstExprs.FunctionDeclaration.new(
            nameToken.lexeme,
            returnType,
            isPub,
            attributes,
            body,
            parameters,
        );
        return stmt;
    }

    fn parseExpression(self: *Parser) ParseError!*AstExprs.Expression {
        return self.parseLogicalOr();
    }

    fn parseLogicalOr(self: *Parser) ParseError!*AstExprs.Expression {
        var left = try self.parseLogicalAnd();

        while (self.match(TokenKind.OR)) {
            const op = self.currentToken().kind;
            self.advance();

            const right = try self.parseLogicalAnd();

            const expr = try self.new_expr();
            expr.* = AstExprs.BinaryExpression.new(left, op, right);

            left = expr;
        }

        return left;
    }

    fn parseLogicalAnd(self: *Parser) ParseError!*AstExprs.Expression {
        var left = try self.parseBitwiseOr();

        while (self.match(TokenKind.AND)) {
            const op = self.currentToken().kind;
            self.advance();
            const right = try self.parseBitwiseOr();
            const expr = try self.new_expr();
            expr.* = AstExprs.BinaryExpression.new(left, op, right);
            left = expr;
        }

        return left;
    }

    fn parseBitwiseOr(self: *Parser) ParseError!*AstExprs.Expression {
        var left = try self.parseBitwiseXor();

        while (self.match(TokenKind.PIPE)) {
            const op = self.currentToken().kind;
            self.advance();
            const right = try self.parseBitwiseXor();
            const expr = try self.new_expr();
            expr.* = AstExprs.BinaryExpression.new(left, op, right);
            left = expr;
        }

        return left;
    }

    fn parseBitwiseXor(self: *Parser) ParseError!*AstExprs.Expression {
        var left = try self.parseBitwiseAnd();

        while (self.match(TokenKind.CARET)) {
            const op = self.currentToken().kind;
            self.advance();
            const right = try self.parseBitwiseAnd();
            const expr = try self.new_expr();
            expr.* = AstExprs.BinaryExpression.new(left, op, right);
            left = expr;
        }

        return left;
    }

    fn parseBitwiseAnd(self: *Parser) ParseError!*AstExprs.Expression {
        var left = try self.parseComparison();

        while (self.match(TokenKind.AMPERSAND)) {
            const op = self.currentToken().kind;
            self.advance();
            const right = try self.parseComparison();
            const expr = try self.new_expr();
            expr.* = AstExprs.BinaryExpression.new(left, op, right);
            left = expr;
        }

        return left;
    }

    fn parseComparison(self: *Parser) ParseError!*AstExprs.Expression {
        var left = try self.parseBitwiseShift();

        while (self.match(TokenKind.GREATER_THAN) or self.match(TokenKind.LESS_THAN) or self.match(TokenKind.GREATER_THAN_EQUALS) or self.match(TokenKind.LESS_THAN_EQUALS) or self.match(TokenKind.DOUBLE_EQUALS) or self.match(TokenKind.NOT_EQUALS)) {
            const op = self.currentToken().kind;
            self.advance();

            const right = try self.parseBitwiseShift();

            const expr = try self.new_expr();
            expr.* = AstExprs.BinaryExpression.new(left, op, right);

            left = expr;
        }

        return left;
    }

    fn parseBitwiseShift(self: *Parser) ParseError!*AstExprs.Expression {
        var left = try self.parseTerm();

        while (self.match(TokenKind.LEFT_SHIFT) or self.match(TokenKind.RIGHT_SHIFT)) {
            const op = self.currentToken().kind;
            self.advance();

            const right = try self.parseTerm();

            const expr = try self.new_expr();
            expr.* = AstExprs.BinaryExpression.new(left, op, right);

            left = expr;
        }

        return left;
    }

    fn parseTerm(self: *Parser) ParseError!*AstExprs.Expression {
        var left = try self.parseFactor();

        while (self.match(TokenKind.PLUS) or self.match(TokenKind.MINUS)) {
            const op = self.currentToken().kind;
            self.advance();

            const right = try self.parseFactor();

            const expr = try self.new_expr();
            expr.* = AstExprs.BinaryExpression.new(left, op, right);

            left = expr;
        }

        return left;
    }

    fn parseFactor(self: *Parser) ParseError!*AstExprs.Expression {
        var left = try self.parseUnary();

        while (self.match(TokenKind.STAR) or self.match(TokenKind.SLASH) or self.match(TokenKind.MODULO)) {
            const op = self.currentToken().kind;
            self.advance();

            const right = try self.parseUnary();

            const expr = try self.new_expr();
            expr.* = AstExprs.BinaryExpression.new(left, op, right);

            left = expr;
        }

        return left;
    }

    fn parseUnary(self: *Parser) ParseError!*AstExprs.Expression {
        while (self.match(TokenKind.NOT) or self.match(TokenKind.TILDE)) {
            const op = self.currentToken();
            self.advance();

            const right = try self.parseExpression();

            const expr = try self.new_expr();
            expr.* = AstExprs.UnaryExpression.new(op.kind, right);
            return expr;
        }

        return try self.parsePointerOperator();
    }

    fn parsePointerOperator(self: *Parser) ParseError!*AstExprs.Expression {
        while (self.match(TokenKind.AMPERSAND) or self.match(TokenKind.STAR)) {
            const op = self.currentToken();
            self.advance();

            const right = try self.parseExpression();

            const expr = try self.new_expr();
            expr.* = AstExprs.UnaryExpression.new(op.kind, right);
            return expr;
        }

        return try self.parsePropertyAccessor();
    }

    fn parsePropertyAccessor(self: *Parser) ParseError!*AstExprs.Expression {
        var expr = try self.parsePrimary();

        while (self.match(TokenKind.DOT)) {
            self.advance();

            const propertyToken = self.currentToken();
            if (!self.match(TokenKind.IDENTIFIER)) {
                return self.badExpr();
            }

            self.advance();

            const newExpr = try self.new_expr();
            newExpr.* = AstExprs.PropertyAccessExpression.new(expr, propertyToken.lexeme);
            expr = newExpr;
        }

        return expr;
    }

    fn parsePrimary(self: *Parser) ParseError!*AstExprs.Expression {
        const token: Token = self.currentToken();
        self.advance();

        if (token.kind == TokenKind.INTEGER) {
            const expr = try self.new_expr();
            const value = std.fmt.parseInt(i64, token.lexeme, 10) catch return self.badExpr();

            expr.* = AstExprs.IntegerExpression.new(value);
            return expr;
        } else if (token.kind == TokenKind.IDENTIFIER) {
            if (self.match(TokenKind.LEFT_PAREN)) {
                self.recede();
                return try self.parseCallExpr();
            }

            const expr = try self.new_expr();
            expr.* = AstExprs.IdentifierExpression.new(token.lexeme);
            return expr;
        } else if (token.kind == TokenKind.TRUE or token.kind == TokenKind.FALSE) {
            const expr = try self.new_expr();
            expr.* = AstExprs.IdentifierExpression.new(token.lexeme);
            return expr;
        } else if (token.kind == TokenKind.STRING) {
            const expr = try self.new_expr();
            expr.* = AstExprs.StringExpression.new(token.lexeme);
            return expr;
        } else if (token.kind == TokenKind.CHAR) {
            const expr = try self.new_expr();
            expr.* = AstExprs.CharExpression.new(token.lexeme);
            return expr;
        }

        const expr = try self.new_expr();
        expr.* = AstExprs.Expression{ .unknown = .{} };
        return expr;
    }

    fn new_stmt(self: *Parser) ParseError!*AstExprs.Statement {
        return try self.allocator.create(AstExprs.Statement);
    }

    fn new_expr(self: *Parser) ParseError!*AstExprs.Expression {
        return try self.allocator.create(AstExprs.Expression);
    }

    fn match(self: *Parser, kind: TokenKind) bool {
        return self.currentToken().kind == kind;
    }

    fn expect(self: *Parser, kind: TokenKind) bool {
        if (self.match(kind)) {
            self.advance();
            return true;
        }

        return false;
    }

    fn currentToken(self: *Parser) Token {
        if (self.isEnd()) {
            return Token.eof();
        }

        return self.tokens.items[self.current];
    }

    fn isEnd(self: *Parser) bool {
        if (self.current >= self.tokens.items.len) {
            return true;
        }

        if (self.tokens.items[self.current].kind == TokenKind.EOF) {
            return true;
        }

        return false;
    }

    fn advance(self: *Parser) void {
        self.current += 1;
    }

    fn recede(self: *Parser) void {
        self.current -= 1;
    }
};
