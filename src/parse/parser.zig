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
                    }
                }

                for (func.body.items) |stmt| {
                    self.printStmt(stmt, indent + 2);
                }
            },
            .badStmt => |_| {},
            .attribute => |_| {},
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
            // .unknown => |_| {
            //     std.debug.print("Unknown\n", .{});
            // },
            // else => {
            //     std.debug.print("UNKNOWN", .{});
            // },
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

        const stmt = switch (self.current_token().kind) {
            TokenKind.FN => try self.parseFunction(attributes),
            TokenKind.PUB => try self.parsePub(attributes),
            TokenKind.LET => try self.parseVariable(),
            TokenKind.EMBED => try self.parseEmbed(),
            TokenKind.RETURN => try self.parseReturn(),
            TokenKind.IDENTIFIER => try self.parseIdentifierStatement(),
            else => try self.badStmt(),
        };

        return stmt;
    }

    fn parseIdentifierStatement(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        const next = self.current_token();
        self.recede();

        const stmt = switch (next.kind) {
            TokenKind.LEFT_PAREN => self.parseCallExprStmt(),
            else => self.badStmt(),
            TokenKind.SINGLE_EQUALS => self.parseAssignExpr(),
        };

        return stmt;
    }

    fn parseCallExprStmt(self: *Parser) ParseError!*AstExprs.Statement {
        const identifier = self.current_token().lexeme;
        self.advance();

        self.advance(); // (
        self.advance(); // )

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.FunctionCallStatement.new(identifier);
        return stmt;
    }

    fn parseAssignExpr(self: *Parser) ParseError!*AstExprs.Statement {
        const identifier = self.current_token().lexeme;
        self.advance();

        self.advance(); // =

        const expr = try self.parseExpression();

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.AssignmentStatement.new(identifier, expr.*);
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

        const next = self.current_token();
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

    fn parseVariable(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        const isConstant = true;

        const nameToken = self.current_token();
        if (!self.match(TokenKind.IDENTIFIER)) return self.badStmt();
        self.advance();

        var typeAnnotation: []const u8 = "";
        var pointerLevel: u8 = 0;

        if (self.match(TokenKind.COLON)) {
            self.advance();

            while (self.match(TokenKind.STAR)) {
                self.advance();
                pointerLevel += 1;
            }

            const typeAnnotationToken = self.current_token();
            if (!self.match(TokenKind.IDENTIFIER)) {
                return self.badStmt();
            }
            self.advance();

            typeAnnotation = typeAnnotationToken.lexeme;
        }

        if (!self.expect(TokenKind.SINGLE_EQUALS)) {
            return self.badStmt();
        }

        const expr = try self.parseExpression();

        if (!self.expect(TokenKind.NEWLINE)) {
            return self.badStmt();
        }

        const stmt = try self.new_stmt();
        stmt.* = AstExprs.VariableDeclaration.new(nameToken.lexeme, typeAnnotation, expr.*, isConstant, pointerLevel);
        return stmt;
    }

    fn parseAttribute(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        if (self.match(TokenKind.ENTRY)) {
            return self.parseEntryAttribute();
        } else if (self.match(TokenKind.INLINE)) {
            return self.parseInlineAttribute();
        } else if (self.match(TokenKind.C_TYPE)) {
            return self.parseCTypeAttribute();
        } else {
            return self.badStmt();
        }
    }

    fn parseCTypeAttribute(self: *Parser) ParseError!*AstExprs.Statement {
        self.advance();

        const cTypeToken = self.current_token();
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

        stmt.* = AstExprs.BadStatement.new();
        return stmt;
    }

    fn parsePub(self: *Parser, attributes: std.ArrayList(AstExprs.AttributeStatement)) ParseError!*AstExprs.Statement {
        self.advance();

        if (self.match(TokenKind.FN)) {
            self.recede();
            return self.parseFunction(attributes);
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

        const nameToken = self.current_token();
        self.advance();

        if (!self.expect(TokenKind.LEFT_PAREN)) {
            return self.badStmt();
        }

        if (!self.expect(TokenKind.RIGHT_PAREN)) {
            return self.badStmt();
        }

        if (!self.expect(TokenKind.COLON)) {
            return self.badStmt();
        }

        const returnTypeToken = self.current_token();
        self.advance();

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

        stmt.* = AstExprs.FunctionDeclaration.new(nameToken.lexeme, returnTypeToken.lexeme, isPub, attributes, body);
        return stmt;
    }

    fn parseExpression(self: *Parser) ParseError!*AstExprs.Expression {
        return self.parseLogicalOr();
    }

    fn parseLogicalOr(self: *Parser) ParseError!*AstExprs.Expression {
        var left = try self.parseLogicalAnd();

        while (self.match(TokenKind.OR)) {
            const op = self.current_token().kind;
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
            const op = self.current_token().kind;
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
            const op = self.current_token().kind;
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
            const op = self.current_token().kind;
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
            const op = self.current_token().kind;
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
            const op = self.current_token().kind;
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
            const op = self.current_token().kind;
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
            const op = self.current_token().kind;
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
            const op = self.current_token().kind;
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
            const op = self.current_token();
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
            const op = self.current_token();
            self.advance();

            const right = try self.parseExpression();

            const expr = try self.new_expr();
            expr.* = AstExprs.UnaryExpression.new(op.kind, right);
            return expr;
        }

        return try self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ParseError!*AstExprs.Expression {
        const token: Token = self.current_token();
        self.advance();

        if (token.kind == TokenKind.INTEGER) {
            const expr = try self.new_expr();
            const value = std.fmt.parseInt(i64, token.lexeme, 10) catch return self.badExpr();

            expr.* = AstExprs.IntegerExpression.new(value);
            return expr;
        } else if (token.kind == TokenKind.IDENTIFIER) {
            if (self.match(TokenKind.LEFT_PAREN)) {
                self.advance();

                self.advance(); // )

                const expr = try self.new_expr();
                expr.* = AstExprs.FunctionCallExpression.new(token.lexeme);
                return expr;
            }

            const expr = try self.new_expr();
            expr.* = AstExprs.IdentifierExpression.new(token.lexeme);
            return expr;
        } else if (token.kind == TokenKind.TRUE or token.kind == TokenKind.FALSE) {
            const expr = try self.new_expr();
            expr.* = AstExprs.IdentifierExpression.new(token.lexeme);
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
        return self.current_token().kind == kind;
    }

    fn expect(self: *Parser, kind: TokenKind) bool {
        if (self.match(kind)) {
            self.advance();
            return true;
        }

        return false;
    }

    fn current_token(self: *Parser) Token {
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
