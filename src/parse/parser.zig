const AstExprs = @import("ast.zig");

const Token = @import("../tokenize/token.zig").Token;
const TokenKind = @import("../tokenize/token_kind.zig").TokenKind;

const std = @import("std");

pub const Parser = struct {
    current: usize,
    tokens: std.ArrayList(Token),
    exprs: std.ArrayList(AstExprs.Expression),
    allocator: std.mem.Allocator,

    pub fn new(tokens: std.ArrayList(Token), allocator: std.mem.Allocator) !Parser {
        return Parser{
            .current = 0,
            .tokens = tokens,
            .exprs = std.ArrayList(AstExprs.Expression).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn print(self: *Parser) void {
        for (self.exprs.items) |expr| {
            self.printExpr(expr, 0);
            std.debug.print("\n", .{});
        }
    }

    fn printIndent(indent: usize) void {
        for (0..indent) |_| std.debug.print(" ", .{});
    }

    fn printExpr(self: *Parser, expr: AstExprs.Expression, indent: usize) void {
        printIndent(indent);

        switch (expr) {
            .intExpr => |intExpr| {
                std.debug.print("{}\n", .{intExpr.value});
            },
            .idExpr => |idExpr| {
                std.debug.print("{s}\n", .{idExpr.name});
            },
            .binExpr => |binExpr| {
                std.debug.print("Binary Expression:\n", .{});

                printIndent(indent);
                std.debug.print("Operator: {s}\n", .{@tagName(binExpr.op)});

                printIndent(indent);
                std.debug.print("Left:\n", .{});
                self.printExpr(binExpr.left.*, indent + 2);

                printIndent(indent);
                std.debug.print("Right:\n", .{});
                self.printExpr(binExpr.right.*, indent + 2);
            },
            .unknown => |_| {
                std.debug.print("Unknown\n", .{});
            },
        }
    }

    pub fn constructAst(self: *Parser) !std.ArrayList(AstExprs.Expression) {
        while (!self.isEnd()) {
            const expr = try self.parseTerm();
            try self.exprs.append(expr.*);
        }

        return self.exprs;
    }

    fn parseStatement(self: *Parser) !*AstExprs.Expression {
        return switch (self.current_token().kind) {
            TokenKind.FN => parseFunction(),
            else => self.parseExpression(),
        };
    }

    fn parseFunction(self: *Parser) !*AstExprs.Expression {
        _ = self;
    }

    fn parseExpression(self: *Parser) !*AstExprs.Expression {
        _ = self;
    }

    fn parseTerm(self: *Parser) !*AstExprs.Expression {
        var left = try self.parsePrimary();

        while (self.match(TokenKind.PLUS) or self.match(TokenKind.MINUS)) {
            const op = self.current_token().kind;
            self.advance();

            const right = try self.parsePrimary();

            const expr = try self.new_expr();
            expr.* = AstExprs.BinaryExpression.new(left, op, right);

            left = expr;
        }

        return left;
    }

    fn parsePrimary(self: *Parser) !*AstExprs.Expression {
        const token: Token = self.current_token();
        self.advance();

        if (token.kind == TokenKind.INTEGER) {
            const expr = try self.new_expr();

            expr.* = AstExprs.IntegerExpression.new(
                try std.fmt.parseInt(i64, token.lexeme, 10),
            );
            return expr;
        } else if (token.kind == TokenKind.IDENTIFIER) {
            const expr = try self.new_expr();
            expr.* = AstExprs.IdentifierExpression.new(token.lexeme);
            return expr;
        }

        const expr = try self.new_expr();
        expr.* = AstExprs.Expression{ .unknown = .{} };
        return expr;
    }

    fn new_expr(self: *Parser) !*AstExprs.Expression {
        return try self.allocator.create(AstExprs.Expression);
    }

    fn match(self: *Parser, kind: TokenKind) bool {
        return self.current_token().kind == kind;
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
};
