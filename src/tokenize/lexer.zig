const std = @import("std");
const Allocator = @import("std").mem.Allocator;

const Token = @import("token.zig").Token;
const TokenKind = @import("token_kind.zig").TokenKind;

pub const Lexer = struct {
    allocator: Allocator,
    current: usize,
    buffer: []const u8,
    tokens: std.ArrayList(Token),

    pub fn new(buffer: []const u8, allocator: Allocator) Lexer {
        return Lexer{
            .current = 0,
            .buffer = buffer,
            .allocator = allocator,
            .tokens = std.ArrayList(Token).init(allocator),
        };
    }

    pub fn print(self: *Lexer) void {
        for (self.tokens.items) |token| {
            std.debug.print("'{s}': {s}\n", .{ token.lexeme, @tagName(token.kind) });
        }
    }

    pub fn tokenize(self: *Lexer) !void {
        while (!self.isEnd()) {
            while (self.current_char() == ' ') {
                self.advance();
                continue;
            }

            const token = try self.tokenize_char();
            try self.tokens.append(token);
        }

        try self.tokens.append(Token.eof());
    }

    fn tokenize_char(self: *Lexer) !Token {
        if (std.ascii.isAlphabetic(self.current_char())) {
            return self.tokenize_identifier();
        } else if (std.ascii.isDigit(self.current_char())) {
            return self.tokenize_numeric();
        } else if (self.current_char() == '\n') {
            self.advance();
            return Token.new("\\n", TokenKind.NEWLINE);
        } else if (self.current_char() == '\r') {
            self.advance();
            return Token.new("\\r", TokenKind.CARRIAGE_RETURN);
        }

        return self.tokenize_symbol();
    }

    fn tokenize_symbol(self: *Lexer) !Token {
        const token = switch (self.current_char()) {
            ':' => Token.new(":", TokenKind.COLON),
            '(' => Token.new("(", TokenKind.LEFT_PAREN),
            ')' => Token.new(")", TokenKind.RIGHT_PAREN),
            '=' => Token.new("=", TokenKind.SINGLE_EQUALS),
            ';' => Token.new(";", TokenKind.SEMICOLON),
            '+' => Token.new("+", TokenKind.PLUS),
            '-' => Token.new("-", TokenKind.MINUS),
            '*' => Token.new("*", TokenKind.STAR),
            '/' => Token.new("/", TokenKind.SLASH),
            '%' => Token.new("%", TokenKind.MODULO),
            else => Token.new("BAD", TokenKind.BAD),
        };

        self.advance();
        return token;
    }

    fn tokenize_whitespace(self: *Lexer) !Token {
        const start: usize = self.current;
        while (!self.isEnd() and self.current_char() == ' ') {
            self.advance();
        }

        const lexeme = self.buffer[start..self.current];
        return Token.new(lexeme, TokenKind.WHITE_SPACE);
    }

    fn tokenize_numeric(self: *Lexer) !Token {
        const start: usize = self.current;
        while (!self.isEnd() and std.ascii.isDigit(self.current_char())) {
            self.advance();
        }

        const lexeme = self.buffer[start..self.current];
        return Token.new(lexeme, TokenKind.INTEGER);
    }

    fn tokenize_identifier(self: *Lexer) !Token {
        const start: usize = self.current;
        while (!self.isEnd() and std.ascii.isAlphanumeric(self.current_char())) {
            self.advance();
        }

        const lexeme = self.buffer[start..self.current];
        const kind = get_identifier_kind(lexeme);

        return Token.new(lexeme, kind);
    }

    fn match_keyword(identifier: []const u8, keyword: []const u8) bool {
        return std.mem.eql(u8, identifier, keyword);
    }

    fn get_identifier_kind(identifier: []const u8) TokenKind {
        if (match_keyword(identifier, "let")) {
            return TokenKind.LET;
        } else if (match_keyword(identifier, "pub")) {
            return TokenKind.PUB;
        } else if (match_keyword(identifier, "fn")) {
            return TokenKind.FN;
        } else if (match_keyword(identifier, "return")) {
            return TokenKind.RETURN;
        }

        return TokenKind.IDENTIFIER;
    }

    fn current_char(self: *Lexer) u8 {
        return self.buffer[self.current];
    }

    fn isEnd(self: *Lexer) bool {
        return self.current >= self.buffer.len;
    }

    fn advance(self: *Lexer) void {
        self.current += 1;
    }

    fn recede(self: *Lexer) void {
        self.current -= 1;
    }
};
