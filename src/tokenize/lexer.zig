const std = @import("std");
const Allocator = @import("std").mem.Allocator;

const Token = @import("token.zig").Token;
const TokenKind = @import("token_kind.zig").TokenKind;

pub const Lexer = struct {
    allocator: Allocator,
    current: usize,
    buffer: []const u8,
    tokens: std.ArrayList(Token),
    isEmbedMode: bool = false,

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
            while (self.currentChar() == ' ' or self.currentChar() == '\r') {
                self.advance();
                continue;
            }

            const token = try self.tokenizeThing();
            if (token.kind == TokenKind.EMBED) {
                try self.tokens.append(token);

                const embedToken = try self.tokenizeEmbed();
                try self.tokens.append(embedToken);

                continue;
            }

            try self.tokens.append(token);
        }

        try self.tokens.append(Token.eof());
    }

    fn tokenizeEmbed(self: *Lexer) !Token {
        self.advance();

        const start: usize = self.current;
        while (true) {
            if (self.isEnd() or self.currentChar() == ')') {
                self.advance();

                const maybeEnd = try self.tokenizeIdentifier();
                if (std.mem.eql(u8, maybeEnd.lexeme, "end")) {
                    self.current -= 4; // length of 'end' and ')'
                    break;
                } else {
                    continue;
                }
            }

            self.advance();
        }

        const lexeme = self.buffer[start..self.current];
        self.advance();

        return Token.new(lexeme, TokenKind.EMBED_BLOCK);
    }

    fn tokenizeThing(self: *Lexer) !Token {
        if (std.ascii.isAlphabetic(self.currentChar())) {
            return self.tokenizeIdentifier();
        } else if (std.ascii.isDigit(self.currentChar())) {
            return self.tokenizeNumeric();
        } else if (self.currentChar() == '\n') {
            self.advance();
            return Token.new("\\n", TokenKind.NEWLINE);
        } else if (self.currentChar() == '\"') {
            return self.tokenizeString();
        } else if (self.currentChar() == '\'') {
            return self.tokenizeChar();
        }

        return self.tokenizeSymbol();
    }

    fn tokenizeChar(self: *Lexer) !Token {
        self.advance();

        const start: usize = self.current;

        var isEscape = false;
        if (self.currentChar() == '\\') {
            isEscape = true;
            self.advance();
        }
        self.advance();

        const lexeme = self.buffer[start..self.current];
        self.advance();

        return Token.new(lexeme, TokenKind.CHAR);
    }

    fn tokenizeString(self: *Lexer) !Token {
        self.advance();

        const start: usize = self.current;
        while (!self.isEnd() and self.currentChar() != '\"') {
            self.advance();
        }

        const lexeme = self.buffer[start..self.current];
        self.advance();

        return Token.new(lexeme, TokenKind.STRING);
    }

    fn tokenizeSymbol(self: *Lexer) !Token {
        const c = self.currentChar();
        self.advance();

        const token = switch (c) {
            ':' => Token.new(":", TokenKind.COLON),
            '(' => Token.new("(", TokenKind.LEFT_PAREN),
            ')' => Token.new(")", TokenKind.RIGHT_PAREN),
            '=' => {
                if (self.currentChar() == '=') {
                    self.advance();
                    return Token.new("==", TokenKind.DOUBLE_EQUALS);
                }

                return Token.new("=", TokenKind.SINGLE_EQUALS);
            },
            '!' => {
                if (self.currentChar() == '=') {
                    self.advance();
                    return Token.new("!=", TokenKind.NOT_EQUALS);
                }

                return Token.new("=", TokenKind.EXCLAMATION);
            },
            ';' => Token.new(";", TokenKind.SEMICOLON),
            '+' => Token.new("+", TokenKind.PLUS),
            '-' => {
                if (self.currentChar() == '>') {
                    self.advance();
                    return Token.new("->", TokenKind.DEREF_ARROW);
                }

                return Token.new("-", TokenKind.MINUS);
            },
            '*' => Token.new("*", TokenKind.STAR),
            '/' => Token.new("/", TokenKind.SLASH),
            '%' => Token.new("%", TokenKind.MODULO),
            '@' => Token.new("@", TokenKind.AT),
            '&' => Token.new("&", TokenKind.AMPERSAND),
            '|' => Token.new("|", TokenKind.PIPE),
            '~' => Token.new("~", TokenKind.TILDE),
            '^' => Token.new("^", TokenKind.CARET),
            ',' => Token.new(",", TokenKind.COMMA),
            '.' => Token.new(".", TokenKind.DOT),
            '>' => {
                if (self.currentChar() == '=') {
                    self.advance();
                    return Token.new(">=", TokenKind.GREATER_THAN_EQUALS);
                }

                if (self.currentChar() == '>') {
                    self.advance();
                    return Token.new(">>", TokenKind.RIGHT_SHIFT);
                }

                return Token.new(">", TokenKind.GREATER_THAN);
            },
            '<' => {
                if (self.currentChar() == '=') {
                    self.advance();
                    return Token.new("<=", TokenKind.LESS_THAN_EQUALS);
                }

                if (self.currentChar() == '<') {
                    self.advance();
                    return Token.new("<<", TokenKind.LEFT_SHIFT);
                }

                return Token.new("<", TokenKind.LESS_THAN);
            },
            else => Token.new("BAD", TokenKind.BAD),
        };

        return token;
    }

    fn peek(self: *Lexer) u8 {
        const next = self.current + 1;
        if (next >= self.buffer.len) return @as(u8, 0);
        return self.buffer[next];
    }

    fn tokenizeWhitespace(self: *Lexer) !Token {
        const start: usize = self.current;
        while (!self.isEnd() and self.currentChar() == ' ') {
            self.advance();
        }

        const lexeme = self.buffer[start..self.current];
        return Token.new(lexeme, TokenKind.WHITE_SPACE);
    }

    fn tokenizeNumeric(self: *Lexer) !Token {
        const start: usize = self.current;
        while (!self.isEnd() and std.ascii.isDigit(self.currentChar())) {
            self.advance();
        }

        const lexeme = self.buffer[start..self.current];
        return Token.new(lexeme, TokenKind.INTEGER);
    }

    fn tokenizeIdentifier(self: *Lexer) !Token {
        const start: usize = self.current;
        while (!self.isEnd() and std.ascii.isAlphanumeric(self.currentChar())) {
            self.advance();
        }

        const lexeme = self.buffer[start..self.current];
        const kind = getIdentifierKind(lexeme);

        return Token.new(lexeme, kind);
    }

    fn matchKeyword(identifier: []const u8, keyword: []const u8) bool {
        return std.mem.eql(u8, identifier, keyword);
    }

    fn getIdentifierKind(identifier: []const u8) TokenKind {
        if (matchKeyword(identifier, "let")) return TokenKind.LET;
        if (matchKeyword(identifier, "pub")) return TokenKind.PUB;
        if (matchKeyword(identifier, "fn")) return TokenKind.FN;
        if (matchKeyword(identifier, "return")) return TokenKind.RETURN;
        if (matchKeyword(identifier, "end")) return TokenKind.END;
        if (matchKeyword(identifier, "entry")) return TokenKind.ENTRY;
        if (matchKeyword(identifier, "inline")) return TokenKind.INLINE;
        if (matchKeyword(identifier, "ctype")) return TokenKind.C_TYPE;
        if (matchKeyword(identifier, "embed")) return TokenKind.EMBED;
        if (matchKeyword(identifier, "true")) return TokenKind.TRUE;
        if (matchKeyword(identifier, "false")) return TokenKind.FALSE;
        if (matchKeyword(identifier, "not")) return TokenKind.NOT;
        if (matchKeyword(identifier, "and")) return TokenKind.AND;
        if (matchKeyword(identifier, "or")) return TokenKind.OR;
        if (matchKeyword(identifier, "while")) return TokenKind.WHILE;
        if (matchKeyword(identifier, "interface")) return TokenKind.INTERFACE;
        if (matchKeyword(identifier, "next")) return TokenKind.NEXT;
        if (matchKeyword(identifier, "stop")) return TokenKind.STOP;
        if (matchKeyword(identifier, "if")) return TokenKind.IF;
        if (matchKeyword(identifier, "then")) return TokenKind.THEN;
        if (matchKeyword(identifier, "do")) return TokenKind.DO;
        if (matchKeyword(identifier, "self")) return TokenKind.SELF;
        if (matchKeyword(identifier, "struct")) return TokenKind.STRUCT;
        if (matchKeyword(identifier, "new")) return TokenKind.NEW;

        return TokenKind.IDENTIFIER;
    }

    fn currentChar(self: *Lexer) u8 {
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
