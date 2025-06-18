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
            while (self.current_char() == ' ' or self.current_char() == '\r') {
                self.advance();
                continue;
            }

            const token = try self.tokenize_char();
            if (token.kind == TokenKind.EMBED) {
                try self.tokens.append(token);

                const embedToken = try self.tokenize_embed();
                try self.tokens.append(embedToken);

                continue;
            }

            try self.tokens.append(token);
        }

        try self.tokens.append(Token.eof());
    }

    fn tokenize_embed(self: *Lexer) !Token {
        self.advance();

        const start: usize = self.current;
        while (true) {
            if (self.isEnd() or self.current_char() == ')') {
                self.advance();

                const maybeEnd = try self.tokenize_identifier();
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

    fn tokenize_char(self: *Lexer) !Token {
        if (std.ascii.isAlphabetic(self.current_char())) {
            return self.tokenize_identifier();
        } else if (std.ascii.isDigit(self.current_char())) {
            return self.tokenize_numeric();
        } else if (self.current_char() == '\n') {
            self.advance();
            return Token.new("\\n", TokenKind.NEWLINE);
        } else if (self.current_char() == '\"') {
            return self.tokenize_string();
        }

        return self.tokenize_symbol();
    }

    fn tokenize_string(self: *Lexer) !Token {
        self.advance();

        const start: usize = self.current;
        while (!self.isEnd() and self.current_char() != '\"') {
            self.advance();
        }

        const lexeme = self.buffer[start..self.current];
        return Token.new(lexeme, TokenKind.STRING);
    }

    fn tokenize_symbol(self: *Lexer) !Token {
        const c = self.current_char();
        self.advance();

        const token = switch (c) {
            ':' => Token.new(":", TokenKind.COLON),
            '(' => Token.new("(", TokenKind.LEFT_PAREN),
            ')' => Token.new(")", TokenKind.RIGHT_PAREN),
            '=' => {
                if (self.current_char() == '=') {
                    self.advance();
                    return Token.new("==", TokenKind.DOUBLE_EQUALS);
                }

                return Token.new("=", TokenKind.SINGLE_EQUALS);
            },
            '!' => {
                if (self.current_char() == '=') {
                    self.advance();
                    return Token.new("!=", TokenKind.NOT_EQUALS);
                }

                return Token.new("=", TokenKind.EXCLAMATION);
            },
            ';' => Token.new(";", TokenKind.SEMICOLON),
            '+' => Token.new("+", TokenKind.PLUS),
            '-' => Token.new("-", TokenKind.MINUS),
            '*' => Token.new("*", TokenKind.STAR),
            '/' => Token.new("/", TokenKind.SLASH),
            '%' => Token.new("%", TokenKind.MODULO),
            '@' => Token.new("@", TokenKind.AT),
            '&' => Token.new("&", TokenKind.AMPERSAND),
            '|' => Token.new("|", TokenKind.PIPE),
            '~' => Token.new("~", TokenKind.TILDE),
            '^' => Token.new("^", TokenKind.CARET),
            ',' => Token.new(",", TokenKind.COMMA),
            '>' => {
                if (self.current_char() == '=') {
                    self.advance();
                    return Token.new(">=", TokenKind.GREATER_THAN_EQUALS);
                }

                if (self.current_char() == '>') {
                    self.advance();
                    return Token.new(">>", TokenKind.RIGHT_SHIFT);
                }

                return Token.new(">", TokenKind.GREATER_THAN);
            },
            '<' => {
                if (self.current_char() == '=') {
                    self.advance();
                    return Token.new("<=", TokenKind.LESS_THAN_EQUALS);
                }

                if (self.current_char() == '<') {
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
        if (match_keyword(identifier, "let")) return TokenKind.LET;
        if (match_keyword(identifier, "pub")) return TokenKind.PUB;
        if (match_keyword(identifier, "fn")) return TokenKind.FN;
        if (match_keyword(identifier, "return")) return TokenKind.RETURN;
        if (match_keyword(identifier, "end")) return TokenKind.END;
        if (match_keyword(identifier, "entry")) return TokenKind.ENTRY;
        if (match_keyword(identifier, "inline")) return TokenKind.INLINE;
        if (match_keyword(identifier, "ctype")) return TokenKind.C_TYPE;
        if (match_keyword(identifier, "embed")) return TokenKind.EMBED;
        if (match_keyword(identifier, "true")) return TokenKind.TRUE;
        if (match_keyword(identifier, "false")) return TokenKind.FALSE;
        if (match_keyword(identifier, "not")) return TokenKind.NOT;
        if (match_keyword(identifier, "and")) return TokenKind.AND;
        if (match_keyword(identifier, "or")) return TokenKind.OR;
        if (match_keyword(identifier, "while")) return TokenKind.WHILE;
        if (match_keyword(identifier, "interface")) return TokenKind.INTERFACE;
        if (match_keyword(identifier, "next")) return TokenKind.NEXT;
        if (match_keyword(identifier, "stop")) return TokenKind.STOP;
        if (match_keyword(identifier, "if")) return TokenKind.IF;
        if (match_keyword(identifier, "then")) return TokenKind.THEN;

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
