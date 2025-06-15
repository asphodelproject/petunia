const TokenKind = @import("token_kind.zig").TokenKind;

pub const Token = struct {
    lexeme: []const u8,
    kind: TokenKind,

    pub fn new(lexeme: []const u8, kind: TokenKind) Token {
        return Token{
            .lexeme = lexeme,
            .kind = kind,
        };
    }

    pub fn eof() Token {
        return Token.new("EOF", TokenKind.EOF);
    }
};
