pub const TokenKind = enum {
    PLUS,
    MINUS,
    STAR,
    SLASH,
    MODULO,

    LEFT_PAREN,
    RIGHT_PAREN,
    SINGLE_EQUALS,
    SEMICOLON,
    COLON,

    LET,
    PUB,
    FN,
    RETURN,

    IDENTIFIER,
    INTEGER,

    NEWLINE,
    CARRIAGE_RETURN,
    WHITE_SPACE,
    BAD,
    EOF,

    pub fn binOpToStr(op: TokenKind) []const u8 {
        return switch (op) {
            TokenKind.PLUS => "+",
            TokenKind.MINUS => "-",
            TokenKind.STAR => "*",
            TokenKind.SLASH => "/",
            TokenKind.MODULO => "%",
            else => "",
        };
    }
};
