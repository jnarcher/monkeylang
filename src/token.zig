pub const TokenType = enum {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,

    pub fn toString(self: TokenType) []const u8 {
        switch (self) {
            TokenType.ILLEGAL => return "ILLEGAL",
            TokenType.EOF => return "EOF",
            TokenType.IDENT => return "IDENT",
            TokenType.INT => return "INT",
            TokenType.ASSIGN => return "=",
            TokenType.PLUS => return "+",
            TokenType.COMMA => return ",",
            TokenType.SEMICOLON => return ";",
            TokenType.LPAREN => return "(",
            TokenType.RPAREN => return ")",
            TokenType.LBRACE => return "{",
            TokenType.RBRACE => return "}",
            TokenType.FUNCTION => return "FUNCTION",
            TokenType.LET => return "LET",
        }
    }
};

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
    col: u8,
    row: u8,
    filename: []const u8,
};
