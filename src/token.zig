const TokenType = enum {};

const Token = struct {
    type: TokenType,
    literal: *const []u8,
    // metadata
    col: u8,
    row: u8,
    filename: *const []u8,
};

const ILLEGAL = "ILLEGAL";
const EOF = "EOF";

// Identifiers + literals
const IDENT = "IDENT"; // add, foobar, x, y, ...
const INT = "INT"; // 123123094

// Operators
const ASSIGN = "=";
const PLUS = "+";

// Delimiters
const COMMA = ",";
const SEMICOLON = ";";

const LPAREN = "(";
const RPAREN = ")";
const LBRACE = "{";
const RBRACE = "}";

// Keywords
const FUNCTION = "FUNCTION";
const LET = "LET";
