#pragma once

#include "string2.h"

#include "knot.h"
#include "ast.h"


enum TokenKind {
    TOKEN_END_OF_INPUT,
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_INTEGER,
    TOKEN_FLOAT,

    TOKEN_DOT,
    TOKEN_DOUBLE_DOT,
    TOKEN_COMMA,
    TOKEN_EQUAL,
    TOKEN_COLON,
    TOKEN_SEMICOLON,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_ASTERISK,
    TOKEN_AMPERSAND,
    TOKEN_SLASH,

    TOKEN_RIGHT_ARROW,

    TOKEN_LEFT_PARENTHESIS,
    TOKEN_RIGHT_PARENTHESIS,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_LEFT_BRACKET,
    TOKEN_RIGHT_BRACKET,

    TOKEN_KEYWORD_STRUCT,
    TOKEN_KEYWORD_RETURN,

    TOKEN_UNKNOWN,

    TOKEN_COUNT
};
struct Token {
    TokenKind kind;
    SourceLocation loc;

    String content;
};

s32 const PARSER_MAX_PEEK = 16;
struct Parser {
    String source_code;
    String filename;

    SourceLocation loc;

    Environment *env;

    Token previous_token;
    Token current_token;

    Token peek[PARSER_MAX_PEEK]; // NOTE: This is ment as a stack.
    s32   peek_count;

    s32 brace_count;

    bool error_mode;
};

Parser init_parser(String filename, String source);


bool parse_as_knot_code(Parser *parser, Environment *env);

