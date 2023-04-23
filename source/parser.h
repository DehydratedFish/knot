#ifndef INCLUDE_GUARD_KNOT_PARSER_H
#define INCLUDE_GUARD_KNOT_PARSER_H

#include "string2.h"
#include "array.h"

#include "knot.h"
#include "ast.h"


enum {
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
	TOKEN_DOUBLE_COLON,
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

	TOKEN_KEYWORD_TYPE,
	TOKEN_KEYWORD_RETURN,

	TOKEN_UNKNOWN,

	TOKEN_COUNT
};
struct Token {
	u32 kind;
	SourceLocation loc;

	String content;
};

struct Parser {
	String source_code;
	String filename;

	SourceLocation loc;

	Token previous_token;
	Token current_token;

	bool has_peek;
	Token peek_token;

	bool error_mode;
};

Parser init_parser(String filename, String source);


AbstractSyntaxTree parse_as_knot_code(Parser *parser);

void print_ast(AbstractSyntaxTree *result);

#endif // INCLUDE_GUARD_KNOT_PARSER_H

