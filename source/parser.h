#ifndef INCLUDE_GUARD_KNOT_PARSER_H
#define INCLUDE_GUARD_KNOT_PARSER_H

#include "string2.h"
#include "array.h"

#include "ast.h"


struct SourceLocation {
	s32 line;
	s32 column;
};

enum {
	TOKEN_END_OF_INPUT,
	TOKEN_IDENTIFIER,
	TOKEN_STRING,
	TOKEN_INTEGER,
	TOKEN_FLOAT,

	TOKEN_DOT,
	TOKEN_COMMA,
	TOKEN_EQUAL,
	TOKEN_COLON,
	TOKEN_SEMICOLON,
	TOKEN_PLUS,
	TOKEN_MINUS,
	TOKEN_ASTERISK,
	TOKEN_AMPERSAND,

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
	u32 kind;
	SourceLocation loc;

	String content;
};

struct SyntaxError {
	String file;
	String message;
	SourceLocation location;
	u8 *position;
};

struct Parser {
	String source_code;
	String filename;

	SourceLocation loc;

	Token previous_token;
	Token current_token;

	bool error_mode;
	Array<SyntaxError> errors;
};

Parser init_parser(String filename, String source);


struct ParseResult {
	Array<AstNode*> nodes;
};

ParseResult parse_as_knot_code(Parser *parser);

void print_ast(ParseResult *result);

#endif // INCLUDE_GUARD_KNOT_PARSER_H

