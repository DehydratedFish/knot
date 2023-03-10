#include "parser.h"

#include "platform.h"


enum {
	CHAR_CHARACTER,
	CHAR_DIGIT,
	CHAR_WHITESPACE,
	CHAR_CONTROL,
	CHAR_UNUSED,
};

#define A CHAR_CHARACTER
#define B CHAR_DIGIT
#define C CHAR_WHITESPACE
#define D CHAR_CONTROL
#define E CHAR_UNUSED

INTERNAL u8 Lookup[] = {
	E, E, E, E, E, E, E, E, E, C, C, C, E, C, E, E,
	E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E,
	C, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D,
	B, B, B, B, B, B, B, B, B, B, D, D, D, D, D, D,
	D, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A,
	A, A, A, A, A, A, A, A, A, A, A, D, D, D, D, D,
	D, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A,
	A, A, A, A, A, A, A, A, A, A, A, D, D, D, D, E,

	// TODO: unicode characters and forbidden values
};

#undef A
#undef B
#undef C
#undef D
#undef E


INTERNAL void advance_source(Parser *parser, s32 amount) {
	assert(amount <= parser->source_code.size);

	parser->source_code.data += amount;
	parser->source_code.size -= amount;

	parser->loc.column += 1;
}

INTERNAL bool peek_char(Parser *parser, u8 *c) {
	if (parser->source_code.size == 0) return false;

	*c = parser->source_code.data[0];

	return true;
}

INTERNAL bool get_char(Parser *parser, u8 *c) {
	if (parser->source_code.size == 0) return false;

	*c = parser->source_code.data[0];
	advance_source(parser, 1);

	return true;
}

INTERNAL void report_error(Parser *parser, SourceLocation location, String message) {
	if (parser->error_mode) return;

	SyntaxError error = {message, location};

	append(&parser->errors, error);
	parser->error_mode = true;
}

INTERNAL Token parse_identifier(Parser *parser) {
	u8 *mark = parser->source_code.data;
	s32 size = 0;

	SourceLocation location = parser->loc;

	u8 c;
	while (peek_char(parser, &c)) {
		s32 type = Lookup[c];
		if (type == CHAR_CHARACTER || type == CHAR_DIGIT || type == '_') {
			size += 1;
			advance_source(parser, 1);
		} else {
			break;
		}
	}

	Token token = {
		TOKEN_IDENTIFIER,
		location,
		{mark, size}
	};

	if (equal(token.content, "struct")) {
		token.kind = TOKEN_KEYWORD_STRUCT;
	} else if (equal(token.content, "return")) {
		token.kind = TOKEN_KEYWORD_RETURN;
	}

	return token;
}

INTERNAL Token parse_number(Parser *parser) {
	u8 *mark = parser->source_code.data;
	s32 size = 0;
	u32 kind = TOKEN_INTEGER;

	SourceLocation location = parser->loc;

	u8 c;
	while (peek_char(parser, &c)) {
		s32 type = Lookup[c];
		if (type == CHAR_DIGIT || c == '.') {
			if (c == '.') {
				if (kind == TOKEN_INTEGER) {
					kind = TOKEN_FLOAT;
				} else {
					report_error(parser, location, "Multiple . in number literal.");
				}
			}

			size += 1;
			advance_source(parser, 1);
		} else {
			break;
		}
	}

	Token token = {
		kind,
		location,
		{mark, size}
	};

	return token;
}

INTERNAL Token parse_control(Parser *parser) {
	Token token = {};
	token.loc = parser->loc;

	u8 c;
	get_char(parser, &c);

	switch (c) {
	case '.': { token.kind = TOKEN_DOT; } break;
	case ',': { token.kind = TOKEN_COMMA; } break;
	case '=': { token.kind = TOKEN_EQUAL; } break;
	case ':': { token.kind = TOKEN_COLON; } break;
	case ';': { token.kind = TOKEN_SEMICOLON; } break;

	case '(': { token.kind = TOKEN_LEFT_PARENTHESIS; } break;
	case ')': { token.kind = TOKEN_RIGHT_PARENTHESIS; } break;
	case '{': { token.kind = TOKEN_LEFT_BRACE; } break;
	case '}': { token.kind = TOKEN_RIGHT_BRACE; } break;
	case '[': { token.kind = TOKEN_LEFT_BRACKET; } break;
	case ']': { token.kind = TOKEN_RIGHT_BRACKET; } break;

	default:
		SourceLocation location = parser->loc;
		location.column -= 1;
		report_error(parser, location, "Unsupported character [Placeholder for format string].");
	}

	return token;
}

INTERNAL void skip_whitespaces(Parser *parser) {
	s32 const multi_new_line = '\n' + '\r';

	u8 c;
	while (peek_char(parser, &c) && Lookup[c] == CHAR_WHITESPACE) {
		if (c == '\n' || c == '\r') {
			advance_source(parser, 1);

			u8 c2;
			if (peek_char(parser, &c2) && (c + c2) == multi_new_line) {
				advance_source(parser, 1);
			}

			parser->loc.line += 1;
			parser->loc.column = 1;

			continue;
		}

		advance_source(parser, 1);
	}
}


INTERNAL Token next_token(Parser *parser) {
	if (parser->error_mode) {
		parser->error_mode = false;
	}

	while (parser->source_code.size) {
		switch (Lookup[parser->source_code.data[0]]) {
		case CHAR_CHARACTER: {
			return parse_identifier(parser);
		} break;

		case CHAR_DIGIT: {
			return parse_number(parser);
		} break;

		case CHAR_WHITESPACE: {
			skip_whitespaces(parser);
			continue;
		} break;

		case CHAR_CONTROL: {
			return parse_control(parser);
		} break;

		case CHAR_UNUSED: {
			Token token;
			token.kind = TOKEN_UNKNOWN;
			token.content = String(parser->source_code.data - 1, 1);

			return token;
		} break;
		}
	}

	die("In function next_token(): unreachable code path.");
	return {};
}

INTERNAL void advance_token(Parser *parser) {
	parser->previous_token = parser->current_token;
	parser->current_token  = next_token(parser);
}

Parser init_parser(String source) {
	Parser parser = {0};

	parser.source_code = source;
	assert(parser.source_code.alloc == 0);

	parser.loc.line = 1;
	parser.loc.column = 1;

	advance_token(&parser);

	return parser;
}

INTERNAL bool current_token_is(Parser *parser, u32 kind) {
	return parser->current_token.kind == kind;
}

INTERNAL bool consume(Parser *parser, u32 kind, String error_msg) {
	if (current_token_is(parser, kind)) {
		advance_token(parser);
		return true;
	} else {
		report_error(parser, parser->current_token.loc, error_msg);
	}

	return false;
}

INTERNAL bool match(Parser *parser, u32 kind) {
	if (current_token_is(parser, kind)) {
		advance_token(parser);
		return true;
	}
	return false;
}

INTERNAL TypeSpecifier parse_type_specifier

INTERNAL AstNode *parse_struct_declaration(Parser *parser, Token name) {
	AstStructDeclaration *decl = ALLOCATE(AstStructDeclaration);
	decl->name = name.content;
	consume(parser, TOKEN_KEYWORD_STRUCT, "Missing keyword struct.");
	consume(parser, TOKEN_LEFT_BRACE, "Expected { in struct declaration.");

	do {
		if (current_token_is(parser, TOKEN_RIGHT_BRACE)) break;

		AstStructMember member = {0};

		consume(parser, TOKEN_IDENTIFIER, "Missing member name in declaration of struct [name placeholder].");
		member.name = parser->previous_token.content;

		consume(parser, TOKEN_COLON, "Missing : in member declaration");
		consume(parser, TOKEN_IDENTIFIER, "Missing type specifier in declaration of struct [name placeholder].");
		member.declared_type = parser->previous_token.content;

		append(&decl->members, member);
	} while (consume(parser, TOKEN_SEMICOLON, "Missing ; after struct member declaration."));

	consume(parser, TOKEN_RIGHT_BRACE, "Missing } to end declaration of struct [name placeholder]");

	return decl;
}

INTERNAL AstExpression parse_expression() {
	return {};
}

INTERNAL AstNode *parse_variable_declaration(Parser *parser, Token name) {
	AstVariableDeclaration *decl = ALLOCATE(AstVariableDeclaration);
	decl->name = name.content;

	if (match(parser, TOKEN_IDENTIFIER)) {
		decl->declared_type = parser->previous_token.content;
	}

	consume(parser, TOKEN_EQUAL, "Missing = in declaration.");

	decl->expr = parse_expression();

	return decl;
}

INTERNAL AstNode *parse_declaration(Parser *parser) {
	consume(parser, TOKEN_IDENTIFIER, "Expected identifier for declaration.");
	Token identifier = parser->previous_token;

	if (match(parser, TOKEN_COLON)) {
		switch (parser->current_token.kind) {
		case TOKEN_KEYWORD_STRUCT: {
			return parse_struct_declaration(parser, identifier);
		} break;

		case TOKEN_IDENTIFIER:
		case TOKEN_EQUAL: {
			return parse_variable_declaration(parser, identifier);
		} break;
		}
		report_error(parser, parser->current_token.loc, "Missing type specifier.");
	}

	return 0;
}

INTERNAL AstNode *parse_statement(Parser *parser) {
	if (current_token_is(parser, TOKEN_IDENTIFIER)) {
		return parse_declaration(parser);
	}

	report_error(parser, parser->current_token.loc, "Expected statement.");

	return 0;
}

INTERNAL void synchronize(Parser *parser) {
	while (!current_token_is(parser, TOKEN_END_OF_INPUT)) {
		switch (parser->current_token.kind) {
		case TOKEN_RIGHT_BRACE: 
		case TOKEN_SEMICOLON:
			advance_token(parser);
			goto END;
		break;
		}

		advance_token(parser);
	}

END:
	parser->error_mode = false;
}

ParseResult parse_as_knot_code(Parser *parser) {
	ParseResult result;

	while (!current_token_is(parser, TOKEN_END_OF_INPUT)) {
		if (parser->error_mode) {
			synchronize(parser);
		}
		append(&result.nodes, parse_statement(parser));
	}

	return result;
}

