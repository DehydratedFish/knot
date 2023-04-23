#include "parser.h"

#include "platform.h"
#include "io.h"


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



INTERNAL void advance_source(Parser *parser) {
	assert(parser->source_code.size);

	parser->source_code.data += 1;
	parser->source_code.size -= 1;

	parser->loc.ptr = parser->source_code.data;
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
	advance_source(parser);

	return true;
}

INTERNAL bool match_char(Parser *parser, u8 c) {
	if (parser->source_code.size && parser->source_code[0] == c) {
		advance_source(parser);
		return true;
	}

	return false;
}

INTERNAL bool match_char_type(Parser *parser, u32 type) {
	if (parser->source_code.size && Lookup[parser->source_code[0]] == type) {
		advance_source(parser);
		return true;
	}

	return false;
}

INTERNAL void parse_error(Parser *parser, SourceLocation loc, String message) {
	if (parser->error_mode) return;

	report_diagnostic(DIAGNOSTIC_ERROR, loc, parser->filename, message);
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
			advance_source(parser);
		} else {
			break;
		}
	}

	Token token = {
		TOKEN_IDENTIFIER,
		location,
		{mark, size}
	};

	if (equal(token.content, "type")) {
		token.kind = TOKEN_KEYWORD_TYPE;
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
				if (parser->source_code.size > 2 && parser->source_code[1] == '.') break;
				if (kind == TOKEN_INTEGER) {
					kind = TOKEN_FLOAT;
				} else {
					parse_error(parser, location, "Multiple . in number literal.");
				}
			}

			size += 1;
			advance_source(parser);
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

INTERNAL void skip_comment(Parser *parser) {
	u8 c;
	while (peek_char(parser, &c)) {
		if (c == '\n' || c == '\r') return;

		advance_source(parser);
	}
}

INTERNAL Token next_token(Parser *parser);
INTERNAL Token parse_control(Parser *parser) {
	Token token = {};
	token.loc = parser->loc;

	u8 c;
	get_char(parser, &c);

	token.content = String(parser->source_code.data - 1, 1);

	switch (c) {
	case '.': {
		if (match_char(parser, '.')) {
			token.kind = TOKEN_DOUBLE_DOT;
			token.content.size += 1;
			break;
		}
		token.kind = TOKEN_DOT;
	} break;
	case ',': { token.kind = TOKEN_COMMA; } break;
	case '=': { token.kind = TOKEN_EQUAL; } break;
	case ':': {
		if (match_char(parser, ':')) {
			token.kind = TOKEN_DOUBLE_COLON;
			token.content.size += 1;
			break;
		}
		token.kind = TOKEN_COLON;
	} break;

	case ';': { token.kind = TOKEN_SEMICOLON; } break;
	case '*': { token.kind = TOKEN_ASTERISK; } break;
	case '&': { token.kind = TOKEN_AMPERSAND; } break;
	case '/': {
		if (match_char(parser, '/')) {
			skip_comment(parser);
			return next_token(parser);
			break;
		}
		token.kind = TOKEN_SLASH;
	} break;

	case '+': { token.kind = TOKEN_PLUS; } break;
	case '-': {
		if (match_char(parser, '>')) {
			token.kind = TOKEN_RIGHT_ARROW;
			token.content.size += 1;
			break;
		}
		token.kind = TOKEN_MINUS;
	} break;

	case '(': { token.kind = TOKEN_LEFT_PARENTHESIS; } break;
	case ')': { token.kind = TOKEN_RIGHT_PARENTHESIS; } break;
	case '{': { token.kind = TOKEN_LEFT_BRACE; } break;
	case '}': { token.kind = TOKEN_RIGHT_BRACE; } break;
	case '[': { token.kind = TOKEN_LEFT_BRACKET; } break;
	case ']': { token.kind = TOKEN_RIGHT_BRACKET; } break;

	default:
		parse_error(parser, token.loc, "Unsupported character.");
	}

	return token;
}

INTERNAL void skip_whitespaces(Parser *parser) {
	s32 const multi_new_line = '\n' + '\r';

	u8 c;
	while (peek_char(parser, &c) && Lookup[c] == CHAR_WHITESPACE) {
		if (c == '\n' || c == '\r') {
			advance_source(parser);

			u8 c2;
			if (peek_char(parser, &c2) && (c + c2) == multi_new_line) {
				advance_source(parser);
			}

			parser->loc.line += 1;
			parser->loc.column = 1;

			continue;
		}

		advance_source(parser);
	}
}


INTERNAL Token next_token(Parser *parser) {
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

	Token token = {0};
	token.kind = TOKEN_END_OF_INPUT;
	token.content.data = parser->source_code.data - 1;
	token.content.size = 0;
	token.loc = parser->loc;
	return token;
}

INTERNAL Token peek_token(Parser *parser) {
	parser->peek_token = next_token(parser);
	parser->has_peek = true;
	
	return parser->peek_token;
}

INTERNAL void advance_token(Parser *parser) {
	parser->previous_token = parser->current_token;
	if (parser->has_peek) {
		parser->current_token = parser->peek_token;
		parser->has_peek = false;
	} else {
		parser->current_token  = next_token(parser);
	}
}

Parser init_parser(String filename, String source) {
	Parser parser = {0};

	parser.source_code = source;
	parser.filename = filename;
	assert(parser.source_code.alloc == 0);

	parser.loc.ptr = source.data;
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
		parse_error(parser, parser->current_token.loc, error_msg);
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


INTERNAL TypeInfo parse_type_specifier(Parser *parser) {
	TypeInfo type = {0};

	consume(parser, TOKEN_IDENTIFIER, "Expected type name.");
	type.name = parser->previous_token.content;

	if (match(parser, TOKEN_LEFT_BRACKET)) {
		type.flags |= TYPE_FLAG_ARRAY;
		if (!match(parser, TOKEN_RIGHT_BRACKET)) {
			consume(parser, TOKEN_INTEGER, "Array count needs to be an integer.");
			type.array_size = to_s64(parser->previous_token.content);

			consume(parser, TOKEN_RIGHT_BRACKET, "Expected ] in array type declaration");
		}
	}
	while (match(parser, TOKEN_ASTERISK)) {
		type.pointer_depth += 1;
	}

	return type;
}

INTERNAL void *alloc_node(s32 size, u32 kind) {
	AstNode *node = (AstNode*)allocate(size);
	node->kind = kind;

	return node;
}
#define ALLOC_NODE(type, kind) (type*)alloc_node(sizeof(type), kind)

INTERNAL bool string_number_smaller_as(String number, String maximum) {
	if (number.size < maximum.size) return true;
	if (number.size > maximum.size) return false;

	for (s32 i = 0; i < number.size; i += 1) {
		if (number[i] > maximum[i]) return false;
	}

	return true;
}

INTERNAL AstExpression *parse_number_expr(Parser *parser) {
	u32 kind = parser->current_token.kind;
	advance_token(parser);

	if (kind == TOKEN_INTEGER) {
		if (!string_number_smaller_as(parser->previous_token.content, "18446744073709551615")) {
			parse_error(parser, parser->previous_token.loc, "Integer literal is too large to fit into u64.");
			return 0;
		}

		AstIntegerLiteral *literal = ALLOC_NODE(AstIntegerLiteral, AST_INTEGER_LITERAL);
		literal->value = to_u64(parser->previous_token.content);
		literal->location = parser->previous_token.loc;

		return literal;
	} else if (kind == TOKEN_FLOAT) {
		die("Currently no float implemented in parser");
	} else {
		parse_error(parser, parser->current_token.loc, "Expected numerical expression.");
	}

	return 0;
}

INTERNAL AstExpression *parse_identifier_expr(Parser *parser) {
	AstIdentifier *ident = ALLOC_NODE(AstIdentifier, AST_IDENTIFIER);

	consume(parser, TOKEN_IDENTIFIER, "Expected identifier.");
	ident->name = parser->previous_token.content;
	ident->location = parser->previous_token.loc;

	return ident;
}

enum {
	PREC_NONE,
	PREC_ASSIGNMENT,
	PREC_TERM,
	PREC_FACTOR,
	PREC_UNARY,
	PREC_RANGE,
	PREC_CALL,
	PREC_DOT,
	PREC_PRIMARY
};
INTERNAL AstNode *parse_statement(Parser *parser);
INTERNAL AstExpression *parse_expression(Parser *parser, u32 precedence);
INTERNAL AstExpression *parse_array_declaration(Parser *parser);

INTERNAL AstNode *parse_type_declaration(Parser *parser, Token name) {
	AstTypeDeclaration *decl = ALLOC_NODE(AstTypeDeclaration, AST_TYPE_DECLARATION);
	decl->name = name.content;
	decl->declaration.kind = TYPE_STRUCT;
	decl->declaration.filename = parser->filename;
	decl->declaration.location = name.loc;

	consume(parser, TOKEN_KEYWORD_TYPE, "Missing keyword type.");
	decl->location = parser->previous_token.loc;
	if (match(parser, TOKEN_LEFT_BRACE)) {
		decl->decl_kind = TYPE_DECL_STRUCT;

		Token left_brace = parser->previous_token;

		if (match(parser, TOKEN_RIGHT_BRACE)) {
			parse_error(parser, parser->previous_token.loc, "Empty structs are currently not supported.");
			return decl;
		}

		do {
			TypeField field = {0};

			consume(parser, TOKEN_IDENTIFIER, "Missing member name in declaration of type.");
			field.name = parser->previous_token.content;
			field.location = parser->previous_token.loc;

			consume(parser, TOKEN_COLON, "Missing : in field declaration.");
			field.type = parse_type_specifier(parser);

			consume(parser, TOKEN_SEMICOLON, "Missing ; to end type field.");

			append(&decl->declaration.fields, field);
		} while (!current_token_is(parser, TOKEN_RIGHT_BRACE));

		if (current_token_is(parser, TOKEN_END_OF_INPUT)) {
			parse_error(parser, left_brace.loc, "Missing } for type declaration.");
			return decl;
		}

		consume(parser, TOKEN_RIGHT_BRACE, "Missing } to end declaration of type.");
	} else if (match(parser, TOKEN_EQUAL)) {
		decl->decl_kind = TYPE_DECL_BASIC;
		decl->expr = parse_expression(parser, PREC_ASSIGNMENT);

		consume(parser, TOKEN_SEMICOLON, "Missing ; .");
	} else {
		parse_error(parser, parser->current_token.loc, "Unknown declaration kind.");
	}

	return decl;
}

INTERNAL AstExpression *parse_reference(Parser *parser) {
	AstReference *ref = ALLOC_NODE(AstReference, AST_REFERENCE);
	ref->location = parser->current_token.loc;

	consume(parser, TOKEN_AMPERSAND, "Expected & .");
	ref->expr = parse_expression(parser, PREC_UNARY);

	return ref;
}

INTERNAL AstExpression *parse_dereference(Parser *parser) {
	AstDereference *deref = ALLOC_NODE(AstDereference, AST_DEREFERENCE);
	deref->location = parser->current_token.loc;

	consume(parser, TOKEN_ASTERISK, "Expected * .");
	deref->expr = parse_expression(parser, PREC_UNARY);

	return deref;
}

INTERNAL u32 operator_precedence(u32 kind) {
	switch (kind) {
	case TOKEN_PLUS: return PREC_TERM;
	case TOKEN_MINUS: return PREC_TERM;
	case TOKEN_ASTERISK: return PREC_FACTOR;

	case TOKEN_DOUBLE_DOT: return PREC_RANGE;
	}

	return PREC_NONE;
}

INTERNAL u32 PrecedenceTable[] = {
	PREC_TERM,   // BINARY_OP_ADD,
	PREC_TERM,   // BINARY_OP_SUB,
	PREC_FACTOR, // BINARY_OP_MUL,
	PREC_FACTOR, // BINARY_OP_DIV,
};

INTERNAL AstExpression *parse_binary_operator(Parser *parser, AstExpression *lhs, u32 op) {
	AstBinaryOperator *bin = ALLOC_NODE(AstBinaryOperator, AST_BINARY_OPERATOR);
	bin->location = parser->previous_token.loc;

	bin->op = op;
	bin->lhs = lhs;
	bin->rhs = parse_expression(parser, PrecedenceTable[op] + 1);

	return bin;
}

INTERNAL AstExpression *parse_range_expression(Parser *parser, AstExpression *lhs) {
	AstRange *range = ALLOC_NODE(AstRange, AST_RANGE);
	range->location = parser->previous_token.loc;

	range->start = lhs;
	range->end = parse_expression(parser, PREC_RANGE + 1);

	return range;
}

INTERNAL AstExpression *parse_grouping(Parser *parser) {
	AstExpression *expr = 0;
	consume(parser, TOKEN_LEFT_PARENTHESIS, "Expected ( .");

	expr = parse_expression(parser, PREC_ASSIGNMENT);

	consume(parser, TOKEN_RIGHT_PARENTHESIS, "Missing ) .");

	return expr;
}

INTERNAL AstExpression *parse_negation(Parser *parser) {
	consume(parser, TOKEN_MINUS, "Missing - .");

	AstNegate *negate = ALLOC_NODE(AstNegate, AST_NEGATE);
	negate->expr = parse_expression(parser, PREC_UNARY);
	negate->location = parser->previous_token.loc;

	return negate;
}

INTERNAL AstExpression *parse_expression(Parser *parser, u32 precedence) {
	AstExpression *lhs;

	switch (parser->current_token.kind) {
	case TOKEN_INTEGER:
	case TOKEN_FLOAT:
		lhs = parse_number_expr(parser);
	break;

	case TOKEN_IDENTIFIER:
		lhs = parse_identifier_expr(parser);
	break;

	case TOKEN_LEFT_BRACKET:
		lhs = parse_array_declaration(parser);
	break;

	case TOKEN_AMPERSAND:
		lhs = parse_reference(parser);
	break;

	case TOKEN_MINUS:
		lhs = parse_negation(parser);
	break;

	case TOKEN_LEFT_PARENTHESIS:
		lhs = parse_grouping(parser);
	break;

	default:
		parse_error(parser, parser->current_token.loc, "Expected expression.");
		advance_token(parser);
		return 0;
	}

	while (precedence <= operator_precedence(parser->current_token.kind)) {
		advance_token(parser);

		switch (parser->previous_token.kind) {
		case TOKEN_PLUS: { lhs = parse_binary_operator(parser, lhs, BINARY_OP_ADD); } break;
		case TOKEN_MINUS: { lhs = parse_binary_operator(parser, lhs, BINARY_OP_SUB); } break;
		case TOKEN_ASTERISK: { lhs = parse_binary_operator(parser, lhs, BINARY_OP_MUL); } break;

		case TOKEN_DOUBLE_DOT: { lhs = parse_range_expression(parser, lhs); } break;

		default:
			parse_error(parser, parser->previous_token.loc, "Unknown binary operator.");
			advance_token(parser);
		}
	}

	return lhs;
}

INTERNAL AstNode *parse_variable_declaration(Parser *parser, Token name) {
	AstVariableDeclaration *decl = ALLOC_NODE(AstVariableDeclaration, AST_VARIABLE_DECLARATION);
	decl->name = name.content;
	decl->location = name.loc;

	if (current_token_is(parser, TOKEN_IDENTIFIER)) {
		decl->type = parse_type_specifier(parser);
	}

	if (match(parser, TOKEN_EQUAL)) {
	    decl->expr = parse_expression(parser, PREC_ASSIGNMENT);
	}

	consume(parser, TOKEN_SEMICOLON, "Missing ; .");

	return decl;
}

INTERNAL AstNode *parse_function_declaration(Parser *parser, Token name) {
	AstFunctionDeclaration *decl = ALLOC_NODE(AstFunctionDeclaration, AST_FUNCTION_DECLARATION);
	decl->name = name.content;
	decl->location = name.loc;

	consume(parser, TOKEN_LEFT_PARENTHESIS, "Expected ( .");
	decl->location = parser->previous_token.loc;

	if (!match(parser, TOKEN_RIGHT_PARENTHESIS)) {
		do {
			AstParameter param = {};
			consume(parser, TOKEN_IDENTIFIER, "Identifier or ) expected");
			param.name = parser->previous_token.content;
			param.location = parser->previous_token.loc;

			consume(parser, TOKEN_COLON, "Missing : after function parameter name.");
			param.type = parse_type_specifier(parser);

			append(&decl->params, param);
		} while (match(parser, TOKEN_COMMA));

		consume(parser, TOKEN_RIGHT_PARENTHESIS, "Missing ) in function declaration.");
	}

	if (match(parser, TOKEN_RIGHT_ARROW)) {
		do {
			append(&decl->return_types, parse_type_specifier(parser));
		} while (match(parser, TOKEN_COMMA));
	}

	consume(parser, TOKEN_LEFT_BRACE, "Missing function body.");
	Token left_brace = parser->previous_token;

	while (!match(parser, TOKEN_RIGHT_BRACE)) {
		if (current_token_is(parser, TOKEN_END_OF_INPUT)) {
			parse_error(parser, left_brace.loc, "Missing } to end function body.");
			return decl;
		}

		append(&decl->body.statements, parse_statement(parser));
	}

	return decl;
}

INTERNAL AstExpression *parse_array_declaration(Parser *parser) {
	consume(parser, TOKEN_LEFT_BRACKET, "Expected [ for array declaration.");

	AstArrayDeclaration *decl = ALLOC_NODE(AstArrayDeclaration, AST_ARRAY_DECLARATION);
	decl->location = parser->previous_token.loc;

	if (match(parser, TOKEN_RIGHT_BRACKET)) return decl;

	do {
		if (current_token_is(parser, TOKEN_RIGHT_BRACE)) break;

		append(&decl->elements, parse_expression(parser, PREC_ASSIGNMENT));
	} while (match(parser, TOKEN_COMMA));

	consume(parser, TOKEN_RIGHT_BRACKET, "Missing ] in array declaration.");

	return decl;
}

INTERNAL AstNode *parse_declaration(Parser *parser) {
	consume(parser, TOKEN_IDENTIFIER, "Expected identifier for declaration.");
	Token identifier = parser->previous_token;

	if (match(parser, TOKEN_DOUBLE_COLON)) {
		switch (parser->current_token.kind) {
		case TOKEN_KEYWORD_TYPE: {
			return parse_type_declaration(parser, identifier);
		} break;

		case TOKEN_LEFT_PARENTHESIS: {
			return parse_function_declaration(parser, identifier);
		} break;

		default:
			parse_error(parser, parser->current_token.loc, "Expected declaration.");
		}
	} else if (match(parser, TOKEN_COLON)) {
		switch (parser->current_token.kind) {
		case TOKEN_IDENTIFIER:
		case TOKEN_EQUAL: {
			return parse_variable_declaration(parser, identifier);
		} break;
		}
	} else {
		parse_error(parser, parser->current_token.loc, "Missing : for declaration.");
	}

	return 0;
}

INTERNAL AstNode *parse_return(Parser *parser) {
	AstReturn *ret = ALLOC_NODE(AstReturn, AST_RETURN);
	ret->location = parser->current_token.loc;

	consume(parser, TOKEN_KEYWORD_RETURN, "Expected return.");

	if (match(parser, TOKEN_SEMICOLON)) return ret;
	do {
		append(&ret->values, parse_expression(parser, PREC_ASSIGNMENT));
	} while (match(parser, TOKEN_COMMA));

	consume(parser, TOKEN_SEMICOLON, "Missing ; .");

	return ret;
}

INTERNAL AstNode *parse_statement(Parser *parser) {
	switch (parser->current_token.kind) {
	case TOKEN_IDENTIFIER: {
		Token token = peek_token(parser);
		if (token.kind == TOKEN_DOUBLE_COLON || token.kind == TOKEN_COLON) {
			return parse_declaration(parser);
		} else {
			AstExpression *expr = parse_expression(parser, PREC_ASSIGNMENT);
			consume(parser, TOKEN_SEMICOLON, "Missing ; .");
			return expr;
		}
	} break;

	case TOKEN_KEYWORD_RETURN: {
		return parse_return(parser);
	} break;
	}

	parse_error(parser, parser->current_token.loc, "Expected statement.");
	advance_token(parser);

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

		case TOKEN_END_OF_INPUT:
			goto END;
		break;
		}

		advance_token(parser);
	}

END:
	parser->error_mode = false;
}

AbstractSyntaxTree parse_as_knot_code(Parser *parser) {
	AbstractSyntaxTree result = {};

	result.filename = parser->filename;
	while (!current_token_is(parser, TOKEN_END_OF_INPUT)) {
		if (parser->error_mode) {
			synchronize(parser);
		}
		append(&result.global_scope.statements, parse_statement(parser));
	}

	return result;
}



INTERNAL void print_indentation(s32 depth) {
	for (s32 i = 0; i < depth * 2; i += 1) print(" ");
}

INTERNAL void print_operator(u32 op) {
	switch (op) {
	case BINARY_OP_ADD: print("+"); break;
	case BINARY_OP_SUB: print("-"); break;
	case BINARY_OP_MUL: print("*"); break;
	case BINARY_OP_DIV: print("/"); break;
	}
}

INTERNAL void print_ast_expression(AstExpression *expr) {
	switch (expr->kind) {
	case AST_ERROR: {
		print("Node type not set.");
	} break;

	case AST_IDENTIFIER: {
		AstIdentifier *identifier = (AstIdentifier*)expr;
		print("%S", pr(identifier->name));
	} break;

	case AST_BINARY_OPERATOR: {
		AstBinaryOperator *op = (AstBinaryOperator*)expr;
		print("(");
		print_ast_expression(op->lhs);
		print_operator(op->op);
		print_ast_expression(op->rhs);
		print(")");
	} break;

	default:
		print("Can't print ast node with type %d", expr->kind);
	}
}

INTERNAL void print_ast_node(AstNode *node, s32 depth) {
	print_indentation(depth);

	switch (node->kind) {
	case AST_ERROR: {
		print("Node type not set.");
	} break;

	case AST_TYPE_DECLARATION:{
		AstTypeDeclaration *decl = (AstTypeDeclaration*)node;
		print("Declare struct: %S {", pr(decl->name));

		if (decl->declaration.fields.size) print("%S: %S", pr(decl->declaration.fields[0].name), pr(decl->declaration.fields[0].type.name));
		for (s32 i = 1; i < decl->declaration.fields.size; i += 1) {
			print(", %S: %S", pr(decl->declaration.fields[i].name), pr(decl->declaration.fields[i].type.name));
		}

		print("}\n");
	} break;

	case AST_VARIABLE_DECLARATION: {
		AstVariableDeclaration *decl = (AstVariableDeclaration*)node;
		print("Declare variable: %S: %S", pr(decl->name), pr(decl->type.name));
		if (decl->type.array_size) print("[%d]", decl->type.array_size);
		if (decl->type.pointer_depth) for (s32 i = 0; i < decl->type.pointer_depth; i += 1) print("*");
		print("\n");
	} break;

	case AST_FUNCTION_DECLARATION: {
		AstFunctionDeclaration *decl = (AstFunctionDeclaration*)node;
		print("Declare function: %S with body:\n", pr(decl->name));

		for (s32 i = 0; i < decl->body.statements.size; i += 1) {
			print_ast_node(decl->body.statements[i], depth + 1);
		}
	} break;

	case AST_RETURN: {
		print("return: ");

		AstReturn *ret = (AstReturn*)node;
		for (s32 i = 0; i < ret->values.size; i += 1) {
			print_ast_expression(ret->values[i]);
		}
		print("\n");
	} break;

	default:
		print("Can't print ast node with type %d\n", node->kind);
	}
}

void print_ast(AbstractSyntaxTree *result) {
	for (s32 i = 0; i < result->global_scope.statements.size; i += 1) {
		print_ast_node(result->global_scope.statements[i], 0);
	}
}

