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


INTERNAL TypeSpecifier VoidType = {
	String("void")
};

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

INTERNAL bool match_char(Parser *parser, u8 c) {
	if (parser->source_code.size && parser->source_code.data[0] == c) {
		advance_source(parser, 1);
		return true;
	}

	return false;
}

INTERNAL void report_error(Parser *parser, SourceLocation loc, u8 *pos, String message) {
	if (parser->error_mode) return;

	SyntaxError error = {parser->filename, message, loc, pos};

	append(&parser->errors, error);
	parser->error_mode = true;
}
INTERNAL void report_error(Parser *parser, Token token, String message) {
	report_error(parser, token.loc, token.content.data, message);
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
					report_error(parser, location, mark, "Multiple . in number literal.");
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

	token.content = String(parser->source_code.data - 1, 1);

	switch (c) {
	case '.': { token.kind = TOKEN_DOT; } break;
	case ',': { token.kind = TOKEN_COMMA; } break;
	case '=': { token.kind = TOKEN_EQUAL; } break;
	case ':': { token.kind = TOKEN_COLON; } break;
	case ';': { token.kind = TOKEN_SEMICOLON; } break;
	case '*': { token.kind = TOKEN_ASTERISK; } break;
	case '&': { token.kind = TOKEN_AMPERSAND; } break;

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
		report_error(parser, token, "Unsupported character.");
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

	Token token = {0};
	token.kind = TOKEN_END_OF_INPUT;
	token.content.data = parser->source_code.data - 1;
	token.content.size = 0;
	return token;
}

INTERNAL void advance_token(Parser *parser) {
	parser->previous_token = parser->current_token;
	parser->current_token  = next_token(parser);
}

Parser init_parser(String filename, String source) {
	Parser parser = {0};

	parser.source_code = source;
	parser.filename = filename;
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
		report_error(parser, parser->current_token, error_msg);
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


INTERNAL TypeSpecifier parse_type_specifier(Parser *parser) {
	TypeSpecifier type = {0};

	consume(parser, TOKEN_IDENTIFIER, "Expected type name.");
	type.name = parser->previous_token.content;

	if (match(parser, TOKEN_LEFT_BRACKET)) {
		consume(parser, TOKEN_INTEGER, "Array count needs to be an integer.");
		type.array_size = convert_string_to_s64(parser->previous_token.content.data, parser->previous_token.content.size);

		consume(parser, TOKEN_RIGHT_BRACKET, "Expected ] in array type declaration");
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

INTERNAL AstNode *parse_struct_declaration(Parser *parser, Token name) {
	AstStructDeclaration *decl = ALLOC_NODE(AstStructDeclaration, AST_STRUCT_DECLARATION);
	decl->name = name.content;
	consume(parser, TOKEN_KEYWORD_STRUCT, "Missing keyword struct.");
	consume(parser, TOKEN_LEFT_BRACE, "Expected { in struct declaration.");
	Token left_brace = parser->previous_token;

	if (match(parser, TOKEN_RIGHT_BRACE)) {
		report_error(parser, parser->previous_token, "Empty structs are currently not supported.");
		return decl;
	}

	do {

		AstStructMember member = {0};

		consume(parser, TOKEN_IDENTIFIER, "Missing member name in declaration of struct.");
		member.name = parser->previous_token.content;

		consume(parser, TOKEN_COLON, "Missing : in member declaration");
		member.type = parse_type_specifier(parser);

		consume(parser, TOKEN_SEMICOLON, "Missing ; to end struct member.");

		append(&decl->members, member);
	} while (!current_token_is(parser, TOKEN_RIGHT_BRACE));

	if (current_token_is(parser, TOKEN_END_OF_INPUT)) {
		report_error(parser, left_brace, "Missing } for struct declaration.");
		return decl;
	}

	consume(parser, TOKEN_RIGHT_BRACE, "Missing } to end declaration of struct");

	return decl;
}

INTERNAL AstExpression *parse_number_expr(Parser *parser) {
	u32 kind = parser->current_token.kind;
	if (kind == TOKEN_INTEGER || kind == TOKEN_FLOAT) {
		AstNumericLiteral *literal = ALLOC_NODE(AstNumericLiteral, AST_NUMERIC_LITERAL);
		literal->value = parser->current_token.content;
		advance_token(parser);

		return literal;
	}

	report_error(parser, parser->current_token, "Expected numerical expression.");
	return 0;
}

INTERNAL AstExpression *parse_identifier_expr(Parser *parser) {
	AstIdentifier *ident = ALLOC_NODE(AstIdentifier, AST_IDENTIFIER);

	consume(parser, TOKEN_IDENTIFIER, "Expected identifier.");
	ident->name = parser->previous_token.content;

	return ident;
}

enum {
	PREC_NONE,
	PREC_ASSIGNMENT,
	PREC_TERM,
	PREC_FACTOR,
	PREC_UNARY,
	PREC_CALL,
	PREC_DOT,
	PREC_PRIMARY
};
INTERNAL AstNode *parse_statement(Parser *parser);
INTERNAL AstExpression *parse_expression(Parser *parser, u32 precedence);
INTERNAL AstExpression *parse_array_declaration(Parser *parser);

INTERNAL AstExpression *parse_reference(Parser *parser) {
	AstReference *ref = ALLOC_NODE(AstReference, AST_REFERENCE);

	consume(parser, TOKEN_AMPERSAND, "Expected & .");
	ref->expr = parse_expression(parser, PREC_UNARY);

	return ref;
}

INTERNAL AstExpression *parse_dereference(Parser *parser) {
	AstDereference *deref = ALLOC_NODE(AstDereference, AST_DEREFERENCE);

	consume(parser, TOKEN_ASTERISK, "Expected * .");
	deref->expr = parse_expression(parser, PREC_UNARY);

	return deref;
}

INTERNAL u32 operator_precedence(u32 kind) {
	switch (kind) {
	case TOKEN_PLUS: return PREC_TERM;
	case TOKEN_MINUS: return PREC_TERM;
	case TOKEN_ASTERISK: return PREC_FACTOR;
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
	bin->op = op;
	bin->lhs = lhs;
	bin->rhs = parse_expression(parser, PrecedenceTable[op] + 1);

	return bin;
}

INTERNAL AstExpression *parse_grouping(Parser *parser) {
	AstExpression *expr = 0;
	consume(parser, TOKEN_LEFT_PARENTHESIS, "Expected ( .");

	expr = parse_expression(parser, PREC_ASSIGNMENT);

	consume(parser, TOKEN_RIGHT_PARENTHESIS, "Missing ) .");

	return expr;
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

	case TOKEN_LEFT_PARENTHESIS:
		lhs = parse_grouping(parser);
	break;

	default:
		report_error(parser, parser->current_token, "Expected expression.");
		advance_token(parser);
		return 0;
	}

	while (precedence <= operator_precedence(parser->current_token.kind)) {
		advance_token(parser);

		switch (parser->previous_token.kind) {
		case TOKEN_PLUS: { lhs = parse_binary_operator(parser, lhs, BINARY_OP_ADD); } break;
		case TOKEN_MINUS: { lhs = parse_binary_operator(parser, lhs, BINARY_OP_SUB); } break;
		case TOKEN_ASTERISK: { lhs = parse_binary_operator(parser, lhs, BINARY_OP_MUL); } break;

		default:
			report_error(parser, parser->previous_token, "Unknown binary operator.");
			advance_token(parser);
		}
	}

	return lhs;
}

INTERNAL AstNode *parse_variable_declaration(Parser *parser, Token name) {
	AstVariableDeclaration *decl = ALLOC_NODE(AstVariableDeclaration, AST_VARIABLE_DECLARATION);
	decl->name = name.content;

	if (current_token_is(parser, TOKEN_IDENTIFIER)) {
		decl->type = parse_type_specifier(parser);
	}

	consume(parser, TOKEN_EQUAL, "Missing = in declaration.");

	decl->expr = parse_expression(parser, PREC_ASSIGNMENT);

	consume(parser, TOKEN_SEMICOLON, "Missing ; .");

	return decl;
}

INTERNAL AstNode *parse_function_declaration(Parser *parser, Token name) {
	AstFuncitonDeclaration *decl = ALLOC_NODE(AstFuncitonDeclaration, AST_FUNCTION_DECLARATION);
	decl->name = name.content;

	consume(parser, TOKEN_LEFT_PARENTHESIS, "Expected ( .");

	if (!match(parser, TOKEN_RIGHT_PARENTHESIS)) {
		do {
			AstParameter param = {};
			consume(parser, TOKEN_IDENTIFIER, "Identifier or ) expected");
			param.name = parser->previous_token.content;

			consume(parser, TOKEN_COLON, "Missing : after function parameter name.");
			param.type = parse_type_specifier(parser);
		} while (match(parser, TOKEN_COMMA));

		consume(parser, TOKEN_RIGHT_PARENTHESIS, "Missing ) in function declaration.");
	}

	if (match(parser, TOKEN_RIGHT_ARROW)) {
		// TODO: muliple return types
		append(&decl->return_types, parse_type_specifier(parser));
	}

	consume(parser, TOKEN_LEFT_BRACE, "Missing function body.");
	Token left_brace = parser->previous_token;

	do {
		if (current_token_is(parser, TOKEN_END_OF_INPUT)) {
			report_error(parser, left_brace, "Missing } to end function body.");
			return decl;
		}

		append(&decl->body.statements, parse_statement(parser));
	} while (!match(parser, TOKEN_RIGHT_BRACE));

	return decl;
}

INTERNAL AstExpression *parse_array_declaration(Parser *parser) {
	consume(parser, TOKEN_LEFT_BRACKET, "Expected [ for array declaration.");

	AstArrayDeclaration *decl = ALLOC_NODE(AstArrayDeclaration, AST_ARRAY_DECLARATION);

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

	if (match(parser, TOKEN_COLON)) {
		switch (parser->current_token.kind) {
		case TOKEN_KEYWORD_STRUCT: {
			return parse_struct_declaration(parser, identifier);
		} break;

		case TOKEN_IDENTIFIER:
		case TOKEN_EQUAL: {
			return parse_variable_declaration(parser, identifier);
		} break;

		case TOKEN_LEFT_PARENTHESIS: {
			return parse_function_declaration(parser, identifier);
		} break;
		}
		report_error(parser, parser->current_token, "Missing type specifier.");
	}

	return 0;
}

INTERNAL AstNode *parse_return(Parser *parser) {
	AstReturn *ret = ALLOC_NODE(AstReturn, AST_RETURN);

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
		return parse_declaration(parser);
	} break;

	case TOKEN_KEYWORD_RETURN: {
		return parse_return(parser);
	} break;
	}

	report_error(parser, parser->current_token, "Expected statement.");
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

	case AST_STRUCT_DECLARATION:{
		AstStructDeclaration *decl = (AstStructDeclaration*)node;
		print("Declare struct: %S {", pr(decl->name));

		if (decl->members.size) print("%S: %S", pr(decl->members[0].name), pr(decl->members[0].type.name));
		for (s32 i = 1; i < decl->members.size; i += 1) {
			print(", %S: %S", pr(decl->members[i].name), pr(decl->members[i].type.name));
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
		AstFuncitonDeclaration *decl = (AstFuncitonDeclaration*)node;
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

void print_ast(ParseResult *result) {
	for (s32 i = 0; i < result->nodes.size; i += 1) {
		print_ast_node(result->nodes[i], 0);
	}
}

