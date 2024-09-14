#include "parser.h"

#include "ast.h"
#include "definitions.h"
#include "knot.h"
#include "io.h"
#include "memory.h"


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

    report_diagnostic(parser->env, DIAGNOSTIC_ERROR, loc, message);
    parser->error_mode = true;
}

INTERNAL Token parse_identifier(Parser *parser) {
    u8 *mark = parser->source_code.data;
    s32 size = 0;

    SourceLocation location = parser->loc;

    u8 c;
    while (peek_char(parser, &c)) {
        s32 type = Lookup[c];
        if (type == CHAR_CHARACTER || type == CHAR_DIGIT || c == '_') {
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
    TokenKind kind = TOKEN_INTEGER;

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

INTERNAL Token parse_string(Parser *parser) {
    SourceLocation location = parser->loc;

    u8 *mark = parser->source_code.data;
    s32 size = 0;
    TokenKind kind = TOKEN_STRING;

    u8 c;
    while (get_char(parser, &c) && c != '"') {
        size += 1;
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
        token.kind = TOKEN_COLON;
    } break;

    case ';': { token.kind = TOKEN_SEMICOLON; } break;
    case '*': { token.kind = TOKEN_ASTERISK; } break;
    case '&': {
        token.kind = TOKEN_AMPERSAND;
    } break;

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

    case '"': {
        return parse_string(parser);
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

    Token token = {};
    token.kind = TOKEN_END_OF_INPUT;
    token.content.data = parser->source_code.data - 1;
    token.content.size = 0;
    token.loc = parser->loc;
    return token;
}

INTERNAL Token peek_token(Parser *parser, s32 tokens) {
    assert(tokens > 0 && tokens < PARSER_MAX_PEEK);

    if (parser->peek_count > 0 && parser->peek_count < tokens) {
        s32 space = (tokens - parser->peek_count) * sizeof(Token);
        copy_memory(parser->peek + space, parser->peek, parser->peek_count * sizeof(Token));

        for (s32 i = tokens - parser->peek_count; i > 0; i -= 1) {
            parser->peek[i - 1] = next_token(parser);
        }

        parser->peek_count = tokens;
    } else if (parser->peek_count == 0)  {
        for (s32 i = tokens; i > 0; i -= 1) {
            parser->peek[i - 1] = next_token(parser);
        }

        parser->peek_count = tokens;
    }

    return parser->peek[parser->peek_count - tokens];
}

INTERNAL void advance_token(Parser *parser) {
    parser->previous_token = parser->current_token;

    if (parser->peek_count) {
        parser->peek_count -= 1;
        parser->current_token = parser->peek[parser->peek_count];
    } else {
        parser->current_token = next_token(parser);
    }
}

Parser init_parser(String filename, String source) {
    Parser parser = {0};

    parser.source_code = source;
    parser.filename = filename;

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

INTERNAL AstTypeSpecifier parse_type_specifier(Parser *parser) {
     AstTypeSpecifier result = {};

    consume(parser, TOKEN_IDENTIFIER, "Expected type name.");
    result.type.name = parser->previous_token.content;
    result.location  = parser->previous_token.loc;

    if (match(parser, TOKEN_LEFT_BRACKET)) {
        if (match(parser, TOKEN_RIGHT_BRACKET)) {
            result.type.array_size = -1;
        } else if (match(parser, TOKEN_INTEGER)) {
            result.type.array_size = (s32)to_s64(parser->previous_token.content);

            consume(parser, TOKEN_RIGHT_BRACKET, "Expected ] in array type declaration");
        } else {
            parse_error(parser, parser->current_token.loc, "Array count needs to be an integer literal.");
        }
    }
    while (match(parser, TOKEN_AMPERSAND)) {
        result.type.pointer_depth += 1;
    }

    return result;
}

INTERNAL void *alloc_node(s32 size, AstNodeKind kind) {
    AstNode *node = (AstNode*)ALLOC(default_allocator(), u8, size);
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

INTERNAL AstExpression *parse_string_expr(Parser *parser) {
    AstString *string = ALLOC_NODE(AstString, AST_STRING);

    consume(parser, TOKEN_STRING, "Expected string.");
    string->content  = parser->previous_token.content;
    string->location = parser->previous_token.loc;

    return string;
}

INTERNAL AstIdentifier *parse_identifier_expr(Parser *parser) {
    if (!consume(parser, TOKEN_IDENTIFIER, "Expected identifier.")) return 0;

    AstIdentifier *ident = ALLOC_NODE(AstIdentifier, AST_IDENTIFIER);
    ident->name     = parser->previous_token.content;
    ident->location = parser->previous_token.loc;

    return ident;
}

INTERNAL b32 parse_identifier_expr_as_value(Parser *parser, AstIdentifier *ident) {
    if (!match(parser, TOKEN_IDENTIFIER)) return false;
    ident->name     = parser->previous_token.content;
    ident->location = parser->previous_token.loc;

    return true;
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
INTERNAL AstExpression *parse_single_expression(Parser *parser, u32 precedence);
INTERNAL Array<AstExpression*> parse_expression(Parser *parser);

INTERNAL AstExpression *parse_array_declaration(Parser *parser);


INTERNAL AstExpression *parse_reference(Parser *parser) {
    AstReference *ref = ALLOC_NODE(AstReference, AST_REFERENCE);
    ref->location = parser->current_token.loc;

    consume(parser, TOKEN_AMPERSAND, "Expected & .");
    ref->expr = parse_single_expression(parser, PREC_UNARY);

    return ref;
}

INTERNAL AstExpression *parse_dereference(Parser *parser, AstExpression *pointer) {
    AstDereference *deref = ALLOC_NODE(AstDereference, AST_DEREFERENCE);
    deref->location = parser->previous_token.loc;
    deref->expr = pointer;

    return deref;
}

INTERNAL u32 operator_precedence(u32 kind) {
    switch (kind) {
    case TOKEN_PLUS: return PREC_TERM;
    case TOKEN_MINUS: return PREC_TERM;
    case TOKEN_ASTERISK: return PREC_FACTOR;

    case TOKEN_EQUAL: return PREC_ASSIGNMENT;

    case TOKEN_AMPERSAND: return PREC_UNARY;

    case TOKEN_DOUBLE_DOT: return PREC_RANGE;

    case TOKEN_LEFT_PARENTHESIS: return PREC_CALL;
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
    bin->rhs = parse_single_expression(parser, PrecedenceTable[op] + 1);

    return bin;
}

INTERNAL AstExpression *parse_assignment(Parser *parser, AstExpression *thing) {
    AstAssignment *ass = ALLOC_NODE(AstAssignment, AST_ASSIGNMENT);
    ass->location = parser->previous_token.loc;

    ass->thing = thing;
    ass->value = parse_single_expression(parser, PREC_ASSIGNMENT);

    return ass;
}

INTERNAL AstExpression *parse_range_expression(Parser *parser, AstExpression *lhs) {
    AstRange *range = ALLOC_NODE(AstRange, AST_RANGE);
    range->location = parser->previous_token.loc;

    range->start = lhs;
    range->end = parse_single_expression(parser, PREC_RANGE + 1);

    return range;
}

INTERNAL AstExpression *parse_grouping(Parser *parser) {
    AstExpression *expr = 0;
    consume(parser, TOKEN_LEFT_PARENTHESIS, "Expected ( .");

    expr = parse_single_expression(parser, PREC_ASSIGNMENT);

    consume(parser, TOKEN_RIGHT_PARENTHESIS, "Missing ) .");

    return expr;
}

INTERNAL AstExpression *parse_negation(Parser *parser) {
    consume(parser, TOKEN_MINUS, "Missing - .");

    AstNegate *negate = ALLOC_NODE(AstNegate, AST_NEGATE);
    negate->expr = parse_single_expression(parser, PREC_UNARY);
    negate->location = parser->previous_token.loc;

    return negate;
}

INTERNAL DArray<AstExpression*> CallArgumentBuilder;
INTERNAL AstExpression *parse_call(Parser *parser, AstExpression *expr) {
    AstCall *call = ALLOC_NODE(AstCall, AST_CALL);
    call->functor = expr;
    call->location = expr->location;

    if (!match(parser, TOKEN_RIGHT_PARENTHESIS)) {
        CallArgumentBuilder.size = 0;

        do {
            AstExpression *expr = parse_single_expression(parser, PREC_ASSIGNMENT);
            if (expr) {
                append(CallArgumentBuilder, expr);
            } else {
                return 0;
            }
        } while (match(parser, TOKEN_COMMA));

        call->arguments = allocate_array(CallArgumentBuilder);
        consume(parser, TOKEN_RIGHT_PARENTHESIS, "Missing ) in function call.");
    }

    return call;
}

INTERNAL AstExpression *parse_variable_declaration(Parser *parser, Array<AstExpression> names) {
    consume(parser, TOKEN_IDENTIFIER, "Identifier expected in variable declaration.");

    for (s64 i = 0; i < names.size; i += 1) {
        if (names[i].kind != AST_IDENTIFIER) {
            parse_error(parser, names[i].location, "Variable declaration expects an identifier.");
        }
    }

    AstVariableDeclaration *decl = ALLOC_NODE(AstVariableDeclaration, AST_VARIABLE_DECLARATION);
    decl->names    = names;

    decl->location = parser->current_token.loc;
    consume(parser, TOKEN_COLON, "Expected : in variable declaration.");

    if (current_token_is(parser, TOKEN_IDENTIFIER)) {
        decl->type = parse_type_specifier(parser).type;
    }

    return decl;
}

INTERNAL AstLambda *parse_lambda(Parser *parser, AstIdentifier **name = 0);

INTERNAL AstExpression *parse_single_expression(Parser *parser, u32 precedence) {
    AstExpression *lhs;

    switch (parser->current_token.kind) {
    case TOKEN_INTEGER:
    case TOKEN_FLOAT: {
        lhs = parse_number_expr(parser);
    } break;

    case TOKEN_STRING: {
        lhs = parse_string_expr(parser);
    } break;

    case TOKEN_IDENTIFIER: {
        lhs = parse_identifier_expr(parser);
    } break;

    case TOKEN_LEFT_BRACKET: {
        lhs = parse_array_declaration(parser);
    } break;

    case TOKEN_AMPERSAND: {
        lhs = parse_reference(parser);
    } break;

    case TOKEN_MINUS: {
        lhs = parse_negation(parser);
    } break;

    case TOKEN_LEFT_PARENTHESIS: {
        if (peek_token(parser, 2).kind == TOKEN_COLON ||
            peek_token(parser, 1).kind == TOKEN_RIGHT_PARENTHESIS) {

            // TODO: Still declare the named lambda or just keep ignoring the name?
            lhs = parse_lambda(parser, 0);
        } else {
            lhs = parse_grouping(parser);
        }
    } break;

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

        case TOKEN_AMPERSAND: { lhs = parse_dereference(parser, lhs); } break;

        case TOKEN_EQUAL: { lhs = parse_assignment(parser, lhs); } break;

        case TOKEN_DOUBLE_DOT: { lhs = parse_range_expression(parser, lhs); } break;

        case TOKEN_LEFT_PARENTHESIS: { lhs = parse_call(parser, lhs); } break;

        default:
            parse_error(parser, parser->previous_token.loc, "Unknown binary operator.");
            advance_token(parser);
        }
    }

    return lhs;
}

INTERNAL DArray<AstExpression*> ExpressionBuilder;

INTERNAL Array<AstExpression*> parse_expression(Parser* parser) {
    ExpressionBuilder.size = 0;

    do {
        append(ExpressionBuilder, parse_single_expression(parser, PREC_ASSIGNMENT));
    } while (match(parser, TOKEN_COMMA));

    return allocate_array(ExpressionBuilder);
}


INTERNAL DArray<AstParameter>     ParameterBuilder;
INTERNAL DArray<AstTypeSpecifier> TypesBuilder;

// TODO: The double pointer is so ugly.
INTERNAL AstLambda *parse_lambda(Parser *parser, AstIdentifier **name) {
    AstLambda *lambda = ALLOC_NODE(AstLambda, AST_LAMBDA);
    lambda->location = parser->current_token.loc;

    if (!consume(parser, TOKEN_LEFT_PARENTHESIS, "Expected ( .")) return lambda;

    if (!match(parser, TOKEN_RIGHT_PARENTHESIS)) {
        ParameterBuilder.size = 0;

        do {
            AstParameter param = {};
            consume(parser, TOKEN_IDENTIFIER, "Identifier or ) expected.");
            param.ident.name = parser->previous_token.content;
            param.ident.location = parser->previous_token.loc;
            param.location = param.ident.location;

            consume(parser, TOKEN_COLON, "Missing : after function parameter name to specify its type.");
            param.type_spec = parse_type_specifier(parser);

            append(ParameterBuilder, param);
        } while (match(parser, TOKEN_COMMA));

        consume(parser, TOKEN_RIGHT_PARENTHESIS, "Missing ) in function declaration.");

        lambda->params = allocate_array(ParameterBuilder);
    }

    if (current_token_is(parser, TOKEN_IDENTIFIER)) {
        AstIdentifier *ident = parse_identifier_expr(parser);
        if (name) *name = ident;
    }

    if (match(parser, TOKEN_RIGHT_ARROW)) {
        TypesBuilder.size = 0;

        append(TypesBuilder, parse_type_specifier(parser));

        while(match(parser, TOKEN_COMMA)) {
            append(TypesBuilder, parse_type_specifier(parser));
        }

        lambda->return_types = allocate_array(TypesBuilder);
    }

    if (!consume(parser, TOKEN_LEFT_BRACE, "Missing function body.")) return lambda;

    parser->brace_count += 1;
    Token left_brace = parser->previous_token;

    while (!match(parser, TOKEN_RIGHT_BRACE)) {
        if (current_token_is(parser, TOKEN_END_OF_INPUT)) {
            parse_error(parser, left_brace.loc, "Missing } to end function body.");
            return lambda;
        }

        append(lambda->body.nodes, parse_statement(parser));
    }

    lambda->body.lambda = lambda;
    parser->brace_count -= 1;

    return lambda;
}

INTERNAL AstNode *parse_function_declaration(Parser *parser) {
    AstSymbolDeclaration *decl = ALLOC_NODE(AstSymbolDeclaration, AST_SYMBOL_DECLARATION);

    AstIdentifier *name = 0;
    decl->things = make_array<AstNode*>({parse_lambda(parser, &name)});
    if (name == 0) {
        parse_error(parser, decl->things[0]->location, "Function declaration needs a name.");
    }

    decl->symbols  = make_array({name});
    decl->location = name->location;

    return decl;
}

INTERNAL AstExpression *parse_array_declaration(Parser *parser) {
    consume(parser, TOKEN_LEFT_BRACKET, "Expected [ for array declaration.");

    AstArrayDeclaration *decl = ALLOC_NODE(AstArrayDeclaration, AST_ARRAY_DECLARATION);
    decl->location = parser->previous_token.loc;

    if (match(parser, TOKEN_RIGHT_BRACKET)) return decl;

    do {
        if (current_token_is(parser, TOKEN_RIGHT_BRACE)) break;

        decl->elements = parse_expression(parser);
    } while (match(parser, TOKEN_COMMA));

    parser->brace_count -= 1;
    consume(parser, TOKEN_RIGHT_BRACKET, "Missing ] in array declaration.");

    return decl;
}

INTERNAL DArray<String> NameBuilder;
INTERNAL DArray<Field>  StructBuilder;

// TODO: The double pointer is so ugly.
INTERNAL AstNode *parse_struct(Parser *parser, AstIdentifier **name = 0) {
    AstStruct *struct_ = ALLOC_NODE(AstStruct, AST_STRUCT);
    consume(parser, TOKEN_KEYWORD_STRUCT, "Missing keyword struct.");
    struct_->location = parser->previous_token.loc;
    
    if (current_token_is(parser, TOKEN_IDENTIFIER)) {
        AstIdentifier *ident = parse_identifier_expr(parser);
        if (name) *name = ident;
    }

    if (!consume(parser, TOKEN_LEFT_BRACE, "Missing { in struct declaration.")) return struct_;
    parser->brace_count += 1;


    Token left_brace = parser->previous_token;
    if (match(parser, TOKEN_RIGHT_BRACE)) {
        parser->brace_count -= 1;

        return struct_;
    }

    do {
        Field field = {};

        if (!parse_identifier_expr_as_value(parser, &field.ident)) {
            parse_error(parser, struct_->location, "Struct declaration missing a name.");
        }

        if (!consume(parser, TOKEN_COLON, "Missing : in field declaration.")) return struct_;
        field.type_spec = parse_type_specifier(parser);

        if (!consume(parser, TOKEN_SEMICOLON, "Missing ; to end struct field.")) return struct_;

        append(StructBuilder, field);
    } while (!current_token_is(parser, TOKEN_RIGHT_BRACE) && !current_token_is(parser, TOKEN_END_OF_INPUT));

    struct_->fields = allocate_array(StructBuilder);

    if (current_token_is(parser, TOKEN_END_OF_INPUT)) {
        parse_error(parser, left_brace.loc, "Missing } for struct declaration.");
        return struct_;
    }

    if (consume(parser, TOKEN_RIGHT_BRACE, "Missing } to end declaration of struct.")) {
        parser->brace_count -= 1;
    }

    return struct_;
}

INTERNAL AstNode *parse_struct_declaration(Parser *parser) {
    AstSymbolDeclaration *decl = ALLOC_NODE(AstSymbolDeclaration, AST_SYMBOL_DECLARATION);

    AstIdentifier *name = 0;
    AstNode *node = parse_struct(parser, &name);
    if (name == 0) {
        parse_error(parser, node->location, "Missing name in struct declaration.");

        return decl;
    }

    decl->location = node->location;
    decl->symbols  = make_array({name});
    decl->things   = make_array({node});

    return decl;
}

INTERNAL AstNode *parse_return(Parser *parser) {
    AstReturn *ret = ALLOC_NODE(AstReturn, AST_RETURN);
    ret->location = parser->current_token.loc;

    consume(parser, TOKEN_KEYWORD_RETURN, "Expected return.");

    if (match(parser, TOKEN_SEMICOLON)) return ret;

    ret->values = parse_expression(parser);

    match(parser, TOKEN_SEMICOLON);

    return ret;
}

INTERNAL DArray<AstNode*> ThingBuilder;

INTERNAL AstNode *parse_thing(Parser *parser) {
    switch (parser->current_token.kind) {
    case TOKEN_KEYWORD_STRUCT: {
        return parse_struct(parser);
    } break;

    case TOKEN_LEFT_PARENTHESIS: {
        if (peek_token(parser, 2).kind == TOKEN_COLON ||
            peek_token(parser, 1).kind == TOKEN_RIGHT_PARENTHESIS) {
            return parse_lambda(parser);
        }
    } break;
    }

    return parse_single_expression(parser, PREC_ASSIGNMENT);

    parse_error(parser, parser->current_token.loc, "Expected statement that can be bound to a symbol.");
    advance_token(parser);

    return 0;
}

INTERNAL void fill_thing_builder(Parser *parser) {
    ThingBuilder.size = 0;

    do {
        AstNode *result = parse_thing(parser);
        if (result == 0) return;

        append(ThingBuilder, result);
    } while (match(parser, TOKEN_COMMA));
}

INTERNAL AstNode *parse_symbol_declarations(Parser *parser, Array<AstExpression*> expressions) {
    for (s64 i = 0; i < expressions.size; i += 1) {
        if (expressions[i]->kind != AST_IDENTIFIER) {
            parse_error(parser, expressions[i]->location, "Identifier expected in declaration.");

            return 0;
        }
    }

    if (!consume(parser, TOKEN_COLON, "Missing : in declaration.")) return 0;

    Token colon = parser->previous_token;
    AstSymbolDeclaration *decl = ALLOC_NODE(AstSymbolDeclaration, AST_SYMBOL_DECLARATION);
    decl->symbols = cast_array<AstIdentifier*>(expressions);

    if (current_token_is(parser, TOKEN_IDENTIFIER)) {
        TypesBuilder.size = 0;

        s64 i = 0;
        do {
            if (i >= expressions.size) {
                parse_error(parser, parser->current_token.loc, "Too many type specifiers in declaration.");

                return 0;
            }
            
            append(TypesBuilder, parse_type_specifier(parser));

            i += 1;
        } while (match(parser, TOKEN_COMMA));

        decl->types = allocate_array(TypesBuilder);
    }

    if (match(parser, TOKEN_COLON)) {
        fill_thing_builder(parser);

        if (ThingBuilder.size == 0) {
            parse_error(parser, colon.loc, "Declaration is missing bindable statement(s).");
        } else {
            decl->location = colon.loc;
            decl->variable = false;

            decl->things = allocate_array(ThingBuilder);
        }
    } else if (match(parser, TOKEN_EQUAL)) {
        Array<AstExpression*> expressions = parse_expression(parser);
        if (expressions.size == 0) {
            parse_error(parser, colon.loc, "Declaration is missing expression(s).");
        } else {
            decl->location = colon.loc;
            decl->variable = true;

            decl->things = cast_array<AstNode*>(expressions);
        }
    } else {
        parse_error(parser, parser->current_token.loc, "Expected : or = to declare a symbol.");

        return 0;
    }

    match(parser, TOKEN_SEMICOLON);

    return decl;
}

INTERNAL AstNode *parse_statement(Parser *parser) {
    switch (parser->current_token.kind) {
    case TOKEN_KEYWORD_STRUCT: {
        return parse_struct_declaration(parser);
    } break;

    case TOKEN_LEFT_PARENTHESIS: {
        if (peek_token(parser, 2).kind == TOKEN_COLON ||
            peek_token(parser, 1).kind == TOKEN_RIGHT_PARENTHESIS) {
            return parse_function_declaration(parser);
        }
    } break;

    case TOKEN_KEYWORD_RETURN: {
        return parse_return(parser);
    } break;
    }

    Array<AstExpression*> expressions = parse_expression(parser);
    if (current_token_is(parser, TOKEN_COLON)) {
        return parse_symbol_declarations(parser, expressions);
    }

    parse_error(parser, parser->current_token.loc, "Expected statement.");
    advance_token(parser);

    return 0;
}

INTERNAL void synchronize(Parser *parser) {
    DEFER(parser->error_mode = false);

    s32 current_brace_count = parser->brace_count;
    while (!current_token_is(parser, TOKEN_END_OF_INPUT)) {
        switch (parser->current_token.kind) {
        case TOKEN_LEFT_BRACE:
            parser->brace_count += 1;
        break;

        case TOKEN_RIGHT_BRACE:
            parser->brace_count -= 1;

            if (parser->brace_count < current_brace_count) {
                advance_token(parser);

                return;
            }
        break;

        case TOKEN_SEMICOLON:
            if (parser->brace_count == current_brace_count) {
                advance_token(parser);

                return;
            }
        break;

        case TOKEN_END_OF_INPUT:
            return;
        break;
        }

        advance_token(parser);
    }
}

bool parse_as_knot_code(Parser *parser, Environment *env) {
    bool has_error = false;

    env->filename = parser->filename;
    parser->env = env;

    while (!current_token_is(parser, TOKEN_END_OF_INPUT)) {
        append(env->root.nodes, parse_statement(parser));
        if (parser->error_mode) {
            has_error = true;
            synchronize(parser);
        }
    }

    return !has_error;
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
        print("%S", identifier->name);
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

    case AST_RETURN: {
        print("return: ");

        AstReturn *ret = (AstReturn*)node;
        for (s64 i = 0; i < ret->values.size; i += 1) {
            print_ast_expression(ret->values[i]);
        }
        print("\n");
    } break;

    default:
        print("Can't print ast node with type %d\n", node->kind);
    }
}

//void print_ast(AbstractSyntaxTree *result) {
//	for (s32 i = 0; i < result->global_scope.nodes.size; i += 1) {
//		print_ast_node(result->global_scope.nodes[i], 0);
//	}
//}

