#include "parser.h"

#include "definitions.h"
#include "knot.h"
#include "list.h"
#include "memory.h"
#include "string2.h"

#include "syntax.h"


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
    assert(parser->loc.pos < parser->source_code.size);

    parser->loc.pos += 1;
    parser->loc.column += 1;
}

INTERNAL bool peek_char(Parser *parser, u8 *c) {
    if (parser->loc.pos == parser->source_code.size) return false;

    *c = parser->source_code.data[parser->loc.pos];

    return true;
}

INTERNAL bool get_char(Parser *parser, u8 *c) {
    if (parser->loc.pos == parser->source_code.size) return false;

    *c = parser->source_code.data[parser->loc.pos];
    advance_source(parser);

    return true;
}

INTERNAL bool match_char(Parser *parser, u8 c) {
    u8 peek;
    if (peek_char(parser, &peek) && peek == c) {
        advance_source(parser);
        return true;
    }

    return false;
}

INTERNAL bool match_char_type(Parser *parser, u32 type) {
    u8 peek;
    if (peek_char(parser, &peek) && Lookup[peek] == type) {
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
    u8 *mark = &parser->source_code[parser->loc.pos];
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

    if (token.content == "struct") {
        token.kind = TOKEN_KEYWORD_STRUCT;
    } else if (token.content == "return") {
        token.kind = TOKEN_KEYWORD_RETURN;
    }

    return token;
}

INTERNAL Token parse_number(Parser *parser) {
    u8 *mark = &parser->source_code[parser->loc.pos];
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

    u8 *mark = &parser->source_code[parser->loc.pos];
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
    while (parser->loc.pos < parser->source_code.size) {
        switch (Lookup[parser->source_code.data[parser->loc.pos]]) {
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
    token.content.data = &parser->source_code[-1];
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

    parser.loc.pos    = 0;
    parser.loc.line   = 1;
    parser.loc.column = 1;

    advance_token(&parser);

    return parser;
}

INTERNAL bool current_token_is(Parser *parser, TokenKind kind) {
    return parser->current_token.kind == kind;
}

INTERNAL bool consume(Parser *parser, TokenKind kind, String error_msg) {
    if (current_token_is(parser, kind)) {
        advance_token(parser);
        return true;
    } else {
        parse_error(parser, parser->current_token.loc, error_msg);
    }

    return false;
}

INTERNAL bool match(Parser *parser, TokenKind kind) {
    if (current_token_is(parser, kind)) {
        advance_token(parser);
        return true;
    }
    return false;
}


/*
INTERNAL Type parse_type_specifier(Parser *parser) {
    Type type = {};

    consume(parser, TOKEN_IDENTIFIER, "Expected type name.");
    type.name = parser->previous_token.content;

    if (match(parser, TOKEN_LEFT_BRACKET)) {
        type.kind = TYPE_ARRAY;
        if (match(parser, TOKEN_RIGHT_BRACKET)) {
            type.array_type.size = -1;
        } else if (match(parser, TOKEN_INTEGER)) {
            type.array_type.size = to_s64(parser->previous_token.content);

            consume(parser, TOKEN_RIGHT_BRACKET, "Expected ] in array type declaration");
        } else {
            parse_error(parser, parser->current_token.loc, "Array count needs to be an integer literal.");
        }
    }

    while (match(parser, TOKEN_AMPERSAND)) {
        type.pointer_depth += 1;
    }

    return type;
}

INTERNAL b32 parse_type_list(Parser *parser) {
    s64 index = 0;

    do {
        if (parser->current_token.kind == TOKEN_COLON ||
            parser->current_token.kind == TOKEN_EQUAL) {
            break;
        }
        
        if (index >= parser->decl_or_assign_list.size) {
            parse_error(parser, parser->current_token.loc, "Too many type specifiers.");
            return false;
        }

        SourceItem *item = &parser->env->source_items[parser->decl_or_assign_list[index]];
        item->type = parse_type_specifier(parser);

        index += 1;
    } while (match(parser, TOKEN_COMMA));

    return true;
}

INTERNAL b32 string_number_smaller_as(String number, String maximum) {
    if (number.size < maximum.size) return true;
    if (number.size > maximum.size) return false;

    for (s32 i = 0; i < number.size; i += 1) {
        if (number[i] > maximum[i]) return false;
    }

    return true;
}

INTERNAL b32 parse_number_expr(Parser *parser) {
    u32 kind = parser->current_token.kind;
    advance_token(parser);

    if (kind == TOKEN_INTEGER) {
        if (!string_number_smaller_as(parser->previous_token.content, "18446744073709551615")) {
            parse_error(parser, parser->previous_token.loc, "Integer literal is too large to fit into u64.");
            return false;
        }

        emit_integer_literal(parser->env, parser->previous_token.content, parser->previous_token.loc);

        return true;
    } else if (kind == TOKEN_FLOAT) {
        die("Currently no float literals implemented in parser.");
    } else {
        parse_error(parser, parser->current_token.loc, "Expected numerical expression.");
    }

    return false;
}

INTERNAL b32 parse_string_expr(Parser *parser) {
    consume(parser, TOKEN_STRING, "Expected string.");

    emit_string_literal(parser->env, parser->previous_token.content, parser->previous_token.loc);
    return true;
}

INTERNAL b32 parse_identifier_expr(Parser *parser) {
    if (!consume(parser, TOKEN_IDENTIFIER, "Expected identifier.")) return 0;

    emit_identifier(parser->env, parser->previous_token.content, parser->previous_token.loc);
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
//INTERNAL b32 parse_statement(Parser *parser);
//INTERNAL b32 parse_single_expression(Parser *parser, u32 precedence);

//INTERNAL Array<AstExpression*> parse_expression(Parser *parser);
//INTERNAL AstExpression *parse_array_declaration(Parser *parser);


INTERNAL b32 parse_reference(Parser *parser) {
    consume(parser, TOKEN_AMPERSAND, "Expected & .");
    emit_instruction(parser->env, TAKE_REFERENCE);
    
    return parse_single_expression(parser, PREC_UNARY);
}

INTERNAL b32 parse_dereference(Parser *parser, AstExpression *pointer) {
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

INTERNAL b32 parse_binary_operator(Parser *parser, AstExpression *lhs, u32 op) {
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
    decl->names = names;

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

INTERNAL Array<AstExpression*> parse_expression(Parser *parser) {
    ExpressionBuilder.size = 0;

    do {
        append(ExpressionBuilder, parse_single_expression(parser, PREC_ASSIGNMENT));
    } while (match(parser, TOKEN_COMMA));

    return allocate_array(ExpressionBuilder);
}


INTERNAL Instruction Map[OP_COUNT] = {
    TAKE_REFERENCE,

    MUL,
    DIV,

    ADD,
    SUB,
};

INTERNAL String MapString[OP_COUNT] = {
    "&",

    "*",
    "/",

    "+",
    "-",
};
*/


/*
INTERNAL List<OperatorInfo> OperatorStack;

INTERNAL void push_operator(Operator op, SourceLocation loc) {
    append(&OperatorStack, {op, loc});
}

*/

INTERNAL b32 parse_struct_declaration(Parser *parser) {
    consume(parser, TOKEN_KEYWORD_STRUCT, "Missing keyword struct.");

    emit_instruction(parser->env, STRUCT_DECLARATION);
    emit_source_item(parser->env, parser->previous_token.content, parser->previous_token.loc);

    Backpatch fields = backpatch<s32>(parser->env);

    s32 field_count = 0;
    DEFER(fill_backpatch(&fields, &field_count, sizeof(field_count)));

    if (!consume(parser, TOKEN_LEFT_BRACE, "Missing { in struct declaration.")) return false;
    parser->brace_count += 1;

    Token left_brace = parser->previous_token;
    if (match(parser, TOKEN_RIGHT_BRACE)) {
        // TODO: Is an empty struct an error?

        parser->brace_count -= 1;
        return true;
    }

    do {
        SourceItem field = {};

        if (!match(parser, TOKEN_IDENTIFIER)) {
            parse_error(parser, parser->current_token.loc, "Struct field missing a name.");
            return false;
        } else {
            field.name = parser->previous_token.content;
            field.loc  = parser->previous_token.loc;
        }

        if (!consume(parser, TOKEN_COLON, "Missing : in field declaration.")) return false;
        field.type = parse_type_specifier(parser);

        if (!consume(parser, TOKEN_SEMICOLON, "Missing ; to end struct field.")) return false;

        emit_source_item(parser->env, &field);
        field_count += 1;
    } while (!current_token_is(parser, TOKEN_RIGHT_BRACE) && !current_token_is(parser, TOKEN_END_OF_INPUT));

    if (current_token_is(parser, TOKEN_END_OF_INPUT)) {
        parse_error(parser, left_brace.loc, "Missing } for struct declaration.");
        return false;
    }

    if (consume(parser, TOKEN_RIGHT_BRACE, "Missing } to end declaration of struct.")) {
        parser->brace_count -= 1;
    }

    return true;
}

INTERNAL b32 parse_statement(Parser *parser);

struct OperatorInfo {
    SyntaxOperator op;
    SourceLocation loc;
};

INTERNAL void add_operator(Parser *parser, Token *token) {
    OperatorInfo info;

    switch (token->kind) {
    case TOKEN_AMPERSAND: info.op = OP_REFERENCE;
    case TOKEN_PLUS:      info.op = OP_PLUS;
    case TOKEN_MINUS:     info.op = OP_MINUS;
    case TOKEN_ASTERISK:  info.op = OP_MULTIPLY;
    case TOKEN_SLASH:     info.op = OP_DIVIDE;

    default:
        die("Token is not an operator.");
    }

    info.loc = token->loc;

    append(&parser->builder.operator_stack, info);
}

INTERNAL void add_operand(Parser *parser, SyntaxElement *elem) {
    append(&parser->builder.operand_stack, elem);
}

INTERNAL void reduce(Parser *parser, SyntaxOperator op = OP_COUNT) {
    auto *stack = &parser->builder.operator_stack;
    while (stack->size) {
        OperatorInfo last = stack->data[stack->size - 1];
        if (op < last.op) break;

        switch (last.op) {
        case OP_PLUS:
        case OP_MINUS:
        case OP_MULTIPLY:
        case OP_DIVIDE: {
            SyntaxBinaryOperator *bin = ALLOC(DefaultAllocator, SyntaxBinaryOperator, 1);
            bin->kind = SYNTAX_BINARY_OPERATOR;
            bin->loc  = last.loc;
            bin->operator_kind = last.op;

            auto *operands = &parser->builder.operand_stack;
            assert(operands->size >= 2);
            
            bin->lhs = (*operands)[-2];
            bin->rhs = (*operands)[-1];

            operands->size -= 2;

            add_operand(parser, bin);
        } break;

        case OP_REFERENCE: {
            SyntaxReference *ref = ALLOC(DefaultAllocator, SyntaxReference, 1);
            ref->kind  = SYNTAX_REFERENCE;
            ref->loc   = last.loc;

            auto *operands = &parser->builder.operand_stack;
            assert(operands->size >= 1);

            ref->thing = (*operands)[-1];

            operands->size -= 1;

            add_operand(parser, ref);
        }
        }

        stack->size -= 1;
    }
}

INTERNAL b32 parse_unary_expression(Parser *parser) {
    switch (parser->current_token.kind) {
    case TOKEN_AMPERSAND: {
        add_operator(parser, &parser->current_token);
        advance_token(parser);
    } break;

    case TOKEN_INTEGER: {
        SyntaxIntegerLiteral *literal = ALLOC(DefaultAllocator, SyntaxIntegerLiteral, 1);
        literal->kind = SYNTAX_INTEGER_LITERAL;
        literal->loc  = parser->current_token.loc;

        literal->value = parser->current_token.content;
        add_operand(parser, literal);

        parser->builder.is_binary = true;

        advance_token(parser);
    } break;

    case TOKEN_IDENTIFIER: {
        SyntaxIdentifier *ident = ALLOC(DefaultAllocator, SyntaxIdentifier, 1);
        ident->kind = SYNTAX_IDENTIFIER;
        ident->loc  = parser->current_token.loc;

        ident->name = parser->current_token.content;
        add_operand(parser, ident);

        parser->builder.is_binary = true;

        advance_token(parser);
    } break;

    default:
        return false;
    }

    return true;
}

INTERNAL b32 parse_binary_expression(Parser *parser) {
    switch (parser->current_token.kind) {
    case TOKEN_PLUS: {
        reduce(parser, OP_PLUS);
        add_operator(parser, &parser->current_token);

        parser->builder.is_binary = false;
    } break;

    case TOKEN_MINUS: {
        reduce(parser, OP_MINUS);
        add_operator(parser, &parser->current_token);

        parser->builder.is_binary = false;
    } break;

    case TOKEN_ASTERISK: {
        reduce(parser, OP_MULTIPLY);
        add_operator(parser, &parser->current_token);

        parser->builder.is_binary = false;
    } break;

    case TOKEN_SLASH: {
        reduce(parser, OP_DIVIDE);
        add_operator(parser, &parser->current_token);

        parser->builder.is_binary = false;
    } break;

    default:
        return false;
    }

    return true;
}

INTERNAL b32 parse_expression(Parser *parser) {
    if (parser->builder.is_binary) {
        parse_binary_expression(parser);
    } else {
        parse_unary_expression(parser);
    }
}

INTERNAL b32 parse_expressions(Parser *parser) {
    do {
        if (!parse_expression(parser)) return false;
    } while(match(parser, TOKEN_COMMA));

    return true;
}

INTERNAL b32 parse_function_declaration(Parser *parser) {
    emit_instruction(parser->env, FUNCTION_DECLARATION);
    emit_source_item(parser->env, "", parser->current_token.loc);

    if (!consume(parser, TOKEN_LEFT_PARENTHESIS, "Expected ( .")) return false;

    Backpatch args = backpatch<s32>(parser->env);

    s32 count = 0;
    if (!match(parser, TOKEN_RIGHT_PARENTHESIS)) {
        do {
            SourceItem param = {};
            if (!consume(parser, TOKEN_IDENTIFIER, "Identifier or ) expected.")) return false;
            param.name = parser->previous_token.content;
            param.loc  = parser->previous_token.loc;

            if (!consume(parser, TOKEN_COLON, "Missing : after function parameter name to specify its type.")) return false;
            param.type = parse_type_specifier(parser);

            emit_source_item(parser->env, &param);

            count += 1;
        } while (match(parser, TOKEN_COMMA));

        if (!consume(parser, TOKEN_RIGHT_PARENTHESIS, "Missing ) in function declaration.")) return false;
    }
    fill_backpatch(&args, &count, (sizeof(count)));
    count = 0;

    Backpatch returns = backpatch<s32>(parser->env);
    if (match(parser, TOKEN_COLON)) {
        do {
            Type type = parse_type_specifier(parser);
            //emit_type_specifier(parser->env, type);

            count += 1;
        } while(match(parser, TOKEN_COMMA));
    }

    fill_backpatch(&returns, &count, (sizeof(count)));

    if (!consume(parser, TOKEN_LEFT_BRACE, "Missing function body.")) return false;

    parser->brace_count += 1;
    Token left_brace = parser->previous_token;

    Backpatch scope = emit_scope(parser->env);
    while (!match(parser, TOKEN_RIGHT_BRACE)) {
        if (current_token_is(parser, TOKEN_END_OF_INPUT)) {
            parse_error(parser, left_brace.loc, "Missing } to end function body.");
            return false;
        }

        if (!parse_statement(parser)) break;
    }
    end_scope(parser->env, &scope);

    parser->brace_count -= 1;

    return true;
}

// NOTE: Bindings mean expressions or types here.
INTERNAL s32 parse_bindings(Parser *parser) {
    s32 count = 1;

    switch (parser->current_token.kind) {
    case TOKEN_KEYWORD_STRUCT: {
        parse_struct_declaration(parser);
    } break;

    case TOKEN_LEFT_PARENTHESIS: {
        if (peek_token(parser, 2).kind == TOKEN_COLON) {
            parse_function_declaration(parser);
        } else {
            die("Oppsie...\n");
        }
    } break;

    default:
        return parse_expression(parser);
    }

    return count;
}

INTERNAL b32 parse_return(Parser *parser) {
    if (!consume(parser, TOKEN_KEYWORD_RETURN, "Expected return keyword.")) return false;

    emit_instruction(parser->env, RETURN);
    emit_source_item(parser->env, parser->previous_token.content, parser->previous_token.loc);

    Backpatch returns = backpatch<s32>(parser->env);
    s32 count = parse_expression(parser);
    if (count < 0) return false;

    fill_backpatch(&returns, &count, sizeof(count));

    return true;
}

INTERNAL b32 parse_declaration(Parser *parser) {
    if (match(parser, TOKEN_EQUAL)) {
        emit_instruction(parser->env, DECLARE_VARIABLES);
    } else if (match(parser, TOKEN_COLON)) {
        // TODO: Check and make sure only identifiers are declared?
        emit_instruction(parser->env, DECLARE_SYMBOLS);
    } else {
        parse_error(parser, parser->current_token.loc, "Malformed declaration.");
        return false;
    }

    Backpatch things = backpatch<s32>(parser->env);

    b32 result = parse_expression(parser);
    if (!result) {
        parse_error(parser, parser->current_token.loc, "Nothing to declare.");
        return result;
    }

    s32 thing_count = parser->decl_or_assign_list.size;
    fill_backpatch(&things, &thing_count, sizeof(thing_count));

    return result;
}

INTERNAL b32 parse_statement(Parser *parser) {
    emit_instruction(parser->env, STATEMENT);
    Backpatch statement = backpatch<s32>(parser->env);

    TokenKind kind = parser->current_token.kind;
    if (kind == TOKEN_KEYWORD_RETURN) {
        return parse_return(parser);
    }

    if (!parse_expression(parser)) return false;
    if (parser->decl_or_assign_list.size == 0) {
        parse_error(parser, parser->current_token.loc, "Expected statement or expression.");
        return false;
    }

    b32 result = true;
    if (match(parser, TOKEN_COLON)) {
        // TODO: Maybe make the colon be a binary operator and process declarations inside
        //       the expression function?
        if (!parse_type_list(parser)) return false;
        result = parse_declaration(parser);
    }

    match(parser, TOKEN_SEMICOLON);
    s32 length = parser->env->instructions.size - statement.index - sizeof(s32);
    fill_backpatch(&statement, &length, sizeof(length));

    return result;
}

INTERNAL void synchronize(Parser *parser) {
    parser->error_mode = false;

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

        default:
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
        if (!parse_statement(parser)) {
            has_error = true;
            synchronize(parser);
        }
    }

    env->scopes[0].instructions = env->instructions;

    return !has_error;
}

