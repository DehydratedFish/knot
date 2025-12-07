#include "parser.h"

#include "definitions.h"
#include "io.h"
#include "knot.h"
#include "list.h"
#include "memory.h"
#include "platform.h"
#include "string2.h"

#include "syntax.h"
#include <cstdarg>



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

    token.content = sub_string(parser->source_code, parser->loc.pos - 1, 1);

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
    case '=': { 
        if (match_char(parser, '=')) {
            token.kind = TOKEN_EQUAL;
            token.content.size += 1;
            break;
        }
        token.kind = TOKEN_EQUAL_SIGN; } break;
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


struct OperatorInfo {
    SyntaxOperator op;
    SourceLocation loc;
    String text;
};

INTERNAL void add_operator(Parser *parser, Token *token) {
    OperatorInfo info;

    switch (token->kind) {
    case TOKEN_AMPERSAND: info.op = OP_REFERENCE; break;
    case TOKEN_DOT:       info.op = OP_DOT;       break;
    case TOKEN_PLUS:      info.op = OP_PLUS;      break;
    case TOKEN_MINUS:     info.op = OP_MINUS;     break;
    case TOKEN_ASTERISK:  info.op = OP_MULTIPLY;  break;
    case TOKEN_SLASH:     info.op = OP_DIVIDE;    break;

    case TOKEN_EQUAL:     info.op = OP_EQUAL;     break;

    default:
        die("Token is not an operator.");
    }

    info.loc  = token->loc;
    info.text = token->content;

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
        case OP_DIVIDE: 
        case OP_EQUAL: {
            SyntaxBinaryOperator *bin = ALLOC(DefaultAllocator, SyntaxBinaryOperator, 1);
            bin->kind = SYNTAX_BINARY_OPERATOR;
            bin->loc  = last.loc;
            bin->text = last.text;
            bin->operator_kind = last.op;

            auto *operands = &parser->builder.operand_stack;
            assert(operands->size >= 2);
            
            bin->lhs = (*operands)[-2];
            bin->rhs = (*operands)[-1];

            operands->size -= 2;

            add_operand(parser, bin);
        } break;

        case OP_DOT: {
            auto *operands = &parser->builder.operand_stack;
            assert(operands->size >= 2);

            SyntaxElement *lhs = (*operands)[-2];
            SyntaxElement *rhs = (*operands)[-1];

            if (lhs->kind != SYNTAX_IDENTIFIER) {
                report_error(parser->env, lhs->loc, "Left of . is not an identifier.");
                return;
            }
            if (rhs->kind != SYNTAX_IDENTIFIER) {
                report_error(parser->env, rhs->loc, "Right of . is not an identifier.");
                return;
            }

            SyntaxDotOperator *dot = ALLOC(DefaultAllocator, SyntaxDotOperator, 1);
            dot->kind = SYNTAX_DOT;
            dot->loc  = last.loc;

            dot->lhs = (SyntaxIdentifier*)lhs;
            dot->rhs = (SyntaxIdentifier*)rhs;

            operands->size -= 2;
            add_operand(parser, dot);
        } break;

        case OP_REFERENCE: {
            SyntaxReference *ref = ALLOC(DefaultAllocator, SyntaxReference, 1);
            ref->kind = SYNTAX_REFERENCE;
            ref->loc  = last.loc;

            auto *operands = &parser->builder.operand_stack;
            assert(operands->size >= 1);

            ref->thing = (*operands)[-1];

            operands->size -= 1;

            add_operand(parser, ref);
        } break;

        default:
            die("Unhandled reduce for operator.\n");
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
    parser->builder.is_binary = false;

    switch (parser->current_token.kind) {
    case TOKEN_PLUS: {
        reduce(parser, OP_PLUS);
        add_operator(parser, &parser->current_token);
    } break;

    case TOKEN_MINUS: {
        reduce(parser, OP_MINUS);
        add_operator(parser, &parser->current_token);
    } break;

    case TOKEN_ASTERISK: {
        reduce(parser, OP_MULTIPLY);
        add_operator(parser, &parser->current_token);
    } break;

    case TOKEN_SLASH: {
        reduce(parser, OP_DIVIDE);
        add_operator(parser, &parser->current_token);
    } break;

    case TOKEN_EQUAL: {
        reduce(parser, OP_EQUAL);
        add_operator(parser, &parser->current_token);
    } break;

    case TOKEN_DOT: {
        reduce(parser, OP_DOT);
        add_operator(parser, &parser->current_token);
    } break;

    default:
        return false;
    }

    advance_token(parser);

    return true;
}

INTERNAL void parse_expression(Parser *parser) {
    parser->builder.is_binary = false;
    parser->builder.operand_stack.size  = 0;
    parser->builder.operator_stack.size = 0;

    b32 keep_parsing = true;
    while (keep_parsing) {
        if (parser->builder.is_binary) {
            keep_parsing = parse_binary_expression(parser);
        } else {
            keep_parsing = parse_unary_expression(parser);
        }
    }

    reduce(parser);
}

INTERNAL SyntaxElement *parse(Parser *parser);
INTERNAL SyntaxElement *parse_syntax_element(Parser *parser);
INTERNAL b32 parse_syntax_list(Parser *parser) {
    parser->list_builder.size = 0;

    do {
        SyntaxElement *element = parse_syntax_element(parser);
        if (!element) {
            return false;
        } else {
            append(&parser->list_builder, element);
        }
    } while(match(parser, TOKEN_COMMA));

    return true;
}

INTERNAL SyntaxElement *parse_return(Parser *parser) {
    if (!consume(parser, TOKEN_KEYWORD_RETURN, "Expectes return keyword.")) return 0;
    SourceLocation loc = parser->previous_token.loc;

    if (!parse_syntax_list(parser)) return 0;

    // TODO: Empty returns.
    SyntaxReturn *ret = ALLOC(DefaultAllocator, SyntaxReturn, 1);
    ret->kind = SYNTAX_RETURN;
    ret->loc  = loc;
    ret->returns = create_array(parser->list_builder);

    parser->list_builder.size = 0;

    return ret;
}

INTERNAL b32 parse_scope(Parser *parser, SyntaxScope *scope) {
    scope->parent = parser->env->current_scope;
    parser->env->current_scope = scope;

    DEFER(parser->env->current_scope = scope->parent;);

    b32 is_root_scope = parser->env->current_scope->parent == 0;
    if (!is_root_scope) {
        if (!consume(parser, TOKEN_LEFT_BRACE, "Expected { to start scope.")) return false;
        parser->brace_count += 1;
    }

    while (!current_token_is(parser, TOKEN_END_OF_INPUT)) {
        while (match(parser, TOKEN_SEMICOLON));
        if (!is_root_scope && match(parser, TOKEN_RIGHT_BRACE)) {
            parser->brace_count -= 1;
            break;
        }

        SyntaxElement *elem = 0;
        // NOTE: Return statements can only be at the top level of the scope.
        if (!is_root_scope && current_token_is(parser, TOKEN_KEYWORD_RETURN)) {
            elem = parse_return(parser);
        } else {
            elem = parse(parser);
        }
        if (!elem) {
            return false;
        } else {
            append(&parser->env->current_scope->elements, elem);
        }
    }

    return true;
}

INTERNAL SyntaxElement *parse_scope(Parser *parser) {
    SyntaxScope *scope = ALLOC(DefaultAllocator, SyntaxScope, 1);
    if (!parse_scope(parser, scope)) {
        DEALLOC(DefaultAllocator, scope, 1);
        return 0;
    }

    return scope;
}

// NOTE: Because casting templates is not possible in C++ I need to resort to this thing.
INTERNAL Array<SyntaxIdentifier*> create_identifier_list(List<SyntaxElement*> list) {
    auto tmp = create_array(list);

    Array<SyntaxIdentifier*> arr = {};
    arr.data = (SyntaxIdentifier**)tmp.data;
    arr.size = tmp.size;

    return arr;
}

INTERNAL SyntaxElement *parse_declaration(Parser *parser) {
    b32 check = match(parser, TOKEN_COLON);
    assert(check);

    // TODO: Type specifiers.

    SyntaxElement *result = 0;
    if (match(parser, TOKEN_COLON)) {
        SourceLocation loc = parser->previous_token.loc;

        if (parser->list_builder.size == 0) {
            report_error(parser->env, parser->previous_token.loc, "Declaration needs identifiers to bind to.");
            return result;
        }

        auto *builder = &parser->list_builder;
        for (s64 i = 0; i < parser->list_builder.size; i += 1) {
            auto *expression = parser->list_builder[i];

            if (expression->kind != SYNTAX_IDENTIFIER) {
                report_error(parser->env, expression->loc, "Declaration needs identifier to bind.");
                return result;
            }
        }

        SyntaxSymbolDeclaration *decl = ALLOC(DefaultAllocator, SyntaxSymbolDeclaration, 1);
        decl->kind = SYNTAX_SYMBOL_DECL;
        decl->loc  = loc;

        decl->symbols = create_identifier_list(parser->list_builder);
        parser->list_builder.size = 0;

        if (!parse_syntax_list(parser)) {
            // TODO: Free array decl->symbols and decl.

            return result;
        }

        decl->elements = create_array(parser->list_builder);
        parser->list_builder.size = 0;

        result = decl;
    } else if (match(parser, TOKEN_EQUAL_SIGN)) {
        SourceLocation loc = parser->previous_token.loc;

        if (parser->list_builder.size == 0) {
            report_error(parser->env, parser->previous_token.loc, "Declaration needs identifiers to bind to.");
            return result;
        }

        // TODO: Check if all elements are assignable.
        SyntaxVariableDeclaration *decl = ALLOC(DefaultAllocator, SyntaxVariableDeclaration, 1);
        decl->kind = SYNTAX_VARIABLE_DECL;
        decl->loc  = loc;

        decl->variables = create_identifier_list(parser->list_builder);
        parser->list_builder.size = 0;

        if (!parse_syntax_list(parser)) {
            // TODO: Free array decl->symbols and decl.

            return result;
        }

        decl->expressions = create_array(parser->list_builder);
        parser->list_builder.size = 0;

        result = decl;
    } else {
        report_error(parser->env, parser->current_token.loc, "Expected declaration.");
    }

    return result;
}

INTERNAL b32 parse_type_specifier(Parser *parser, Type *type) {
    if (!consume(parser, TOKEN_IDENTIFIER, "Missing type name.")) return false;
    type->name = parser->previous_token.content;
    type->kind = TYPE_SPECIFIER;

    while (match(parser, TOKEN_AMPERSAND)) {
        type->pointer_depth += 1;
    }

    return true;
}

INTERNAL b32 parse_struct_member(Parser *parser, SyntaxStructMember *member) {
    if (!consume(parser, TOKEN_IDENTIFIER, "Expected identifier to start struct member.")) return false;

    member->ident.kind = SYNTAX_IDENTIFIER;
    member->ident.name = parser->previous_token.content;
    member->ident.loc  = parser->previous_token.loc;

    if (!consume(parser, TOKEN_COLON, "Expected type specifier for struct member.")) return false;
    member->loc = parser->current_token.loc;

    if (!parse_type_specifier(parser, &member->type)) return false;

    return true;
}

INTERNAL SyntaxElement *parse_struct_declaration(Parser *parser) {
    b32 check = match(parser, TOKEN_KEYWORD_STRUCT);
    assert(check);

    Token keyword = parser->previous_token;

    if (!consume(parser, TOKEN_LEFT_BRACE, "Missing struct body.")) return 0;
    parser->brace_count += 1;

    // TODO: Keep buffer around.
    List<SyntaxStructMember> fields = {};
    DEFER(destroy(&fields));

    do {
        SyntaxStructMember *member = append(&fields);
        member->kind = SYNTAX_STRUCT_FIELD;
        if (!parse_struct_member(parser, member)) return 0;

        while (match(parser, TOKEN_SEMICOLON));
    } while (!match(parser, TOKEN_RIGHT_BRACE));
    parser->brace_count -= 1;

    SyntaxStruct *s = ALLOC(DefaultAllocator, SyntaxStruct, 1);
    s->kind    = SYNTAX_STRUCT_DECL;
    s->loc     = keyword.loc;
    s->members = create_array(fields);

    return s;
}

INTERNAL SyntaxElement *parse_lambda_declaration(Parser *parser) {
    if (!consume(parser, TOKEN_LEFT_PARENTHESIS, "Expected lambda declaration.")) return 0;

    SourceLocation loc = parser->previous_token.loc;

    List<SyntaxLambdaParameter> param_list = {};
    DEFER(destroy(&param_list));

    List<SyntaxElement> return_list = {};
    DEFER(destroy(&return_list));

    do {
        if (current_token_is(parser, TOKEN_RIGHT_PARENTHESIS)) break;

        if (!consume(parser, TOKEN_IDENTIFIER, "Expected argument name.")) return 0;
        Token name_token = parser->previous_token;

        if (!consume(parser, TOKEN_COLON, "Expected type specifier for argument.")) return 0;

        SyntaxLambdaParameter *param = append(&param_list);
        param->kind = SYNTAX_LAMBDA_ARGUMENT;
        param->loc = parser->current_token.loc;
        param->name = name_token.content;
        param->name_loc = name_token.loc;

        if (!parse_type_specifier(parser, &param->type)) return 0;
    } while (match(parser, TOKEN_COMMA));

    if (!consume(parser, TOKEN_RIGHT_PARENTHESIS, "Missing ) in function declaration.")) return 0;

    if (match(parser, TOKEN_COLON)) {
        do {
            // NOTE: No kind set. Returns are SyntaxElement(s) instead of Type(s) because
            //       the SourceLocation is needed for type checking.
            SyntaxElement *elem = append(&return_list);
            elem->loc = parser->current_token.loc;
            if (!parse_type_specifier(parser, &elem->type)) return 0;
        } while (match(parser, TOKEN_COMMA));
    }

    SyntaxLambda *lambda = ALLOC(DefaultAllocator, SyntaxLambda, 1);
    lambda->kind = SYNTAX_LAMBDA_DECL;
    lambda->loc  = loc;

    lambda->params  = create_array(param_list);
    lambda->returns = create_array(return_list);

    if (!parse_scope(parser, &lambda->scope)) return 0;

    return lambda;
}

/*
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
*/

INTERNAL SyntaxElement *parse_syntax_element(Parser *parser) {
    if (current_token_is(parser, TOKEN_KEYWORD_STRUCT)) {
        return parse_struct_declaration(parser);
    }

    if (current_token_is(parser, TOKEN_LEFT_PARENTHESIS)) {
        if (peek_token(parser, 2).kind == TOKEN_COLON ||
            peek_token(parser, 1).kind == TOKEN_RIGHT_PARENTHESIS) {
            return parse_lambda_declaration(parser);
        }
    }

    parse_expression(parser);

    if (parser->builder.operand_stack.size != 1) {
        report_error(parser->env, parser->current_token.loc, "Expected expression.");
        return 0;
    }

    return parser->builder.operand_stack[0];
}

INTERNAL SyntaxElement *parse(Parser *parser) {
    while (parser->current_token.kind == TOKEN_SEMICOLON) {
        advance_token(parser);
    }

    if (!parse_syntax_list(parser)) {
        return 0;
    }

    if (current_token_is(parser, TOKEN_COLON)) {
        return parse_declaration(parser);
    }

    if (parser->list_builder.size == 1) {
        return parser->list_builder[0];
    }

    report_error(parser->env, parser->current_token.loc, "Unused list.");
    return 0;
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
    parser->env->current_scope = &env->root;

    while (!current_token_is(parser, TOKEN_END_OF_INPUT)) {
        SyntaxElement *elem = parse(parser);
        if (!elem) {
            has_error = true;
            synchronize(parser);
        } else {
            append(&parser->env->current_scope->elements, elem);
        }
    }

    return !has_error;
}


Environment parse_knot_file(String filename) {
    Environment env = {};
    env.filename = filename;
    
    PlatformReadResult read_result = platform_read_entire_file(filename);
    if (read_result.error != PLATFORM_READ_OK) {
        report_error(&env, {-1}, t_format("Can't open or read this file.", filename));
        return env;
    };

    env.source = read_result.content;

    Parser parser = init_parser(filename, read_result.content);
    parser.env = &env;

    // TODO: Report error?
    parse_scope(&parser, &env.root);
    /*
    while (!current_token_is(&parser, TOKEN_END_OF_INPUT)) {
        SyntaxElement *elem = parse(&parser);
        if (!elem) {
            synchronize(&parser);
        } else {
            append(&parser.env->current_scope->elements, elem);
        }
    }
    */

    return env;
}



struct PrettyPrinter {
    PlatformFile *out;
    s32 indent;
};

INTERNAL s32 const MaxIndent = 32;
INTERNAL char const *IndentBuffer = "                                ";

INTERNAL void print_syntax_element(PrettyPrinter *printer, SyntaxElement *elem);

INTERNAL void print(PrettyPrinter *printer, char const *fmt, ...) {
    assert(printer->indent < MaxIndent);

    platform_write(printer->out, IndentBuffer, printer->indent);

    va_list args;
    va_start(args, fmt);
    format(printer->out, fmt, args);
    va_end(args);
}

INTERNAL void print_element_location(PrettyPrinter *printer, SyntaxElement *elem) {
    print(printer, "[%d:%d]", elem->loc.line, elem->loc.column);
}

INTERNAL void print_syntax_identifier(PrettyPrinter *printer, SyntaxIdentifier *ident) {
    print(printer, "{Identifier: %S, ", ident->name);
    print_element_location(printer, ident);
    print(printer, "}\n");
}

INTERNAL void print_syntax_integer(PrettyPrinter *printer, SyntaxIntegerLiteral *literal) {
    print(printer, "{Integer Literal: %D, ", to_s64(literal->value));
    print_element_location(printer, literal);
    print(printer, "}\n");
}

INTERNAL void print_symbol_decl(PrettyPrinter *printer, SyntaxSymbolDeclaration *decl) {
    print(printer, "{Symbol Declaration: ");
    print_element_location(printer, decl);
    print(printer, "\n");

    printer->indent += 1;
    for (s64 i = 0; i < decl->symbols.size; i += 1) {
        print_syntax_identifier(printer, decl->symbols[i]);
    }

    printer->indent -= 1;
    print(printer, "Binding to:\n");
    printer->indent += 1;

    for (s64 i = 0; i < decl->elements.size; i += 1) {
        print_syntax_element(printer, decl->elements[i]);
    }

    printer->indent -= 1;
    print(printer, "}\n");
}

INTERNAL void print_binary_operator(PrettyPrinter *printer, SyntaxBinaryOperator *op) {
    print(printer, "{Operator %S: \n", enum_string(op->operator_kind));
    print(printer, "LHS: ");
    print_syntax_element(printer, op->lhs);
    print(printer, "RHS: ");
    print_syntax_element(printer, op->rhs);
    print(printer, "}\n");
}

INTERNAL void print_syntax_element(PrettyPrinter *printer, SyntaxElement *elem) {
    switch (elem->kind) {
    case SYNTAX_IDENTIFIER: {
        print_syntax_identifier(printer, (SyntaxIdentifier*)elem);
    } break;

    case SYNTAX_INTEGER_LITERAL: {
        print_syntax_integer(printer, (SyntaxIntegerLiteral*)elem);
    } break;

    case SYNTAX_BINARY_OPERATOR: {
        print_binary_operator(printer, (SyntaxBinaryOperator*)elem);
    } break;

    case SYNTAX_SYMBOL_DECL: {
        print_symbol_decl(printer, (SyntaxSymbolDeclaration*)elem);
    } break;

    default:
        print(printer, "[Unknown syntax element: %d]\n", elem->kind);
    }
}

void print_syntax_tree(SyntaxNode *node) {
    PrettyPrinter printer = {};
    printer.out = Console.out;

    print_syntax_element(&printer, node->tree);
}

