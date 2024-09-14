#include "type_check.h"
#include "ast.h"
#include "definitions.h"
#include "knot.h"
#include "memory.h"


#define CHECK_TYPING(expr) { TypingResult expr_result = (expr); if (expr_result != TYPING_CORRECT) return expr_result; }

enum TypingResult {
    TYPING_ERROR,
    TYPING_UNDECLARED_IDENTIFIER,
    TYPING_CORRECT,
};

INTERNAL TypingResult infer(Environment *env, AstNode *node);
INTERNAL TypingResult check(Environment *env, AstNode *node, Type *expected);

enum {
    TYPE_ID_UNDEFINED,

    TYPE_ID_BUILTIN_S8,
    TYPE_ID_BUILTIN_U8,

    TYPE_ID_BUILTIN_S16,
    TYPE_ID_BUILTIN_U16,

    TYPE_ID_BUILTIN_S32,
    TYPE_ID_BUILTIN_U32,

    TYPE_ID_BUILTIN_S64,
    TYPE_ID_BUILTIN_U64,

    TYPE_ID_BUILTIN_R32,
    TYPE_ID_BUILTIN_R64,

    TYPE_ID_BUILTIN_COUNT
};

INTERNAL u32 CurrentID = TYPE_ID_BUILTIN_COUNT;


INTERNAL bool is_same_type(Type *lhs, Type *rhs) {
    if (lhs->decl != rhs->decl) return false;
    if (lhs->array_size != rhs->array_size) return false;
    if (lhs->pointer_depth != rhs->pointer_depth) return false;

    return true;
}

INTERNAL bool convert_type_to(Type *expected, Type *info) {
    switch (expected->kind) {
        case TYPE_INTEGER: {
            if (info->kind == TYPE_INTEGER) {
                if (expected->array_size != info->array_size) return false;
                if (expected->pointer_depth != info->pointer_depth) return false;

                if (info->as.integer.lower_bound < expected->as.integer.lower_bound) return false;
                if (info->as.integer.upper_bound > expected->as.integer.upper_bound) return false;

                *info = *expected;

                return true;
            }
        } break;
    }

    return false;
}


INTERNAL s32 calculate_integer_size(IntegerType *integer) {
    assert(integer->lower_bound < 0 || (u64)integer->lower_bound < integer->upper_bound);

    s32 bytes = 0;
    if (integer->is_signed) {
        assert(integer->upper_bound <= 9223372036854775807);

        if (integer->lower_bound >= -128 && integer->upper_bound <= 127) {
            bytes = 1;
        } else if (integer->lower_bound >= -32768 && integer->upper_bound <= 32767) {
            bytes = 2;
        } else if (integer->lower_bound >= -2147483648 && integer->upper_bound <= 2147483647) {
            bytes = 4;
        } else {
            bytes = 8;
        }
    } else {
        assert(integer->lower_bound >= 0);

        if (integer->upper_bound <= 127) {
            bytes = 1;
        } else if (integer->upper_bound <= 32767) {
            bytes = 2;
        } else if (integer->upper_bound <= 2147483647) {
            bytes = 4;
        } else {
            bytes = 8;
        }
    }

    return bytes;
}

INTERNAL Identifier *resolve_identifier(Scope *scope, String name) {
    Scope *search = scope;

    Identifier *identifier = find(&search->symbols, name);
    while (identifier->kind == IDENTIFIER_UNDEFINED && search->parent) {
        search = search->parent;
        identifier = find(&search->symbols, name);
    }

    return identifier;
}


INTERNAL Type BuiltinTypeS8;
INTERNAL Type BuiltinTypeS16;
INTERNAL Type BuiltinTypeS32;
INTERNAL Type BuiltinTypeS64;

INTERNAL Type BuiltinTypeU8;
INTERNAL Type BuiltinTypeU16;
INTERNAL Type BuiltinTypeU32;
INTERNAL Type BuiltinTypeU64;

INTERNAL Type BuiltinTypeString;

INTERNAL Type make_builtin_integer_type(String name, IntegerType int_type) {
    Type type = {};
    type.name = name;
    type.kind = TYPE_INTEGER;
    type.size_of    = calculate_integer_size(&int_type);
    type.as.integer = int_type;

    return type;
}

INTERNAL Type make_builtin_string_type() {
    Type type = {};
    type.name = "string";
    type.kind = TYPE_STRING;

    return type;
}

INTERNAL b32 declare_symbol(Environment *env, AstIdentifier *symbol, Type *type, b32 is_variable) {
    Identifier *ident = resolve_identifier(env->current_scope, symbol->name);
    if (ident->kind != IDENTIFIER_UNDEFINED) {
        report_diagnostic(env, DIAGNOSTIC_ERROR, symbol->location, format("Identifier %S already defined.", symbol->name));

        return false;
    }

    ident->name = symbol->name;
    ident->type = type;
    if (is_variable) {
        ident->kind = IDENTIFIER_VARIABLE;
    } else {
        ident->kind = IDENTIFIER_CONSTANT;
    }

    for (s64 i = 0; i < env->undeclared_identifiers.size; i += 1) {
        if (env->undeclared_identifiers[i].name == symbol->name) {
            infer(env, env->undeclared_identifiers[i].node);
            remove(env->undeclared_identifiers, i);

            i -= 1;
        }
    }

    return true;
}

INTERNAL b32 declare_symbol(Environment *env, AstIdentifier *symbol, AstNode *node, b32 is_variable) {
    Identifier *ident = resolve_identifier(env->current_scope, symbol->name);
    if (ident->kind != IDENTIFIER_UNDEFINED) {
        report_diagnostic(env, DIAGNOSTIC_ERROR, symbol->location, format("Identifier %S already defined.", symbol->name));

        return false;
    }

    ident->name = symbol->name;

    if (node->kind == AST_STRUCT) {
        assert(is_variable == false);
        ident->kind = IDENTIFIER_TYPE;
        ident->type = &node->type;
    } else {
        if (is_variable) {
            ident->kind = IDENTIFIER_VARIABLE;
            ident->type = &node->type;
        } else {
            ident->kind = IDENTIFIER_CONSTANT;
            ident->type = &node->type;
        }
    }

    for (s64 i = 0; i < env->undeclared_identifiers.size; i += 1) {
        if (env->undeclared_identifiers[i].name == symbol->name) {
            infer(env, env->undeclared_identifiers[i].node);
            remove(env->undeclared_identifiers, i);

            i -= 1;
        }
    }

    return true;
}

// TODO: Do proper parse error.
INTERNAL b32 declare_type(Environment *env, Type *type) {
    Identifier *ident = find(&env->current_scope->symbols, type->name);
    assert(ident->kind == IDENTIFIER_UNDEFINED);

    ident->name = type->name;
    ident->kind = IDENTIFIER_TYPE;
    ident->type = type;
    
    return true;
}

INTERNAL void init_builtin_types(Environment *env) {
    BuiltinTypeS8  = make_builtin_integer_type("s8" , {true, 127, -128});
    BuiltinTypeS16 = make_builtin_integer_type("s16", {true, 32767, -32768});
    BuiltinTypeS32 = make_builtin_integer_type("s32", {true, 2147483647, -2147483648});
    BuiltinTypeS64 = make_builtin_integer_type("s64", {true, 9223372036854775807, (-1 - 9223372036854775807)});

    BuiltinTypeU8  = make_builtin_integer_type("u8" , {false, 255, 0});
    BuiltinTypeU16 = make_builtin_integer_type("u16", {false, 65535, 0});
    BuiltinTypeU32 = make_builtin_integer_type("u32", {false, 4294967295, 0});
    BuiltinTypeU64 = make_builtin_integer_type("u64", {false, 18446744073709551615ULL, 0});

    BuiltinTypeString = make_builtin_string_type();

    declare_type(env, &BuiltinTypeS8);
    declare_type(env, &BuiltinTypeS16);
    declare_type(env, &BuiltinTypeS32);
    declare_type(env, &BuiltinTypeS64);

    declare_type(env, &BuiltinTypeU8);
    declare_type(env, &BuiltinTypeU16);
    declare_type(env, &BuiltinTypeU32);
    declare_type(env, &BuiltinTypeU64);

    declare_type(env, &BuiltinTypeString);
}

INTERNAL void push_scope(Environment *env, Scope *scope) {
    scope->parent = env->current_scope;
    env->current_scope = scope;
}

INTERNAL void pop_scope(Environment *env) {
    assert(env->current_scope->parent);

    env->current_scope = env->current_scope->parent;
}


INTERNAL void identifier_declared(Environment *env, String name) {
    for (s64 i = 0; i < env->undeclared_identifiers.size; i += 1) {
        UndeclaredIdentifier *it = &env->undeclared_identifiers[i];

        if (it->name == name) {
            TypingResult result = infer(env, it->node);

            remove(env->undeclared_identifiers, i);
            i -= 1;
        }
    }
}

INTERNAL TypingResult fill_type_info(Environment *env, Type *type, SourceLocation type_location) {
    TypingResult result = TYPING_ERROR;

    Identifier *ident = resolve_identifier(env->current_scope, type->name);
    if (ident->kind == IDENTIFIER_TYPE) {
        type->decl    = ident->type->decl;
        type->size_of = ident->type->size_of;
        type->kind    = ident->type->kind;
        type->as      = ident->type->as;

        result = TYPING_CORRECT;
    } else if (ident->kind == IDENTIFIER_UNDEFINED) {
        append(env->undeclared_identifiers, {type->name, type_location, env->parent_node});

        result = TYPING_UNDECLARED_IDENTIFIER;
    } else {
        report_diagnostic(env, DIAGNOSTIC_ERROR, type_location, format("Identifier %S is not a type.", type->name));
    }

    return result;
}


INTERNAL TypingResult infer_integer_literal(Environment *env, AstIntegerLiteral *literal) {
    if (literal->value < 2147483648) {
        literal->type = BuiltinTypeS32;
    } else if (literal->value < 9223372036854775807) {
        literal->type = BuiltinTypeS64;
    } else {
        literal->type = BuiltinTypeU64;
    }

    return TYPING_CORRECT;
}

INTERNAL TypingResult infer_identifier(Environment *env, AstIdentifier *ident) {
    Identifier *identifier = resolve_identifier(env->current_scope, ident->name);
    if (identifier->kind == IDENTIFIER_UNDEFINED) {
        append(env->undeclared_identifiers, {ident->name, ident->location, env->parent_node});

        return TYPING_UNDECLARED_IDENTIFIER;
    }

    if (identifier->kind == IDENTIFIER_VARIABLE) {
        ident->type = *identifier->type;
    } else {
        if (identifier->kind == IDENTIFIER_TYPE) {
            report_diagnostic(env, DIAGNOSTIC_ERROR, ident->location, format("Identifier %S is a type.", ident->name));
        } else {
            report_diagnostic(env, DIAGNOSTIC_ERROR, ident->location, format("Identifier %S is not a variable.", ident->name));
        }

        return TYPING_ERROR;
    }

    return TYPING_CORRECT;
}

INTERNAL TypingResult infer_string_literal(Environment *env, AstString *string) {
    string->type = BuiltinTypeString;

    return TYPING_CORRECT;
}

INTERNAL TypingResult infer_call(Environment *env, AstCall *call) {
    CHECK_TYPING(infer(env, call->functor));

    Type *type = &call->functor->type;

    if (type->kind != TYPE_LAMBDA) {
        report_diagnostic(env, DIAGNOSTIC_ERROR, call->functor->location, "Expression is not callable. Currently only functions are possible to call.");

        return TYPING_ERROR;
    }

    if (type->as.lambda.params.size != call->arguments.size) {
        report_diagnostic(env, DIAGNOSTIC_ERROR, call->functor->location, format("Wrong number of arguments passed to call. Expected %d but got %d.", call->functor->type.as.lambda.params.size, call->arguments.size));

        return TYPING_ERROR;
    }

    FOR (type->as.lambda.params, param) {
        s64 i = FOR_INDEX(type->as.lambda.params, param);
        TypingResult arg_result = infer(env, call->arguments[i]);
        if (arg_result != TYPING_CORRECT) return arg_result;

        if (!is_same_type(&param->type_spec.type, &call->arguments[i]->type)) {
            report_diagnostic(env, DIAGNOSTIC_ERROR, call->arguments[i]->location, format("Argument %D expected to be of type %S but is %S.", i + 1, full_type_name(&type->as.lambda.params[i].type_spec.type), full_type_name(&call->arguments[i]->type)));

            return TYPING_ERROR;
        }
    }

    if (type->as.lambda.return_types.size == 0) {
        call->type.kind = TYPE_NONE;
    } else if (type->as.lambda.return_types.size == 1) {
        call->type = type->as.lambda.return_types[0];
    } else {
        call->type.kind = TYPE_MULTI;
        call->type.as.types = type->as.lambda.return_types;
    }

    return TYPING_CORRECT;
}

INTERNAL DArray<Field> ParameterBuilder;
INTERNAL DArray<Type>  ReturnTypeBuilder;

INTERNAL TypingResult infer_lambda(Environment *env, AstLambda *lambda) {
    LambdaType type = {};

    push_scope(env, &lambda->body);
    DEFER(pop_scope(env));
    
    FOR (lambda->params, param) {
        ParameterBuilder.size = 0;

        Field field = {};
        field.ident = param->ident;
        field.offset = param->position;
        field.type_spec = param->type_spec;

        CHECK_TYPING(fill_type_info(env, &field.type_spec.type, field.type_spec.location));

        declare_symbol(env, &field.ident, &field.type_spec.type, true);

        append(ParameterBuilder, field);
    }
    type.params = allocate_array(ParameterBuilder);

    FOR (lambda->return_types, return_type) {
        ReturnTypeBuilder.size = 0;

        CHECK_TYPING(fill_type_info(env, &return_type->type, return_type->location));

        append(ReturnTypeBuilder, return_type->type);
    }
    type.return_types = allocate_array(ReturnTypeBuilder);

    lambda->type.kind = TYPE_LAMBDA;
    lambda->type.as.lambda = type;

    TypingResult result = TYPING_CORRECT;
    for (s64 i = 0; i < lambda->body.nodes.size; i += 1) {
        TypingResult typing_result = infer(env, lambda->body.nodes[i]);
        if (typing_result == TYPING_ERROR) {
            result = TYPING_ERROR;
        }
    }

    return result;
}

INTERNAL AstIdentifier *next_symbol(Array<AstIdentifier*> symbols, s64 *index) {
    assert(index);

    AstIdentifier *result = 0;
    if (*index < symbols.size) {
        result = symbols[*index];
        *index += 1;
    }
    
    return result;
}

INTERNAL TypingResult infer_symbol_declaration(Environment *env, AstSymbolDeclaration *decl) {
    s64 symbol_index = 0;
    
    for (s64 i = 0; i < decl->things.size; i += 1) {
        AstNode *thing = decl->things[i];

        if (i < decl->types.size) {
            CHECK_TYPING(check(env, thing, &decl->types[i].type));
        } else {
            CHECK_TYPING(infer(env, thing));
        }

        if (thing->kind == AST_CALL) {
            FOR (thing->type.as.lambda.return_types, type) {
                AstIdentifier *symbol = next_symbol(decl->symbols, &symbol_index);
                if (symbol == 0) continue;

                if (!declare_symbol(env, symbol, type, decl->variable)) return TYPING_ERROR;
            }
        } else {
            AstIdentifier *symbol = next_symbol(decl->symbols, &symbol_index);
            if (symbol == 0) continue;

            if (!declare_symbol(env, symbol, thing, decl->variable)) return TYPING_ERROR;
        }
    }

    if (symbol_index != decl->symbols.size) {
        report_diagnostic(env, DIAGNOSTIC_ERROR, decl->symbols[symbol_index]->location, "Too many identifiers in declaration.");

        return TYPING_ERROR;
    }

    return TYPING_CORRECT;
}

INTERNAL TypingResult infer_return_statement(Environment *env, AstReturn *ret) {
    AstLambda *lambda = env->current_scope->lambda;

    if (lambda == 0) {
        report_diagnostic(env, DIAGNOSTIC_ERROR, ret->location, "Return not allowed outside of a lambda.");

        return TYPING_ERROR;
    }

    if (ret->values.size != lambda->return_types.size) {
        report_diagnostic(env, DIAGNOSTIC_ERROR, ret->location, format("Lambda expects %d return values (%d supplied).", lambda->return_types.size, ret->values.size));

        return TYPING_ERROR;
    }

    for (s64 i = 0; i < ret->values.size; i += 1) {
        TypingResult result = check(env, ret->values[i], &lambda->return_types[i].type);

        if (result != TYPING_CORRECT) return result;
    }

    return TYPING_CORRECT;
}

INTERNAL TypingResult infer_binary_operator(Environment *env, AstBinaryOperator *op) {
    TypingResult result;

    result = infer(env, op->lhs);
    if (result != TYPING_CORRECT) return result;
    result = infer(env, op->rhs);
    if (result != TYPING_CORRECT) return result;

    if (!is_same_type(&op->lhs->type, &op->rhs->type)) {
        if (!convert_type_to(&op->lhs->type, &op->rhs->type)) {
            report_diagnostic(env, DIAGNOSTIC_ERROR, op->location, format("Can't convert %S to %S.", op->lhs->type.name, op->rhs->type.name));

            return TYPING_ERROR;
        }
    } else {
        op->type = op->lhs->type;
    }

    return TYPING_CORRECT;
}

INTERNAL TypingResult infer_assignment(Environment *env, AstAssignment *assign) {
    TypingResult result;

    result = infer(env, assign->value);
    if (result != TYPING_CORRECT) return result;

    result = check(env, assign->thing, &assign->value->type);
    if (result != TYPING_CORRECT) return result;

    return TYPING_CORRECT;
}

INTERNAL TypingResult infer_dereference(Environment *env, AstDereference *deref) {
    TypingResult result = infer(env, deref->expr);
    if (result != TYPING_CORRECT) return result;

    if (deref->expr->type.pointer_depth == 0) {
        report_diagnostic(env, DIAGNOSTIC_ERROR, deref->location, format("Can't dereference expression of type %S.", full_type_name(&deref->expr->type)));

        return TYPING_ERROR;
    }

    deref->type = deref->expr->type;
    deref->type.pointer_depth -= 1;

    return TYPING_CORRECT;
}

INTERNAL TypingResult infer_struct(Environment *env, AstStruct *struct_) {
    Type *type = &struct_->type;
    type->kind = TYPE_STRUCT;

    FOR (struct_->fields, field) {
        CHECK_TYPING(fill_type_info(env, &field->type_spec.type, field->type_spec.location));
        // TODO: fill offset
    }
    // TODO: calculate size

    type->kind = TYPE_STRUCT;
    type->decl = struct_;
    type->as.struct_.fields = struct_->fields;

    return TYPING_CORRECT;
}

INTERNAL TypingResult infer(Environment *env, AstNode *node) {
    TypingResult result = {};

    AstNode *last = env->parent_node;
    env->parent_node = node;

    switch (node->kind) {
        case AST_ERROR: { print("Node type not set.\n"); result = TYPING_ERROR; } break;

        case AST_INTEGER_LITERAL:    { result = infer_integer_literal(env, (AstIntegerLiteral*)node); } break;
        case AST_SYMBOL_DECLARATION: { result = infer_symbol_declaration(env, (AstSymbolDeclaration*)node); } break;
        case AST_IDENTIFIER:         { result = infer_identifier(env, (AstIdentifier*)node); } break;
        case AST_STRING:             { result = infer_string_literal(env, (AstString*)node); } break;
        case AST_CALL:               { result = infer_call(env, (AstCall*)node); } break;
        case AST_ASSIGNMENT:         { result = infer_assignment(env, (AstAssignment*)node); } break;
        case AST_RETURN:             { result = infer_return_statement(env, (AstReturn*)node); } break;
        case AST_BINARY_OPERATOR:    { result = infer_binary_operator(env, (AstBinaryOperator*)node); } break;
        case AST_DEREFERENCE:        { result = infer_dereference(env, (AstDereference*)node); } break;
        case AST_STRUCT:             { result = infer_struct(env, (AstStruct*)node); } break;
        case AST_LAMBDA:             { result = infer_lambda(env, (AstLambda*)node); } break; 

        default:
            report_diagnostic(env, DIAGNOSTIC_ERROR, node->location, format("Can't infer ast node with type %S.", AST_KindNames[node->kind]));
    }

    env->parent_node = last;

    return result;
}


INTERNAL TypingResult check_integer_literal(Environment *env, AstNode *node, Type *expected) {
    AstIntegerLiteral *literal = (AstIntegerLiteral*)node;

    if (expected->kind != TYPE_INTEGER) {
        report_diagnostic(env, DIAGNOSTIC_ERROR, literal->location, format("Can't convert integer literal to type %S.\n", expected->name));

        return TYPING_ERROR;
    }

    if (expected->pointer_depth > 0) {
        String name = full_type_name(expected);
        report_diagnostic(env, DIAGNOSTIC_ERROR, literal->location, format("Can't convert integer to %S.\n", name));
    }

    if (literal->value <= expected->as.integer.upper_bound) {
        literal->type = *expected;
    } else {
        report_diagnostic(env, DIAGNOSTIC_ERROR, literal->location, "Literal does not fit into type.\n");

        return TYPING_ERROR;
    }

    return TYPING_CORRECT;
}

INTERNAL TypingResult check_identifier(Environment *env, AstIdentifier *ident, Type *expected) {
    Identifier *identifier = resolve_identifier(env->current_scope, ident->name);
    if (identifier->kind == IDENTIFIER_UNDEFINED) {
        return TYPING_UNDECLARED_IDENTIFIER;
    }

    if (identifier->kind == IDENTIFIER_TYPE) {
        report_diagnostic(env, DIAGNOSTIC_ERROR, ident->location, format("Identifier %S is a type.", ident->name));

        return TYPING_ERROR;
    }

    assert(identifier->kind == IDENTIFIER_VARIABLE);
    ident->type = *identifier->type;

    if (!is_same_type(&ident->type, expected)) {
        if (!convert_type_to(expected, &ident->type)) {
            report_diagnostic(env, DIAGNOSTIC_ERROR, ident->location, format("Can't convert %S to %S.", ident->type.name, expected->name));

            return TYPING_ERROR;
        }
    }

    return TYPING_CORRECT;
}
INTERNAL TypingResult check_binary_operator(Environment *env, AstBinaryOperator *op, Type *expected) {
    TypingResult result;

    result = check(env, op->lhs, expected);
    if (result != TYPING_CORRECT) return result;
    result = check(env, op->rhs, expected);
    if (result != TYPING_CORRECT) return result;

    if (!is_same_type(&op->lhs->type, &op->rhs->type)) {
        if (!convert_type_to(&op->lhs->type, &op->rhs->type)) {
            report_diagnostic(env, DIAGNOSTIC_ERROR, op->location, format("Can't convert %S to %S.", op->lhs->type.name, op->rhs->type.name));

            return TYPING_ERROR;
        }
    }

    return TYPING_CORRECT;
}

INTERNAL TypingResult check_variable_declaration(Environment *env, AstVariableDeclaration *decl, Type *expected) {
    if (decl->type.decl) {
        if (!is_same_type(expected, &decl->type)) {
            report_diagnostic(env, DIAGNOSTIC_ERROR, decl->location, format("Expected expression of type %S.\n", expected->name));

            return TYPING_ERROR;
        }
    } else {
        decl->type = *expected;
    }

    return TYPING_CORRECT;
}

// TODO: This is not correct. But the regular inteface doesn't work with multiple returns.
INTERNAL TypingResult check_call(Environment *env, AstCall *call) {
    return infer_call(env, call);
}

INTERNAL TypingResult check(Environment *env, AstNode *node, Type *expected) {
    TypingResult result = {};

    switch (node->kind) {
        case AST_ERROR: { print("Node type not set.\n"); result = TYPING_ERROR; } break;

        case AST_INTEGER_LITERAL:  { result = check_integer_literal(env, (AstIntegerLiteral*)node, expected); } break;
        case AST_IDENTIFIER:       { result = check_identifier(env, (AstIdentifier*)node, expected); } break;
        case AST_VARIABLE_DECLARATION: { result = check_variable_declaration(env, (AstVariableDeclaration*)node, expected); } break;
        case AST_CALL:             { result = check_call(env, (AstCall*)node); } break;
        case AST_BINARY_OPERATOR:  { result = check_binary_operator(env, (AstBinaryOperator*)node, expected); } break;

        default:
            report_diagnostic(env, DIAGNOSTIC_ERROR, node->location, t_format("Can't check ast node with type %S.", AST_KindNames[node->kind]));
    }

    return result;
}


bool type_check_ast(Environment *env) {
    init(&env->root.symbols, 8);
    env->current_scope = &env->root;

    init_builtin_types(env);

    b32 result = true;

    for (s64 i = 0; i < env->root.nodes.size; i += 1) {
        TypingResult typing_result = infer(env, env->root.nodes[i]);
        if (typing_result == TYPING_ERROR) {
            result = false;
        }
    }

    if (result == true) {
        for (s64 i = 0; i < env->undeclared_identifiers.size; i += 1) {
            UndeclaredIdentifier *ident = &env->undeclared_identifiers[i];
            report_diagnostic(env, DIAGNOSTIC_ERROR, ident->location, t_format("Undeclared identifier %S.", ident->name));

            result = false;
        }
    }

    if (env->undeclared_identifiers.size) result = false;

    return result;
}

