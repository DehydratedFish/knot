#include "type_check.h"
#include "definitions.h"
#include "knot.h"
#include "io.h"
#include "syntax.h"
#include "string2.h"
#include "array.h"



#define CHECK_TYPING(expr) { TypingResult expr_result = (expr); if (expr_result != TYPING_CORRECT) return expr_result; }

enum TypingResult {
    TYPING_ERROR,
    TYPING_UNDECLARED_IDENTIFIER,
    TYPING_CORRECT,
};

INTERNAL TypingResult infer(Environment *env, SyntaxElement *elem);
INTERNAL TypingResult check(Environment *env, SyntaxElement *elem, Type *expected);

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

INTERNAL b32 is_same_type(Type *fst, Type *snd) {
    if (fst->name == snd->name &&
        fst->kind == snd->kind &&
        fst->pointer_depth == snd->pointer_depth) {
        return true;
    }

    return false;
}


INTERNAL b32 declare(SyntaxScope *scope, IdentifierKind kind, String name, SyntaxElement *elem) {
    assert(name != "");

    Identifier *ident = upsert(&scope->identifier_table, name);
    if (ident->kind != IDENTIFIER_UNDEFINED) return false;

    ident->name = name;
    ident->kind = kind;
    ident->type = &elem->type;
    ident->element = elem;

    return true;
}

INTERNAL b32 declare_type(SyntaxScope *scope, Type *type) {
    assert(type->name != "");

    Identifier *ident = upsert(&scope->identifier_table, type->name);
    if (ident->kind != IDENTIFIER_UNDEFINED) return false;

    ident->name = type->name;
    ident->kind = IDENTIFIER_TYPE;
    ident->type = type;

    return true;
}

INTERNAL b32 declare_variable(SyntaxScope *scope, String name, Type *type) {
    assert(name != "");

    Identifier *ident = upsert(&scope->identifier_table, name);
    if (ident->kind != IDENTIFIER_UNDEFINED) return false;

    ident->name = name;
    ident->kind = IDENTIFIER_VARIABLE;
    ident->type = type;

    return true;
}

// TODO: I think overloads have to be added to the front. Else they get resolved
//       the wrong way?
INTERNAL void collect_overloads(SyntaxScope *scope, String name, List<Type*> *list) {
    SyntaxScope *next = scope->parent;
    while (next) {
        Identifier *ident = upsert(&scope->identifier_table, name);
        if (ident->kind == IDENTIFIER_LAMBDA) {
            append(list, (Array<Type*>)ident->lambda_set);
        }

        next = next->parent;
    }
}

INTERNAL b32 is_same_overload(Type *fst, Type *snd) {
    assert(fst->kind == TYPE_LAMBDA && snd->kind == TYPE_LAMBDA);
    if ((fst->flags & TYPE_FLAG_BUILTIN) != (snd->flags & TYPE_FLAG_BUILTIN)) return false;

    auto *fst_params = &fst->as.lambda.decl->params;
    auto *snd_params = &snd->as.lambda.decl->params;

    if (fst_params->size != snd_params->size) return false;
    if (fst_params->size == 0) return true;

    for (s64 i = 0; i < fst_params->size; i += 1) {
        if (!is_same_type(&(*fst_params)[i].type, &(*snd_params)[i].type)) return false;
    }

    return true;
}

INTERNAL b32 overload_found(Identifier *ident, Type *type) {
    assert(type->kind == TYPE_LAMBDA);

    for (s64 i = 0; i < ident->lambda_set.size; i += 1) {
        Type *lambda = ident->lambda_set[i];

        if (is_same_overload(lambda, type)) return true;
    }

    return false;
}

INTERNAL b32 declare_lambda(SyntaxScope *scope, String name, Type *type) {
    assert(name != "");

    Identifier *ident = upsert(&scope->identifier_table, name);

    switch (ident->kind) {
    case IDENTIFIER_UNDEFINED:
        ident->name = name;
        ident->kind = IDENTIFIER_LAMBDA;

        collect_overloads(scope, name, &ident->lambda_set);

    case IDENTIFIER_LAMBDA:
        // TODO: Check if overload is already defined.
        if (overload_found(ident, type)) return false;
        append(&ident->lambda_set, type);
    break;

    default:
        return false;
    }

    return true;
}


INTERNAL Type BuiltinTypeS8;
INTERNAL Type BuiltinTypeS16;
INTERNAL Type BuiltinTypeS32;
INTERNAL Type BuiltinTypeS64;

INTERNAL Type BuiltinTypeU8;
INTERNAL Type BuiltinTypeU16;
INTERNAL Type BuiltinTypeU32;
INTERNAL Type BuiltinTypeU64;

INTERNAL Type BuiltinTypeInt;

INTERNAL Type BuiltinTypeBool;

INTERNAL Type BuiltinTypeString;

INTERNAL Type BuiltinAdd;
INTERNAL SyntaxLambda BuiltinAddInfo;
INTERNAL Type BuiltinSub;
INTERNAL SyntaxLambda BuiltinSubInfo;
INTERNAL Type BuiltinMul;
INTERNAL SyntaxLambda BuiltinMulInfo;
INTERNAL Type BuiltinDiv;
INTERNAL SyntaxLambda BuiltinDivInfo;


INTERNAL Type make_builtin_integer_type(String name, IntegerType int_type) {
    Type type = {};
    type.name = name;
    type.kind = TYPE_INTEGER;
    type.as.integer = int_type;
    type.flags |= TYPE_FLAG_BUILTIN;

    return type;
}

INTERNAL Type make_builtin_bool_type(String name) {
    Type type = {};
    type.name = name;
    type.kind = TYPE_BOOL;
    type.flags |= TYPE_FLAG_BUILTIN;

    return type;
}

INTERNAL Type make_builtin_string_type(String name) {
    Type type = {};
    type.name = name;
    type.kind = TYPE_STRING;
    type.flags |= TYPE_FLAG_BUILTIN;

    return type;
}

INTERNAL Type make_builtin_lambda(String name) {
    Type type = {};
    type.name = name;
    type.kind = TYPE_LAMBDA;
    type.flags |= TYPE_FLAG_BUILTIN;

    return type;
};

/*
INTERNAL void fill_builtin_operator(BuiltinLambdaInfo *info, Type *type) {
    // TODO: These should be static allocations.
    info->params    = array_allocate<Type>(2);
    info->params[0] = *type;
    info->params[1] = *type;
    info->returns    = array_allocate<Type>(1);
    info->returns[0] = *type;
}
*/

INTERNAL void declare_builtins(SyntaxScope *scope) {
    BuiltinTypeU8  = make_builtin_integer_type("u8",  {false});

    BuiltinTypeS32 = make_builtin_integer_type("s32", {true});
    BuiltinTypeS64 = make_builtin_integer_type("s64", {true});

    // TODO: Type int will be platform specific.
    BuiltinTypeInt = make_builtin_integer_type("int", {true});

    BuiltinTypeBool = make_builtin_bool_type("bool");

    BuiltinTypeString = make_builtin_string_type("string");

    /*
    fill_builtin_operator(&BuiltinAddInfo, &BuiltinTypeS32);
    fill_builtin_operator(&BuiltinSubInfo, &BuiltinTypeS32);
    fill_builtin_operator(&BuiltinMulInfo, &BuiltinTypeS32);
    fill_builtin_operator(&BuiltinDivInfo, &BuiltinTypeS32);
    */

    BuiltinAdd = make_builtin_lambda("+");
    BuiltinSub = make_builtin_lambda("-");
    BuiltinMul = make_builtin_lambda("*");
    BuiltinDiv = make_builtin_lambda("/");

    declare_type(scope, &BuiltinTypeU8);
    declare_type(scope, &BuiltinTypeS32);
    declare_type(scope, &BuiltinTypeS64);

    declare_type(scope, &BuiltinTypeInt);

    declare_type(scope, &BuiltinTypeBool);

    declare_type(scope, &BuiltinTypeString);

    declare_lambda(scope, "+", &BuiltinAdd);
    declare_lambda(scope, "-", &BuiltinSub);
    declare_lambda(scope, "*", &BuiltinMul);
    declare_lambda(scope, "/", &BuiltinDiv);
}


Identifier *resolve_identifier(SyntaxScope *scope, String name) {
    SyntaxScope *search = scope;

    Identifier *identifier = upsert(&search->identifier_table, name);
    while (identifier->kind == IDENTIFIER_UNDEFINED && search->parent) {
        search = search->parent;
        identifier = upsert(&search->identifier_table, name);
    }

    return identifier;
}

INTERNAL TypingResult infer_identifier(Environment *env, SyntaxIdentifier *ident) {
    Identifier *identifier = resolve_identifier(env->current_scope, ident->name);

    switch (identifier->kind) {
    case IDENTIFIER_TYPE:
    case IDENTIFIER_VARIABLE: {
        ident->type = *identifier->type;
    } break;

    case IDENTIFIER_SYNTAX: {
        ident->type = identifier->element->type;
    } break;

    case IDENTIFIER_LAMBDA: {
        ident->type.kind = TYPE_UNRESOLVED_LAMBDA;
    } break;

    case IDENTIFIER_UNDEFINED: {
        report_error(env, ident->loc, "Undeclared identifier.");
        return TYPING_ERROR;
    } break;
    }

    return TYPING_CORRECT;
}

INTERNAL b32 smaller_or_equal(String value, String number) {
    if (value.size < number.size) {
        return true;
    } else if (value.size == number.size) {
        for (s64 i = 0; i < value.size; i += 1) {
            if (value[i] > number[i]) return false;
        }

        return true;
    }

    return false;
}

INTERNAL TypingResult infer_integer_literal(Environment *env, SyntaxIntegerLiteral *literal) {
    // TODO: Negative values.
    if (smaller_or_equal(literal->value, "2147483648")) {
        literal->type = BuiltinTypeS32;
    } else if (smaller_or_equal(literal->value, "9223372036854775807")) {
        literal->type = BuiltinTypeS64;
    } else {
        literal->type = BuiltinTypeU64;
    }

    literal->type.flags |= TYPE_FLAG_CONSTANT;

    return TYPING_CORRECT;
}

INTERNAL TypingResult infer_binary_operator(Environment *env, SyntaxBinaryOperator *op) {
    TypingResult result;

    result = infer(env, op->lhs);
    if (result != TYPING_CORRECT) return result;
    result = infer(env, op->rhs);
    if (result != TYPING_CORRECT) return result;

    /*
    if (!is_same_type(&op->lhs->type, &op->rhs->type)) {
        if (!convert_type_to(&op->lhs->type, &op->rhs->type)) {
            report_diagnostic(env, DIAGNOSTIC_ERROR, op->location, format("Can't convert %S to %S.", op->lhs->type.name, op->rhs->type.name));

            return TYPING_ERROR;
        }
    } else {
    }
    */
    
    Identifier *ident = resolve_identifier(env->current_scope, op->text);
    if (ident->kind == IDENTIFIER_UNDEFINED) {
        report_error(env, op->loc, t_format("Operator %S is not implemented.", op->text));
        return TYPING_ERROR;
    }

    op->type = op->lhs->type;
    if (op->lhs->type.flags & TYPE_FLAG_CONSTANT &&
        op->rhs->type.flags & TYPE_FLAG_CONSTANT) {
        op->type.flags |= TYPE_FLAG_CONSTANT;
    }

    return TYPING_CORRECT;
}

INTERNAL SyntaxStructMember *find_member(Type *type, String name) {
    SyntaxStructMember *result = 0;

    FOR (type->as.structure.decl->members, member) {
        if (member->ident.name == name) {
            result = member;
            break;
        }
    }

    return result;
}

INTERNAL TypingResult infer_dot_operator(Environment *env, SyntaxDotOperator *dot) {
    // TODO: Incorrect for nested . operations.
    Identifier *ident = resolve_identifier(env->current_scope, dot->lhs->name);
    if (!ident) {
        report_error(env, dot->lhs->loc, t_format("Undeclared identifier %S.", dot->lhs->name));
        return TYPING_ERROR;
    }

    if (ident->kind == IDENTIFIER_VARIABLE && ident->type->kind == TYPE_STRUCT) {
        SyntaxStructMember *member = find_member(ident->type, dot->rhs->name);
        if (!member) {
            report_error(env, dot->rhs->loc, t_format("Struct %S has no member %S.", dot->lhs->name, dot->rhs->name));
            return TYPING_ERROR;
        }

        dot->type = member->type;
    } else {
        report_error(env, dot->lhs->loc, t_format("%S has no members.", dot->lhs->name));
        return TYPING_ERROR;
    }

    return TYPING_CORRECT;
}

INTERNAL s32 get_binding_count(SyntaxElement *elem) {
    s32 count = 1;

    if (elem->kind == SYNTAX_CALL) {
        die("Implement call returns.");
    }

    return count;
}

INTERNAL b32 bind_symbols(Environment *env, Array<SyntaxIdentifier*> symbols, SyntaxElement *elem) {
    if (elem->kind == SYNTAX_CALL) {
        die("Implement call returns.");
    } else {
        assert(symbols.size == 1);

        if (elem->kind == SYNTAX_STRUCT_DECL) {
            elem->type.name = symbols[0]->name;
            if (!declare_type(env->current_scope, &elem->type)) {
                report_error(env, symbols[0]->loc, t_format("Identifier %S already declared.", symbols[0]->name));
                return false;
            }
        } else if (elem->kind == SYNTAX_INTEGER_LITERAL) {
            if (!declare(env->current_scope, IDENTIFIER_SYNTAX, symbols[0]->name, elem)) {
                report_error(env, symbols[0]->loc, t_format("Identifier %S already declared.", symbols[0]->name));
                return false;
            }
        } else if (elem->kind == SYNTAX_BINARY_OPERATOR) {
            if (elem->type.flags & TYPE_FLAG_CONSTANT) {
                // TODO: Constant folding. Or leave that to the codegen?
                if (!declare(env->current_scope, IDENTIFIER_SYNTAX, symbols[0]->name, elem)) {
                    report_error(env, symbols[0]->loc, t_format("Identifier %S already declared.", symbols[0]->name));
                    return false;
                }
            } else {
                report_error(env, symbols[0]->loc, t_format("Can't bind symbol to non constant expression.", symbols[0]->name));
            }
        } else if (elem->kind == SYNTAX_LAMBDA_DECL) {
            if (!declare_lambda(env->current_scope, symbols[0]->name, &elem->type)) {
                // TODO: Detailed error if overload is redeclared.
                report_error(env, symbols[0]->loc, t_format("Identifier %S already declared.", symbols[0]->name));
                return false;
            }
        } else {
            report_error(env, elem->loc, t_format("Implement bind to %S.", enum_string(elem->kind)));
        }
    }

    return true;
}

INTERNAL TypingResult infer_symbol_declaration(Environment *env, SyntaxSymbolDeclaration *decl) {
    s64 symbol_index = 0;

    for (s64 i = 0; i < decl->elements.size; i += 1) {
        SyntaxElement *elem = decl->elements[i];

        if (symbol_index >= decl->symbols.size) {
            report_error(env, decl->loc, "Too few symbols for declaration.");
            return TYPING_ERROR;
        }

        SyntaxIdentifier *symbol = decl->symbols[symbol_index];
        if (symbol->type.kind == TYPE_SPECIFIER) {
            if (check(env, elem, &symbol->type) == TYPING_ERROR) {
                //print("ERROR: Check in decl %S.", symbol->name);
                return TYPING_ERROR;
            }
        } else {
            if (infer(env, elem) == TYPING_ERROR) {
                //print("ERROR: infer in decl %S.", symbol->name);
                return TYPING_ERROR;
            }
        }

        s32 needed_symbols = get_binding_count(elem);
        Array<SyntaxIdentifier*> symbols = slice(decl->symbols, symbol_index, needed_symbols);

        if (!bind_symbols(env, symbols, elem)) return TYPING_ERROR;

        symbol_index += needed_symbols;
    }

    return TYPING_CORRECT;
}

INTERNAL TypingResult infer_variable_declaration(Environment *env, SyntaxVariableDeclaration *decl) {
    s64 symbol_index = 0;
    
    for (s64 i = 0; i < decl->expressions.size; i += 1) {
        SyntaxElement *elem = decl->expressions[i];

        if (elem->kind == SYNTAX_CALL) {
            die("Call not inferable.");
        } else {
            SyntaxIdentifier *var = decl->variables[symbol_index];
            if (var->type.kind == TYPE_SPECIFIER) {
                check(env, elem, &var->type);
            } else {
                infer(env, elem);
            }

            symbol_index += 1;
        }
    }

    return TYPING_CORRECT;
}

INTERNAL b32 fill_type_info(Environment *env, SyntaxElement *elem) {
    assert(elem->type.kind == TYPE_SPECIFIER);

    Identifier *ident = resolve_identifier(env->current_scope, elem->type.name);
    if (ident->kind == IDENTIFIER_TYPE) {
        elem->type = *ident->type;
        return true;
    } else {
        report_error(env, elem->loc, format("Identifier %S is not a type.", elem->type.name));
    }

    return false;
}

INTERNAL TypingResult infer_struct(Environment *env, SyntaxStruct *s) {
    Type *type = &s->type;
    type->kind = TYPE_STRUCT;

    FOR (s->members, member) {
        if (!fill_type_info(env, member)) {
            return TYPING_ERROR;
        }
        // TODO: fill offset
    }
    // TODO: calculate size
    
    type->as.structure.decl = s;
    
    return TYPING_CORRECT;
}

INTERNAL void push_scope(Environment *env, SyntaxScope *scope) {
    scope->parent = env->current_scope;
    env->current_scope = scope;
}

INTERNAL void pop_scope(Environment *env) {
    assert(env->current_scope != 0);

    env->current_scope = env->current_scope->parent;
}

INTERNAL TypingResult infer_scope(Environment *env, SyntaxScope *scope) {
    TypingResult result = TYPING_CORRECT;

    push_scope(env, scope);
    DEFER(pop_scope(env));

    FOR (scope->elements, elem) {
        TypingResult typing_result = infer(env, *elem);
        if (typing_result == TYPING_ERROR) {
            //print("error: %S\n", enum_string((*elem)->kind));
            result = TYPING_ERROR;
        }
    }

    return result;
}

INTERNAL TypingResult infer_lambda(Environment *env, SyntaxLambda *lambda) {
    SyntaxLambda *old = env->current_lambda;
    env->current_lambda = lambda;

    DEFER(env->current_lambda = old);

    lambda->type.kind = TYPE_LAMBDA;
    lambda->type.as.lambda.decl = lambda;

    FOR (lambda->params, param) {
        if (!fill_type_info(env, param)) {
            return TYPING_ERROR;
        }

        declare_variable(&lambda->scope, param->name, &param->type);
    }

    FOR (lambda->returns, type) {
        if (!fill_type_info(env, type)) {
            return TYPING_ERROR;
        }
    }

    return infer_scope(env, &lambda->scope);
}

INTERNAL TypingResult infer_return(Environment *env, SyntaxReturn *ret) {
    SyntaxLambda *lambda = env->current_lambda;
    if (lambda == 0) {
        report_error(env, ret->loc, "Return not allowed outside of a lambda.");

        return TYPING_ERROR;
    }

    if (ret->returns.size != lambda->returns.size) {
        report_error(env, ret->loc, format("Lambda has %D return values (%D supplied).", lambda->returns.size, ret->returns.size));

        return TYPING_ERROR;
    }

    for (s64 i = 0; i < ret->returns.size; i += 1) {
        TypingResult result = check(env, ret->returns[i], &lambda->returns[i].type);

        if (result != TYPING_CORRECT) return result;
    }

    return TYPING_CORRECT;
}


INTERNAL TypingResult infer(Environment *env, SyntaxElement *elem) {
    TypingResult result = TYPING_ERROR;

    switch (elem->kind) {
    case SYNTAX_KIND_NONE: { print("Node type not set.\n"); result = TYPING_ERROR; } break;

    case SYNTAX_INTEGER_LITERAL: { result = infer_integer_literal(env, (SyntaxIntegerLiteral*)elem); } break;
    case SYNTAX_SYMBOL_DECL:     { result = infer_symbol_declaration(env, (SyntaxSymbolDeclaration*)elem); } break;
    case SYNTAX_VARIABLE_DECL:   { result = infer_variable_declaration(env, (SyntaxVariableDeclaration*)elem); } break;
    case SYNTAX_IDENTIFIER:      { result = infer_identifier(env, (SyntaxIdentifier*)elem); } break;
    case SYNTAX_BINARY_OPERATOR: { result = infer_binary_operator(env, (SyntaxBinaryOperator*)elem); } break;
    case SYNTAX_DOT:             { result = infer_dot_operator(env, (SyntaxDotOperator*)elem); } break;
    case SYNTAX_STRUCT_DECL:     { result = infer_struct(env, (SyntaxStruct*)elem); } break;
    case SYNTAX_LAMBDA_DECL:     { result = infer_lambda(env, (SyntaxLambda*)elem); } break; 
    case SYNTAX_RETURN:          { result = infer_return(env, (SyntaxReturn*)elem); } break;

    default:
        report_diagnostic(env, DIAGNOSTIC_ERROR, elem->loc, format("[DEBUG] Can't infer SyntaxElement with type %S.", enum_string(elem->kind)));
    }

    return result;
}


INTERNAL b32 is_literal_convertable_to_type(SyntaxIntegerLiteral *literal, Type *expected) {
    if (expected->kind != TYPE_INTEGER) return false;
    literal->type = *expected;
    // TODO: Check if literal fits into type.

    return true;
}

INTERNAL TypingResult check_integer_literal(Environment *env, SyntaxIntegerLiteral *literal, Type *expected) {
    if (!is_literal_convertable_to_type(literal, expected)) {
        report_error(env, literal->loc, t_format("Can't convert literal to type %S.", expected->name));
        return TYPING_ERROR;
    }

    return TYPING_CORRECT;
}

INTERNAL TypingResult check_binary_operator(Environment *env, SyntaxBinaryOperator *op, Type *expected) {
    TypingResult result;

    result = infer(env, op->lhs);
    if (result != TYPING_CORRECT) return result;
    result = infer(env, op->rhs);
    if (result != TYPING_CORRECT) return result;

    /*
    if (!is_same_type(&op->lhs->type, &op->rhs->type)) {
        if (!convert_type_to(&op->lhs->type, &op->rhs->type)) {
            report_diagnostic(env, DIAGNOSTIC_ERROR, op->location, format("Can't convert %S to %S.", op->lhs->type.name, op->rhs->type.name));

            return TYPING_ERROR;
        }
    }
    */

    Identifier *ident = resolve_identifier(env->current_scope, op->text);
    if (ident->kind == IDENTIFIER_UNDEFINED) {
        report_error(env, op->loc, t_format("Operator %S is not implemented.", op->text));
        return TYPING_ERROR;
    }

    op->type = op->lhs->type;
    if (op->lhs->type.flags & TYPE_FLAG_CONSTANT &&
        op->rhs->type.flags & TYPE_FLAG_CONSTANT) {
        op->type.flags |= TYPE_FLAG_CONSTANT;
    }

    return TYPING_CORRECT;
}

INTERNAL TypingResult check_identifier(Environment *env, SyntaxIdentifier *ident, Type *expected) {
    Identifier *identifier = resolve_identifier(env->current_scope, ident->name);
    if (identifier->kind == IDENTIFIER_UNDEFINED) {
        // TODO: Add to undeclared identifiers.
        report_error(env, ident->loc, "Undeclared identifier.");
        return TYPING_ERROR;
    }

    if (identifier->kind != IDENTIFIER_VARIABLE) {
        report_error(env, ident->loc, t_format("Identifier %S is not a variable.", ident->name));
        return TYPING_ERROR;
    }

    ident->type = *identifier->type;

    return TYPING_CORRECT;
}

INTERNAL b32 lambda_fits(Environment *env, Type *type, Array<SyntaxElement*> args) {
    assert(type->kind == TYPE_LAMBDA);

    LambdaType *lambda = &type->as.lambda;
    if (lambda->decl->params.size == args.size) {
        for (s64 i = 0; i < lambda->decl->params.size; i += 1) {
            TypingResult result = check(env, args[i], &lambda->decl->params[i].type);
            if (result != TYPING_CORRECT) return false;
        }

        return true;
    }

    return false;
}

INTERNAL s64 resolve_overload(Environment *env, Array<Type*> overloads, Array<SyntaxElement*> args, Array<SyntaxElement*> returns = {}) {
    for (s64 i = 0; i < overloads.size; i += 1) {
        if (lambda_fits(env, overloads[i], args)) return i;
    }

    return -1;
}

INTERNAL Array<Type*> get_overloads(Environment *env, SyntaxElement *elem) {
    if (elem->kind == SYNTAX_IDENTIFIER) {
        SyntaxIdentifier *ident = (SyntaxIdentifier*)elem;
        
        Identifier *identifier = resolve_identifier(env->current_scope, ident->name);
        if (identifier == 0) return {};
        if (identifier->kind != IDENTIFIER_LAMBDA) return {};
        assert(identifier->lambda_set.size > 0);

        return identifier->lambda_set;
    }

    return {};
}

INTERNAL TypingResult check_call(Environment *env, SyntaxCall *call, Type *expected) {
    TypingResult result;

    result = infer(env, call->callee);
    if (result != TYPING_CORRECT) return result;

    Array<Type*> overloads = get_overloads(env, call->callee);
    s64 index = resolve_overload(env, overloads, call->args);
    if (index == -1) {
        report_error(env, call->loc, "Could not resolve lambda for call.");
        return TYPING_ERROR;
    }

    call->type = *overloads[index];

    return TYPING_CORRECT;
}

INTERNAL TypingResult check_lambda_param(Environment *env, SyntaxLambdaParameter *param, Type *expected) {
    if (is_same_type(&param->type, expected)) {
        return TYPING_CORRECT;
    }

    return TYPING_ERROR;
}

INTERNAL TypingResult check(Environment *env, SyntaxElement *elem, Type *expected) {
    TypingResult result = TYPING_ERROR;

    switch (elem->kind) {
    case SYNTAX_INTEGER_LITERAL:  result = check_integer_literal(env, (SyntaxIntegerLiteral*)elem, expected); break;
    case SYNTAX_BINARY_OPERATOR:  result = check_binary_operator(env, (SyntaxBinaryOperator*)elem, expected); break;
    case SYNTAX_IDENTIFIER:       result = check_identifier(env, (SyntaxIdentifier*)elem, expected); break;
    case SYNTAX_CALL:             result = check_call(env, (SyntaxCall*)elem, expected); break;
    case SYNTAX_LAMBDA_PARAMETER: result = check_lambda_param(env, (SyntaxLambdaParameter*)elem, expected); break;

    default:
        report_diagnostic(env, DIAGNOSTIC_ERROR, elem->loc, t_format("[DEBUG] Can't check SyntaxElement with type %S.", enum_string(elem->kind)));
    }

    return result;
}


bool type_check(Environment *env) {
    declare_builtins(&env->root);

    return infer_scope(env, &env->root);
}

