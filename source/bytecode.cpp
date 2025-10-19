#include "bytecode.h"

#include "io.h"
#include "string2.h"


template<class Type>
INTERNAL Array<Type> allocate_array(s64 size, Allocator alloc = DefaultAllocator) {
    Array<Type> array = {};
    array.data = ALLOC(alloc, Type, size);
    array.size = size;

    return array;
}


INTERNAL Scope *current_scope(TypingContext *typing) {
    return &typing->env->scopes[typing->scope];
}

INTERNAL Array<TypingInfo> save_typings(TypingContext *typing) {
    Array<TypingInfo> result = {};
    result.data = ALLOC(DefaultAllocator, TypingInfo, typing->typings.size);
    result.size = typing->typings.size;

    copy_memory(result.data, typing->typings.data, result.size * sizeof(TypingInfo));

    typing->typings.size = 0;

    return result;
}

INTERNAL inline Type core_type(String name, s64 size, s64 align, CoreTypeKind kind) {
    Type type = {};

    type.kind = TYPE_CORE;
    type.name = name;
    type.size = size;
    type.align = align;
    type.core_type.kind = kind;

    return type;
}


Type IntegerLiteralType = {
    TYPE_INTEGER_LITERAL,
    "<Integer Literal>",
};


Type S8Type  = core_type("s8" , 1, 1, TYPE_CORE_S8);
Type S16Type = core_type("s16", 2, 2, TYPE_CORE_S16);
Type S32Type = core_type("s32", 4, 4, TYPE_CORE_S32);
Type S64Type = core_type("s64", 8, 8, TYPE_CORE_S64);


void init(Environment *env) {
    init(&env->instructions, KILOBYTES(4));

    Scope root_scope = {};
    root_scope.parent = -1;
    append(&env->scopes, root_scope);
}


INTERNAL void emit_string(Environment *env, String str) {
    emit_value(env, str.size);
    append(&env->instructions, {str.data, str.size});
}

void emit_source_item(Environment *env, SourceItem *item) {
    s64 item_index = env->source_items.size;
    append(&env->source_items, *item);

    emit_value(env, item_index);
}

void emit_source_item(Environment *env, String name, SourceLocation loc) {
    s64 item_index = env->source_items.size;
    SourceItem *item = append(&env->source_items);
    item->name = name;
    item->loc  = loc;

    emit_value(env, item_index);
}


void emit_instruction(Environment *env, Instruction instruction) {
    append(&env->instructions, (u8)instruction);
}

void emit_identifier(Environment *env, String ident, SourceLocation loc) {
    emit_instruction(env, IDENTIFIER);

    SourceItem item = {};
    item.name = ident;
    item.loc  = loc;

    emit_source_item(env, &item);
}

void emit_integer_literal(Environment *env, String value, SourceLocation loc) {
    s64 number = to_s64(value);

    emit_instruction(env, INTEGER_LITERAL);
    emit_value(env, number);

    SourceItem item = {};
    item.name = value;
    item.loc  = loc;
    item.type.kind = TYPE_INTEGER_LITERAL;

    emit_source_item(env, &item);
}

void emit_string_literal(Environment *env, String str, SourceLocation loc) {
    emit_instruction(env, STRING_LITERAL);

    SourceItem item = {};
    item.name = str;
    item.loc  = loc;

    emit_source_item(env, &item);
}

Backpatch emit_scope(Environment *env) {
    emit_instruction(env, SCOPE);

    s32 no_scope = -1;
    emit_value(env, no_scope);
    return emit_backpatch(env, sizeof(s32));
}

void end_scope(Environment *env, Backpatch *scope) {
    s32 scope_size = env->instructions.size - scope->index - sizeof(s32);
    fill_backpatch(scope, &scope_size, sizeof(s32));
}

Backpatch emit_backpatch(Environment *env, s32 size) {
    Backpatch bp = {};
    bp.env   = env;
    bp.index = env->instructions.size;
    bp.size  = size;

    ensure_space(&env->instructions, size);
    env->instructions.size += size;

    return bp;
}

void fill_backpatch(Backpatch *bp, void *data, s32 size) {
    assert(bp->size == size);

    copy_memory(&bp->env->instructions[bp->index], data, size);
}


INTERNAL Instruction get_instruction(Scope *scope, s64 *ip) {
    Instruction result = (Instruction)scope->instructions[*ip];
    *ip += 1;

    return result;
}

template<class Type>
INTERNAL Type get(Scope *scope, s64 *ip) {
#ifdef BOUNDS_CHECKING
    if (*ip >= scope->instructions.size && *ip + sizeof(Type) > scope->instructions.size) {
        die("get<Type> instruction pointer out of bounds.");
    }
#endif

    Type result;
    copy_memory(&result, &scope->instructions[*ip], sizeof(Type));
    *ip += sizeof(Type);

    return result;
}

INTERNAL String get_string(Scope *scope, s64 *ip) {
    s64 size = get<s64>(scope, ip);

    assert(*ip < scope->instructions.size);
    String result = {&scope->instructions[*ip], size};

    *ip += size;

    return result;
}

INTERNAL SourceItem *get_source_item(Environment *env, Scope *scope, s64 *ip) {
    s64 item_index = get<s64>(scope, ip);

    return &env->source_items[item_index];
}


INTERNAL Array<u8> get_scope_instructions(Scope *scope, s64 *ip) {
    s32 length = get<s32>(scope, ip);

    Array<u8> result = {
        &scope->instructions[*ip],
        length
    };
    
    *ip += length;

    return result;
}

INTERNAL void push_scope(TypingContext *typing, s64 *ip) {
    Scope *old_scope = current_scope(typing);
    Scope  new_scope = {};
    new_scope.parent = typing->scope;

    typing->scope = typing->env->scopes.size;
    append(&typing->env->scopes, new_scope);

    copy_memory(&old_scope->instructions[*ip], &typing->scope, sizeof(s32));
    *ip += sizeof(s32);

    typing->env->scopes[typing->scope].instructions = get_scope_instructions(old_scope, ip);
}

INTERNAL void pop_scope(TypingContext *typing) {
    assert(typing->scope > -1);
    typing->scope = typing->env->scopes[typing->scope].parent;
}

INTERNAL Identifier *resolve_identifier(Environment *env, s64 scope, String name) {
    s64 original_scope = scope;

    while (scope >= 0) {
        Identifier *found = find(&env->scopes[scope].symbols, name);
        if (found) break;

        scope = env->scopes[scope].parent;
    }

    return upsert(&env->scopes[original_scope].symbols, name);
}

INTERNAL b32 resolve_type(TypingContext *typing, SourceItem *item) {
    Identifier *ident = resolve_identifier(typing->env, typing->scope, item->type.name);
    if (ident->kind == IDENT_UNDECLARED) return true;
    if (ident->kind != IDENT_TYPE)       return false;

    return true;
}


INTERNAL b32 equal(Type *lhs, Type *rhs) {
    return lhs == rhs;
}



INTERNAL bool operator==(Type &lhs, Type &rhs) {
    return equal(&lhs, &rhs);
}

INTERNAL bool operator!=(Type &lhs, Type &rhs) {
    return !(lhs == rhs);
}


struct TypeChecker {
    Environment *env;
    Scope *current_scope;
    s64 ip;
};



/*
INTERNAL InferResult infer(Environment *env, s64 *ip) {
    Scope *scope = &env->scopes[env->scope];

    Instruction instruction = get_instruction(scope, ip);
    switch (instruction) {
    case STATEMENT: {
        scope->current_statement = *ip - 1;
        s32 size = get<s32>(scope, ip);
        scope->next_statement = *ip + size;
    } break;

    case SCOPE: {
        push_scope(env, ip);
        DEFER(pop_scope(env));

        return infer_scope(env);
    } break;

    case IDENTIFIER: {
        push_identifier(scope, ip);
    } break;

    case INTEGER_LITERAL: {
        push_integer_literal(scope, ip);
    } break;

    case ADD:
    case SUB:
    case MUL:
    case DIV: {
        SourceLocation loc = get_location(scope, ip);

        Operand rhs_op = pop(scope);
        Operand lhs_op = pop(scope);

        OperandInfer lhs = infer(env, &lhs_op);
        OperandInfer rhs = infer(env, &rhs_op);

        if (lhs.infer != INFER_OK) return lhs.infer; 
        if (rhs.infer != INFER_OK) return rhs.infer; 

        Operand result = {};
        result.kind = lhs_op.kind;
        result.type = *lhs.type;

        append(&scope->stack, result);
    } break;

    case DECLARE_SYMBOL: {
        u32 flags = get<u32>(scope, ip);

        Operand thing  = pop(scope);
        Operand symbol = pop(scope);

        if (symbol.kind != OPERAND_IDENTIFIER) {
            report_error(env, symbol.loc, "Can only declare symbols.");
            return INFER_ERROR;
        }

        Identifier *ident = upsert(&env->scopes[env->scope].symbols, symbol.string);
        if (ident->kind != IDENT_UNDECLARED) {
            report_error(env, symbol.loc, t_format("Identifier %S already declared.", symbol.string));
            report_diagnostic(env, DIAGNOSTIC_NOTE, ident->loc, t_format("%S is declared here.", symbol.string));
            return INFER_ERROR;
        }

        ident->kind  = IDENT_SYMBOL;
        ident->flags = (thing.flags & DECL_CONSTANT) ? IDENT_CONSTANT : 0;
        ident->loc   = symbol.loc;
        ident->type  = thing.type;
    } break;

    case STRUCT_DECLARATION: {
        TypeDeclaration decl = {};
        decl.kind = TYPE_STRUCT;
        decl.loc = get_location(scope, ip);

        s32 length = get<s32>(scope, ip);

        s32 field_count = get<s32>(scope, ip);
        decl.struc.fields = {ALLOC(DefaultAllocator, Field, field_count), field_count};
        for (s32 i = 0; i < field_count; i += 1) {
            Field field = get_field(scope, ip);
            // TODO: Check if field names are doubled.
            
            Identifier *ident = resolve_identifier(env, field.type.name);
            if (!ident) return INFER_PAUSE;

            decl.struc.fields[i] = field;
        }

        Operand op = {};
        op.kind = OPERAND_TYPE_DECLARATION;
        op.loc  = decl.loc;
        op.type_decl = decl;

        append(&scope->stack, op);
    } break;

    default:
        print("Instruction: %d at %D\n", instruction, *ip - 1);
        die("Instruction not implemented.");
    }

    return INFER_OK;
}
*/


INTERNAL Type *get_type(TypingContext *typing, s64 index) {
    TypingInfo *info = &typing->typings[index];

    if (info->kind == OPERAND_IDENTIFIER) {
        Identifier *found = resolve_identifier(typing->env, typing->scope, info->item->name);
        if (found) {
            return found->type;
        } else {
            return 0;
        }
    }

    return 0;
}

INTERNAL TypingInfo *get_typing_info(TypingContext *typing, s64 index) {
    return &typing->typings[index];
}

/*
// TODO: Overloads can be scope specific.
INTERNAL Lambda *find_overload(Environment *env, String name, Array<Type*> args) {
    Identifier *ident = resolve_identifier(env, name);
    if (ident) {
        if (ident->kind == IDENT_OVERLOAD_SET) {
            FOR (ident->overload_set.lambdas, overload) {
                if (overload->args.size != args.size) return 0;
                
                b32 match = false;
                for (s64 i = 0; i < args.size; i += 1) {
                    if (overload->args[i].type != *args[i]) {
                        match = false;
                        break;
                    } else {
                        match = true;
                    }
                }

                if (match) return overload;
            }
        }
    }

    return 0;
}
*/

INTERNAL void pause_type_checking(TypingContext *typing, SourceItem *item) {
    PausedTypecheck paused = {};
    paused.ip    = typing->last_instruction;
    paused.scope = typing->scope;
    paused.item  = item;
    paused.saved_stack = save_typings(typing);

    append(&typing->paused, paused);

    typing->last_infer = INFER_UNDECLARED_IDENTIFIER;
}

INTERNAL Array<TypingInfo> peek_info(TypingContext *typing, s32 count) {
    s64 pos = typing->typings.size - count;

    return {&typing->typings[pos], count};
}

INTERNAL b32 is_core(Type *type) {
    return type->kind == TYPE_INTEGER_LITERAL ||
           type->kind == TYPE_CORE;
}

INTERNAL void fill_type_info(TypingContext *typing, TypingInfo *info) {
    Identifier *ident = resolve_identifier(typing->env, typing->scope, info->item->name);
    if (ident->kind != IDENT_UNDECLARED) {
        info->item->type = *ident->type;
    }

    // TODO: Prefill the ident stuff?
}

INTERNAL b32 binary_operator(TypingContext *typing) {
    Scope *scope = current_scope(typing);

    SourceItem *item = get_source_item(typing->env, scope, &typing->ip);

    s32 const lhs = 0;
    s32 const rhs = 1;
    Array<TypingInfo> typings = peek_info(typing, 2);

    if (typings[lhs].kind == OPERAND_IDENTIFIER) {
        Identifier *ident = resolve_identifier(typing->env, typing->scope, typings[lhs].item->name);
        if (ident->kind == IDENT_UNDECLARED) {
            pause_type_checking(typing, typings[lhs].item);
            return false;
        } else {
            fill_type_info(typing, &typings[lhs]);
        }
    }

    if (typings[rhs].kind == OPERAND_IDENTIFIER) {
        Identifier *ident = resolve_identifier(typing->env, typing->scope, typings[rhs].item->name);
        if (ident->kind == IDENT_UNDECLARED) {
            pause_type_checking(typing, typings[rhs].item);
            return false;
        } else {
            fill_type_info(typing, &typings[rhs]);
        }
    }

    TypingInfo result = {};
    if (is_core(&typings[0].item->type) && is_core(&typings[1].item->type)) {
        // TODO: Change to best fitting integer or integer literal if possible.
        item->type = typings[0].item->type;
        result.kind = typings[0].kind;
        result.item = item;
    } else {
        String msg = t_format("Overloaded operator %S implementation not ready.\n\0", item->name);
        die((char*)msg.data);
    }

    // TODO: Only remove the typings if typing is not paused.
    typing->typings.size -= 2;

    // TODO: Get type of overload.
    append(&typing->typings, result);

    return true;
}

INTERNAL b32 infer(TypingContext *typing) {
    Scope *scope = current_scope(typing);
    typing->last_instruction = typing->ip;

    Instruction instruction = get_instruction(scope, &typing->ip);
    switch (instruction) {
    case STATEMENT: {
        s32 size = get<s32>(scope, &typing->ip);
        typing->next_statement = typing->ip + size;

        typing->typings.size = 0;
    } break;

    case SCOPE: {
    } break;

    case IDENTIFIER: {
        SourceItem *item = get_source_item(typing->env, scope, &typing->ip);

        TypingInfo info = {};
        info.kind = OPERAND_IDENTIFIER;
        info.item = item;
        append(&typing->typings, info);
    } break;

    case INTEGER_LITERAL: {
        get<s64>(scope, &typing->ip); // NOTE: Skipping the value.
        SourceItem *item = get_source_item(typing->env, scope, &typing->ip);
        item->type = IntegerLiteralType;

        TypingInfo info = {};
        info.kind = OPERAND_INTEGER_LITERAL;
        info.item = item;
        append(&typing->typings, info);
    } break;

    case ADD:
    case SUB:
    case MUL:
    case DIV: {
        return binary_operator(typing);
    } break;

    case DECLARE_SYMBOLS: {
        Array<TypingInfo> symbols = typing->typings;
        s32 thing_count  = get<s32>(scope, &typing->ip);

        s32 symbol_index = 0;
        for (s32 i = 0; i < thing_count; i += 1) {
            if (!infer(typing)) return false;

            TypingInfo *thing  = get_typing_info(typing, -1);
            TypingInfo *symbol = &symbols[symbol_index];

            if (symbol->kind != OPERAND_IDENTIFIER) {
                report_error(typing->env, symbol->item->loc, "Symbols need to be undeclared identifiers.");
                return false;
            }

            Identifier *ident = upsert(&current_scope(typing)->symbols, symbol->item->name);
            if (ident->kind != IDENT_UNDECLARED) {
                report_error(typing->env, symbol->item->loc, t_format("Identifier %S already declared.", symbol->item->name));
                report_diagnostic(typing->env, DIAGNOSTIC_NOTE, ident->loc, t_format("%S is declared here.", symbol->item->name));
                return false;
            }

            ident->kind  = IDENT_SYMBOL;
            ident->loc   = symbol->item->loc;
            ident->type  = &thing->item->type;

            typing->typings.size -= 1;
            if (typing->typings.size != symbols.size) {
                report_error(typing->env, symbol->item->loc, "Symbol count does not match bindings in declaration.");
                return false;
            }
        }
    } break;

    case STRUCT_DECLARATION: {
        SourceItem *item = get_source_item(typing->env, scope, &typing->ip);
        s32 fields = get<s32>(scope, &typing->ip);

        item->type.kind = TYPE_STRUCT;
        item->type.struct_type.fields = allocate_array<SourceItem*>(fields);

        for (s32 i = 0; i < fields; i += 1) {
            SourceItem *field_item = get_source_item(typing->env, scope, &typing->ip);

            if (!resolve_type(typing, field_item)) {
                return false;
            }

            if (field_item->type.kind == TYPE_UNDECLARED) {
                pause_type_checking(typing, field_item);
                return true;
            }

            item->type.struct_type.fields[i] = field_item;
        }

        TypingInfo info = {};
        info.kind = OPERAND_TYPE_DECLARATION;
        info.item = item;
        append(&typing->typings, info);
    } break;

    default:
        print("Instruction: %d at %D\n", instruction, typing->ip - 1);
        die("Instruction not implemented in type checking.");
    }

    return true;
}

INTERNAL b32 infer_scope(TypingContext *typing) {
    s64 old_ip = typing->ip;
    DEFER(typing->ip = old_ip);

    typing->ip = 0;

    Scope *scope = current_scope(typing);
    while (typing->ip < scope->instructions.size) {
        if (!infer(typing)) {
            if (typing->last_infer == INFER_UNDECLARED_IDENTIFIER) {
                if (typing->scope == 0) {
                    typing->ip = typing->next_statement;
                    continue;
                } else {
                    return true;
                }
            } else {
                return false;
            }
        }
    }

    return true;
}

INTERNAL b32 continue_paused_type_check(TypingContext *typing, PausedTypecheck *paused) {
    typing->typings.size = 0;

    typing->ip = paused->ip;
    typing->scope = paused->scope;
    append(&typing->typings, paused->saved_stack);

    Scope *scope = current_scope(typing);
    while (typing->ip < scope->instructions.size) {
        if (!infer(typing)) {
            return false;
        }
    }

    // TODO: Continue all previous scopes as well.

    return true;
}

b32 type_check(Environment *env) {
    TypingContext context = {};
    context.env = env;

    if (!infer_scope(&context)) return false;



    // TODO: This is just a simple iteration. Actually the resolver must be a tree
    //       that identifies cycles and can resolve dependent undeclared identifiers
    //       in order.
    if (context.paused.size) {
        for (s64 i = 0; i < context.paused.size; i += 1) {
            PausedTypecheck *paused = &context.paused[i];
            if (continue_paused_type_check(&context, paused)) {
                DEALLOC(DefaultAllocator, paused->saved_stack.data, paused->saved_stack.size);
                remove(&context.paused, i);
                i -= 1;
            }
        }
    }

    if (context.paused.size) {
        FOR (context.paused, paused) {
            report_error(context.env, paused->item->loc, t_format("Undeclared identifier %S.", paused->item->name));
        }
    }

    /*
    if (env->paused.size) {
        b32 complete = true;

        for (s64 i = 0; i < env->paused.size; i += 1) {
            PausedTypecheck *paused = &env->paused[i];
            InferResult scope_result = resume_scope(env, paused);

            if (scope_result == INFER_ERROR) {
                complete = false;
            } else if (scope_result == INFER_PAUSE) {
                continue;
            } else {
                remove(&env->paused, i);
                i -= 1;
            }
        }

        if (env->paused.size) {
            FOR (env->paused, paused) {
                report_error(env, paused->loc, t_format("Undeclared identifier %S.", paused->undeclared_identifier));
                complete = false;
            }
        }

        env->scope = 0;

        return complete;
    }
    */

    return true;
}



/*
b32 interpret(Environment *env, Array<u8> instructions) {
    b32 result = true;

    while (instructions.size) {
        if (!evaluate(env, &instructions)) return false;
    }

    return result;
}

b32 evaluate(Environment *env, Array<u8> *instructions) {
    Instruction instruction = (Instruction)get<u8>(instructions);
    switch (instruction) {
    case SCOPE: {
        s32 scope = push_scope(env, instructions);
        DEFER(pop_scope(env));

        Array<u8> block_instructions = env->scopes[scope].instructions;

        // TODO: Keep interpreting and type checking.
        while (block_instructions.size) {
            if (!evaluate(env, &block_instructions)) return false;
        }
    } break;

    case IDENTIFIER: {
        Operand value = {};
        value.kind = OPERAND_IDENTIFIER;
        value.string = get_string(instructions);
        value.loc    = get_location(instructions);
        append(&env->stack, value);
    } break;

    case INTEGER_LITERAL: {
        Operand value = {};
        value.kind = OPERAND_INTEGER_LITERAL;
        value.flags = OPERAND_CONSTANT;
        value.integer = get<u64>(instructions);
        value.loc  = get_location(instructions);
        value.type = IntegerLiteralType;
        append(&env->stack, value);
    } break;

    case ADD: {
        SourceLocation loc = get_location(instructions);

        Operand rhs = pop(env);
        Operand lhs = pop(env);

        if (lhs.kind == OPERAND_IDENTIFIER) {
            Identifier *ident = resolve_identifier(env, lhs.string);
            if (!ident) {
                report_error(env, lhs.loc, t_format("Undeclared identifier %S.", lhs.string));
                return false;
            }
            if (ident->kind != IDENT_SYMBOL) {
                report_error(env, lhs.loc, t_format("Identifier %S is not a symbol.", lhs.string));
                return false;
            }

            lhs = ident->value;
        }
        if (rhs.kind == OPERAND_IDENTIFIER) {
            Identifier *ident = resolve_identifier(env, lhs.string);
            if (!ident) {
                report_error(env, rhs.loc, t_format("Undeclared identifier %S.", rhs.string));
                return false;
            }
            if (ident->kind != IDENT_SYMBOL) {
                report_error(env, rhs.loc, t_format("Identifier %S is not a symbol.", rhs.string));
                return false;
            }

            rhs = ident->value;
        }

        if (lhs.kind == OPERAND_INTEGER_LITERAL && rhs.kind == OPERAND_INTEGER_LITERAL) {
            Operand value = {};
            value.kind  = OPERAND_INTEGER_LITERAL;
            value.flags = OPERAND_CONSTANT;
            value.integer = lhs.integer + rhs.integer;
            append(&env->stack, value);
        } else {
            report_error(env, loc, "No build in plus operator found.");
            return false;
            Identifier *op = resolve_identifier(env, "+");
            assert(op && op->kind == IDENT_OVERLOAD_SET);

            if (lhs.type != rhs.type) {
                report_error(env, loc, "Type mismatch in addition.");
                return false;
            }
        }
    } break;

    case SUB: {
        SourceLocation loc = get_location(instructions);

        Operand rhs = pop(env);
        Operand lhs = pop(env);

        if (lhs.kind == OPERAND_INTEGER_LITERAL && rhs.kind == OPERAND_INTEGER_LITERAL) {
            Operand value = {};
            value.kind  = OPERAND_INTEGER_LITERAL;
            value.flags = OPERAND_CONSTANT;
            value.integer = lhs.integer - rhs.integer;
            append(&env->stack, value);
        } else {
            report_error(env, loc, "No build in plus operator found.");
            return false;
        }
    } break;

    case MUL: {
        SourceLocation loc = get_location(instructions);

        Operand rhs = pop(env);
        Operand lhs = pop(env);

        if (lhs.kind == OPERAND_IDENTIFIER) {
            Identifier *ident = resolve_identifier(env, lhs.string);
            if (!ident) {
                report_error(env, lhs.loc, t_format("Undeclared identifier %S.", lhs.string));
                return false;
            }
            if (ident->kind != IDENT_SYMBOL) {
                report_error(env, lhs.loc, t_format("Identifier %S is not a symbol.", lhs.string));
                return false;
            }

            lhs = ident->value;
        }
        if (rhs.kind == OPERAND_IDENTIFIER) {
            Identifier *ident = resolve_identifier(env, rhs.string);
            if (!ident) {
                report_error(env, rhs.loc, t_format("Undeclared identifier %S.", rhs.string));
                return false;
            }
            if (ident->kind != IDENT_SYMBOL) {
                report_error(env, rhs.loc, t_format("Identifier %S is not a symbol.", rhs.string));
                return false;
            }

            rhs = ident->value;
        }

        if (lhs.kind == OPERAND_INTEGER_LITERAL && rhs.kind == OPERAND_INTEGER_LITERAL) {
            Operand value = {};
            value.kind  = OPERAND_INTEGER_LITERAL;
            value.flags = OPERAND_CONSTANT;
            value.integer = lhs.integer * rhs.integer;
            append(&env->stack, value);
        } else {
            report_error(env, loc, "No build in plus operator found.");
            return false;
        }
    } break;

    case DIV: {
        SourceLocation loc = get_location(instructions);

        Operand rhs = pop(env);
        Operand lhs = pop(env);

        if (lhs.kind == OPERAND_INTEGER_LITERAL && rhs.kind == OPERAND_INTEGER_LITERAL) {
            Operand value = {};
            value.kind  = OPERAND_INTEGER_LITERAL;
            value.flags = OPERAND_CONSTANT;
            value.integer = lhs.integer / rhs.integer;
            append(&env->stack, value);
        } else {
            report_error(env, loc, "No build in plus operator found.");
            return false;
        }
    } break;

    case DECLARE_SYMBOL: {
        u32 flags = get<u32>(instructions);

        Operand thing  = pop(env);
        Operand symbol = pop(env);

        if (symbol.kind != OPERAND_IDENTIFIER) {
            report_error(env, symbol.loc, "Can only declare symbols.");
            return false;
        }

        Identifier *ident = upsert(&env->scopes[env->scope].symbols, symbol.string);
        if (ident->kind != IDENT_UNDECLARED) {
            report_error(env, symbol.loc, t_format("Identifier %S already declared.", symbol.string));
            report_diagnostic(env, DIAGNOSTIC_NOTE, ident->loc, t_format("%S is declared here.", symbol.string));
            return false;
        }

        ident->kind  = (flags & DECL_CONSTANT) ? IDENT_SYMBOL : IDENT_VARIABLE;
        ident->loc   = symbol.loc;
        ident->type  = thing.type;
        ident->value = thing;
    } break;

    case STRUCT_DECLARATION: {
        TypeDeclaration decl = {};
        decl.kind = TYPE_STRUCT;
        decl.loc = get_location(instructions);

        s32 length = get<s32>(instructions);

        s32 field_count = get<s32>(instructions);
        decl.struc.fields = {ALLOC(DefaultAllocator, Field, field_count), field_count};
        for (s32 i = 0; i < field_count; i += 1) {
            decl.struc.fields[i] = get_field(instructions);
        }

        Operand op = {};
        op.kind = OPERAND_TYPE_DECLARATION;
        op.loc  = decl.loc;
        op.type_decl = decl;

        append(&env->stack, op);
    } break;

    case FUNCTION_DECLARATION: {
        Operand func = {};
        func.kind = OPERAND_LAMBDA;
        func.loc  = get_location(instructions);

        s32 args = get<s32>(instructions);
        if (args) {
            func.lambda.args = allocate_array<Field>(args);
            for (s32 i = 0; i < args; i += 1) {
                func.lambda.args[i] = get_field(instructions);
            }
        }

        s32 returns = get<s32>(instructions);
        if (returns) {
            func.lambda.returns = allocate_array<Type>(args);
            for (s32 i = 0; i < returns; i += 1) {
                // TODO: Resolve type info.
                //func.lambda.returns[i] = get_type_info(instructions);
            }
        }

        u8 *scope_start = instructions->data;
        instruction = (Instruction)get<u8>(instructions);
        assert(instruction == SCOPE);

        s32 length = get<s32>(instructions) + sizeof(s32) + sizeof(u8);
        func.lambda.body.instructions = {scope_start, length};
        func.lambda.body.lambda = &func.lambda;
        skip(instructions, length);

        interpret(env, func.lambda.body.instructions);

        append(&env->stack, func);
    } break;

    case RETURN: {
        SourceLocation loc = get_location(instructions);

        if (env->scopes[env->scope].lambda == 0) {
            report_diagnostic(env, DIAGNOSTIC_ERROR, loc, "Return only allowed inside function.");
            return false;
        }

        s32 return_values = get<s32>(instructions);
        if (return_values != env->scopes[env->scope].lambda->returns.size) {
            report_diagnostic(env, DIAGNOSTIC_ERROR, loc, "Return value mismatch.");
            return false;
        }

        return evaluate(env, instructions);
    } break;

    default: {
        print("Instruction: %d\n", instruction);
        die("Instruction not implemented.");
    }
    }

    return true;
}
*/

