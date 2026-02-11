#include "codegen.h"

#include "knot.h"
#include "string2.h"
#include "io.h"


#include "llvm-c/Core.h"
#include "llvm-c/Analysis.h"
#include "llvm-c/TargetMachine.h"


struct CodegenState {
    Environment *env;

    LLVMContextRef context;
    LLVMModuleRef  module;
    LLVMBuilderRef builder;
};


INTERNAL LLVMTypeRef as_llvm_type(CodegenState *state, Type *type) {
    if (type->flags & TYPE_FLAG_BUILTIN) {
        if (type->name == "int") {
            return LLVMInt32TypeInContext(state->context);
        } else if (type->name == "s32") {
            return LLVMInt32TypeInContext(state->context);
        }
        print("as_llvm_type: builtin type %S not implemented.\n", type->name);
        die("");
    }

    die("as_llvm_type: only supports builtin types.");
    return 0;
}

INTERNAL void codegen(CodegenState *state, SyntaxElement *elem);

INTERNAL LLVMValueRef eval(CodegenState *state, SyntaxElement *elem) {
    switch (elem->kind) {
    case SYNTAX_IDENTIFIER: {
        SyntaxIdentifier *ident = (SyntaxIdentifier*)elem;
        if (ident->type.kind == TYPE_LAMBDA) {
            String name = ident->type.name;
            return LLVMGetNamedFunctionWithLength(state->module, (char*)name.data, name.size);
        } else if (ident->type.kind == TYPE_INTEGER) {
            if (ident->type.flags & TYPE_FLAG_CONSTANT) {
                Identifier *identifier = resolve_identifier(state->env->current_scope, ident->name);
                if (identifier->kind == IDENTIFIER_UNDEFINED) break;

                SyntaxIntegerLiteral *literal = (SyntaxIntegerLiteral*)identifier->element;
                // TODO: Proper typing.
                return LLVMConstInt(LLVMInt32TypeInContext(state->context), to_s64(literal->value), true);
            }
        }
        print("Identifier: %S\n", ident->name);
        die("eval: SYNTAX_IDENTIFIER not complete.");
    } break;

    case SYNTAX_INTEGER_LITERAL: {
        SyntaxIntegerLiteral *literal = (SyntaxIntegerLiteral*)elem;
        return LLVMConstInt(as_llvm_type(state, &literal->type), to_s64(literal->value), elem->type.as.integer.is_signed);
    } break;

    case SYNTAX_BINARY_OPERATOR: {
        SyntaxBinaryOperator *op = (SyntaxBinaryOperator*)elem;

        LLVMValueRef lhs = eval(state, op->lhs);
        LLVMValueRef rhs = eval(state, op->rhs);

        return LLVMBuildAdd(state->builder, lhs, rhs, "tmp");
    } break;

    default:
        print("Unknown SyntaxElement kind %S for eval.\n", enum_string(elem->kind));
        die("Aborting...\n");
    }

    return 0;
}

INTERNAL void codegen_scope(CodegenState *state, SyntaxScope *scope) {
    SyntaxScope *old_scope = state->env->current_scope;
    state->env->current_scope = scope;
    DEFER(state->env->current_scope = old_scope);

    for (s64 i = 0; i < scope->elements.size; i += 1) {
        codegen(state, scope->elements[i]);
    }
}

INTERNAL void codegen_symbol_decl(CodegenState *state, SyntaxSymbolDeclaration *decl) {
    for (s64 i = 0; i < decl->elements.size; i += 1) {
        codegen(state, decl->elements[i]);
    }
}

INTERNAL void codegen_lambda_decl(CodegenState *state, SyntaxLambda *decl) {
    List<LLVMTypeRef> params = {};
    for (s64 i = 0; i < decl->params.size; i += 1) {
        append(&params, as_llvm_type(state, &decl->params[i].type));
    }

    LLVMTypeRef return_type;
    if (decl->returns.size == 0) {
        return_type = LLVMVoidTypeInContext(state->context);
    } else if (decl->returns.size == 1) {
        return_type = as_llvm_type(state, &decl->returns[0].type);
    } else {
        // TODO: Create a tuple or so for multiple returns.
        die("CODEGEN: Multiple returns not possible for now.");
    }

    assert(decl->returns.size < 2);

    char *name = c_string_copy(decl->type.name, TempAllocator);

    LLVMTypeRef  lambda_type  = LLVMFunctionType(return_type, params.data, params.size, false);
    LLVMValueRef lambda_value = LLVMAddFunction(state->module, name, lambda_type);

    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(state->context, lambda_value, "entry");
    LLVMPositionBuilderAtEnd(state->builder, entry);

    codegen_scope(state, &decl->scope);
}

INTERNAL void codegen_return(CodegenState *state, SyntaxReturn *ret) {
    if (ret->returns.size == 0) {
        LLVMBuildRetVoid(state->builder);
    } else if (ret->returns.size == 1) {
        LLVMBuildRet(state->builder, eval(state, ret->returns[0]));
    } else {
        // TODO: Return multiple values as tuple?
        die("CODEGEN: Return has two or more values.");
    }
}

INTERNAL void codegen(CodegenState *state, SyntaxElement *elem) {
    switch (elem->kind) {
    case SYNTAX_SCOPE: {
        codegen_scope(state, (SyntaxScope*)elem);
    } break;

    case SYNTAX_SYMBOL_DECL: {
        codegen_symbol_decl(state, (SyntaxSymbolDeclaration*)elem);
    } break;

    case SYNTAX_LAMBDA_DECL: {
        codegen_lambda_decl(state, (SyntaxLambda*)elem);
    } break;

    case SYNTAX_RETURN: {
        codegen_return(state, (SyntaxReturn*)elem);
    } break;

    default:
        print("Unknown SyntaxElement kind %S.\n", enum_string(elem->kind));
        die("Aborting...\n");
    }
}

void codegen_llvm(Environment *env) {
    CodegenState state = {};
    state.context = LLVMContextCreate();
    state.module  = LLVMModuleCreateWithNameInContext("knot", state.context);
    state.builder = LLVMCreateBuilderInContext(state.context);
    state.env     = env;

    codegen_scope(&state, &env->root);

    char *error = 0;
    LLVMVerifyModule(state.module, LLVMAbortProcessAction, &error);
    LLVMDisposeMessage(error);
    error = 0;

#if 0
    char *ir = LLVMPrintModuleToString(state.module);
    print("LLVM IR output:\n%s\n", ir);
    LLVMDisposeMessage(ir);
#endif

    //char const *triple = "x86_64";
    char *triple = LLVMGetDefaultTargetTriple();
    char const *cpu    = "";

    LLVMInitializeAllTargets();
    LLVMInitializeAllTargetMCs();
    LLVMInitializeAllTargetInfos();
    LLVMInitializeAllAsmPrinters();

    LLVMTargetRef target;
    if (LLVMGetTargetFromTriple(triple, &target, &error)) {
        print("Can't init target: %s\n", error);
        die("");
    }
    LLVMDisposeMessage(error);
    error = 0;

    LLVMTargetMachineRef machine = LLVMCreateTargetMachine(target, triple, cpu, "", LLVMCodeGenLevelNone, LLVMRelocDefault, LLVMCodeModelDefault);
    if (LLVMTargetMachineEmitToFile(machine, state.module, "output.obj", LLVMObjectFile, &error)) {
        print("Can't write object file: %s\n", error);
        die("");
    }
    LLVMDisposeMessage(error);
    error = 0;

    LLVMDisposeMessage(error);
    LLVMDisposeBuilder(state.builder);
    LLVMDisposeModule(state.module);


    /*
    LLVMValueRef lhs = LLVMConstInt(LLVMInt32TypeInContext(context), 42, true);
    LLVMValueRef rhs = LLVMConstInt(LLVMInt32TypeInContext(context), 42, true);

    LLVMTypeRef main_type  = LLVMFunctionType(LLVMInt32TypeInContext(context), 0, 0, false);
    LLVMValueRef main_func = LLVMAddFunction(module, "main", main_type);

    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(context, main_func, "entry");
    LLVMPositionBuilderAtEnd(builder, entry);

    LLVMValueRef tmp = LLVMBuildAdd(builder, lhs, rhs, "tmp");
    LLVMBuildRet(builder, tmp);

    char *error = 0;
    LLVMVerifyModule(module, LLVMAbortProcessAction, &error);
    LLVMDisposeMessage(error);
    error = 0;


#if 0
    char *ir = LLVMPrintModuleToString(module);
    print("LLVM IR output:\n%s\n", ir);
    LLVMDisposeMessage(ir);
#endif

    char const *triple = "x86_64";
    char const *cpu    = "";

    LLVMInitializeAllTargets();
    LLVMInitializeAllTargetMCs();
    LLVMInitializeAllTargetInfos();
    LLVMInitializeAllAsmPrinters();

    LLVMTargetRef target;
    if (LLVMGetTargetFromTriple(triple, &target, &error)) {
        print("Can't init target: %s\n", error);
        die("");
    }
    LLVMDisposeMessage(error);
    error = 0;

    LLVMTargetMachineRef machine = LLVMCreateTargetMachine(target, triple, cpu, "", LLVMCodeGenLevelNone, LLVMRelocDefault, LLVMCodeModelDefault);
    if (LLVMTargetMachineEmitToFile(machine, module, "output.asm", LLVMAssemblyFile, &error)) {
        print("Can't write object file: %s\n", error);
        die("");
    }
    LLVMDisposeMessage(error);
    error = 0;

    LLVMDisposeMessage(error);
    LLVMDisposeBuilder(builder);
    LLVMDisposeModule(module);
    */
}

