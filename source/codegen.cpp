#include "codegen.h"

#include "knot.h"
#include "string2.h"
#include "io.h"

#include "llvm-c/Core.h"
#include "llvm-c/Analysis.h"
#include "llvm-c/TargetMachine.h"


struct CodegenState {
    Environment *env;
    String declaration_name;

    LLVMContextRef context;
    LLVMModuleRef  module;
    LLVMBuilderRef builder;
};


INTERNAL LLVMTypeRef as_llvm_type(CodegenState *state, Type *type) {
    if (type->flags & TYPE_FLAG_BUILTIN) {
        if (type->name == "int") {
            return LLVMInt32TypeInContext(state->context);
        } else if (type->name == "u8") {
            return LLVMInt8TypeInContext(state->context);
        } else if (type->name == "u16") {
            return LLVMInt16TypeInContext(state->context);
        } else if (type->name == "u32") {
            return LLVMInt32TypeInContext(state->context);
        } else if (type->name == "u64") {
            return LLVMInt64TypeInContext(state->context);
        } else if (type->name == "s8") {
            return LLVMInt8TypeInContext(state->context);
        } else if (type->name == "s16") {
            return LLVMInt16TypeInContext(state->context);
        } else if (type->name == "s32") {
            return LLVMInt32TypeInContext(state->context);
        } else if (type->name == "s64") {
            return LLVMInt64TypeInContext(state->context);
        }
        print("as_llvm_type: Builtin type %S not implemented.\n", type->name);
        die("");
    }

    die("as_llvm_type: Only builtin types supported.\n");
    return 0;
}

INTERNAL LLVMValueRef codegen(CodegenState *state, SyntaxElement *elem) {
    switch (elem->kind) {
    case SYNTAX_IDENTIFIER: {
        auto  *ident = (SyntaxIdentifier*)elem;

        if (ident->type.kind == TYPE_LAMBDA) {
            String name = ident->type.name;
            return LLVMGetNamedFunctionWithLength(state->module, (char*)name.data, name.size);
        } else if (ident->type.kind == TYPE_INTEGER) {
            Identifier *identifier = resolve_identifier(state->env->current_scope, ident->name);
            if (identifier->kind == IDENTIFIER_UNDEFINED) break;

            if (ident->type.flags & TYPE_FLAG_CONSTANT) {
                SyntaxIntegerLiteral *literal = (SyntaxIntegerLiteral*)identifier->element;
                // TODO: Proper typing.
                return LLVMConstInt(LLVMInt32TypeInContext(state->context), to_s64(literal->value), true);
            } else {
                assert(identifier->backend_data != 0);

                return (LLVMValueRef)identifier->backend_data;
            }
        }
        print("Identifier: %S with kind %S\n", ident->name, enum_string(ident->type.kind));
        die("eval: SYNTAX_IDENTIFIER not complete.");
    } break;

    case SYNTAX_INTEGER_LITERAL: {
        auto literal = (SyntaxIntegerLiteral*)elem;
        return LLVMConstInt(as_llvm_type(state, &literal->type), to_s64(literal->value), elem->type.as.integer.is_signed);
    } break;

    case SYNTAX_BINARY_OPERATOR: {
        auto *op = (SyntaxBinaryOperator*)elem;

        LLVMValueRef lhs = codegen(state, op->lhs);
        LLVMValueRef rhs = codegen(state, op->rhs);

        return LLVMBuildAdd(state->builder, lhs, rhs, "tmp");
    } break;

    case SYNTAX_SCOPE: {
        auto scope = (SyntaxScope*)elem;

        SyntaxScope *old_scope = state->env->current_scope;
        state->env->current_scope = scope;
        DEFER(state->env->current_scope = old_scope);

        for (s64 i = 0; i < scope->elements.size; i += 1) {
            codegen(state, scope->elements[i]);
        }
    } break;

    case SYNTAX_SYMBOL_DECL: {
        auto decl = (SyntaxSymbolDeclaration*)elem;

        for (s64 i = 0; i < decl->elements.size; i += 1) {
            Identifier *identifier = find(&state->env->current_scope->identifier_table, decl->symbols[i]->name);
            assert(identifier != 0);

            state->declaration_name = decl->symbols[i]->name;
            
            identifier->backend_data = codegen(state, decl->elements[i]);
        }

        state->declaration_name = {};
    } break;

    case SYNTAX_LAMBDA_DECL: {
        auto decl = (SyntaxLambda*)elem;

        List<LLVMTypeRef> params = {};
        DEFER(destroy(&params));
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

        char *name = 0;
        if (state->declaration_name != "") {
            name = c_string_copy(state->declaration_name, TempAllocator);
        }

        LLVMTypeRef  lambda_type  = LLVMFunctionType(return_type, params.data, params.size, false);
        LLVMValueRef lambda_value = LLVMAddFunction(state->module, name, lambda_type);

        if (decl->type.flags & TYPE_FLAG_FOREIGN) { return lambda_value; };

        for (s64 i = 0; i < params.size; i += 1) {
            Identifier *identifier = find(&decl->scope.identifier_table, decl->params[i].name);
            assert(identifier != 0);

            identifier->backend_data = LLVMGetParam(lambda_value, i);
        }

        LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(state->context, lambda_value, "entry");
        LLVMPositionBuilderAtEnd(state->builder, entry);

        codegen(state, &decl->scope);

        if (decl->returns.size == 0) {
            LLVMBuildRetVoid(state->builder);
        }

        return lambda_value;
    } break;

    case SYNTAX_RETURN: {
        auto ret = (SyntaxReturn*)elem;

        if (ret->returns.size == 0) {
            LLVMBuildRetVoid(state->builder);
        } else if (ret->returns.size == 1) {
            LLVMBuildRet(state->builder, codegen(state, ret->returns[0]));
        } else {
            // TODO: Return multiple values as tuple?
            die("CODEGEN: Return has two or more values.");
        }
    } break;

    case SYNTAX_CALL: {
        auto call = (SyntaxCall*)elem;

        if (call->callee->kind != SYNTAX_IDENTIFIER) {
            die("CODEGEN: Currently only identifiers callable.");
        }

        String name = ((SyntaxIdentifier*)call->callee)->name;
        Identifier *identifier = resolve_identifier(state->env->current_scope, name);
        assert(identifier != 0);

        LLVMValueRef lambda = (LLVMValueRef)identifier->backend_data;

        List<LLVMValueRef> args = {};
        DEFER(destroy(&args));

        for (s64 i = 0; i < call->args.size; i += 1) {
            append(&args, codegen(state, call->args[i]));
        }

        List<LLVMTypeRef> params = {};
        DEFER(destroy(&params));
        for (s64 i = 0; i < call->args.size; i += 1) {
            append(&params, as_llvm_type(state, &call->args[i]->type));
        }

        LLVMTypeRef return_type = as_llvm_type(state, call->type.as.lambda.returns[0]);
        LLVMTypeRef lambda_type = LLVMFunctionType(return_type, params.data, params.size, false);

        return LLVMBuildCall2(state->builder, lambda_type, lambda, args.data, args.size, "call");
    } break;

    default:
        print("Unknown SyntaxElement kind %S.\n", enum_string(elem->kind));
        die("Aborting...\n");
    }

    return 0;
}

void codegen_llvm(Environment *env) {
    CodegenState state = {};
    state.context = LLVMContextCreate();
    state.module  = LLVMModuleCreateWithNameInContext("knot", state.context);
    state.builder = LLVMCreateBuilderInContext(state.context);
    state.env     = env;

    codegen(&state, &env->root);

    char *error = 0;
    LLVMVerifyModule(state.module, LLVMAbortProcessAction, &error);
    LLVMDisposeMessage(error);
    error = 0;

#if 1
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

