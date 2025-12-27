#include "codegen.h"

#include "knot.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"


INTERNAL llvm::LLVMContext  Context;
INTERNAL llvm::Module       Module("knot", Context);
INTERNAL llvm::IRBuilder<>  Builder(Context);


void codegen_llvm(Environment *env) {
    auto params = llvm::ArrayRef<llvm::Type*>();
    llvm::FunctionType *main_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(Context), params, false);
    llvm::Function *main_func = llvm::Function::Create(main_type, llvm::Function::InternalLinkage, "main", Module);

    llvm::BasicBlock *block = llvm::BasicBlock::Create(Context, "entry", main_func);
    Builder.SetInsertPoint(block);

    llvm::Value *lhs = llvm::ConstantInt::get(Context, llvm::APInt(32, 42));
    llvm::Value *rhs = llvm::ConstantInt::get(Context, llvm::APInt(32, 42));

    Builder.CreateRet(Builder.CreateAdd(lhs, rhs, "add"));

    llvm::verifyFunction(*main_func, &llvm::errs());

    //Module.print(llvm::errs(), nullptr);
    
    auto target_triple = llvm::sys::getDefaultTargetTriple();

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);

    if (!target) {
        llvm::errs() << error;
        return;
    }

    auto cpu = "generic";
    auto features = "";

    llvm::Triple triple(target_triple);

    llvm::TargetOptions options;
    auto target_machine = target->createTargetMachine(triple, cpu, features, options, llvm::Reloc::PIC_);

    Module.setDataLayout(target_machine->createDataLayout());
    Module.setTargetTriple(triple);

    std::error_code ec;
    llvm::raw_fd_ostream obj("output.o", ec, llvm::sys::fs::OF_None);

    llvm::legacy::PassManager pass;
    auto file_type = llvm::CodeGenFileType::ObjectFile;

    if (target_machine->addPassesToEmitFile(pass, obj, 0, file_type)) {
        llvm::errs() << "Nope\n";
        return;
    }

    pass.run(Module);
    obj.flush();
}

