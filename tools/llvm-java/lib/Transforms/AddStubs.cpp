//===- AddStubs.cpp - Add Stubs Pass --------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements a stub adder pass. Because class2llvm is not able to
// compile all of java at the moment, this pass is used to add dummy returns
// to those functions.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "addstubs"

#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <iostream>
#include <llvm/IR/Module.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Instructions.h>

using namespace llvm;

namespace {
  struct AddStubs : public ModulePass {
    static char ID;
    AddStubs() : ModulePass(ID) {
    }
    virtual bool runOnModule(Module &M);
  };
}
char AddStubs::ID = 1;

bool AddStubs::runOnModule(llvm::Module &M) {
  LLVMContext &ctx = M.getContext();
  static Constant* ALL_ONES = Constant::getAllOnesValue(Type::getInt64Ty(ctx));
  for (Module::iterator F = M.begin(), E = M.end(); F != E; ++F)
    if (F->empty() &&
        (F->getName().find("java") != std::string::npos ||
            F->getName().find("gnu") != std::string::npos)) {
      DEBUG(std::cerr << "Stubbing out: " << F->getName().str() << '\n');
      BasicBlock* entry = BasicBlock::Create(ctx, "entry", F);
      if (F->getReturnType()->isVoidTy())
        ReturnInst::Create(ctx, entry);
      else
        ReturnInst::Create(ctx, CastInst::Create(Instruction::CastOps::BitCast, ALL_ONES, F->getReturnType(), "dummy-value", entry),
                           entry);
    }
  return true;
}
