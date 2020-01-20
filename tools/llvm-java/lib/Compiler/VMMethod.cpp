//===-- VMMethod.cpp - Compiler representation of a Java method -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of the Method class that represents a
// compile time representation of a Java class method (java.lang.Method).
//
//===----------------------------------------------------------------------===//

#include "VMMethod.h"
#include "Resolver.h"
#include "VMClass.h"
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/ADT/STLExtras.h>

using namespace llvm;
using namespace llvm::Java;

void VMMethod::init()
{
   std::string methodName = method_->getName()->str();
   std::string methodDescriptor = method_->getDescriptor()->str();
   std::string functionName =
    parent_->getName() + '/' + methodName + methodDescriptor;

  Resolver* resolver = parent_->getResolver();
  // FIXME: This type should be taken from the owning class's constant
  // pool (parsed only once per class). This means the
  // Resolver::getType() should be moved in VMClass and its return
  // value should be cached in the constant pool along with the
  // others.
   FunctionType* functionType = cast<FunctionType>(
    resolver->getType(methodDescriptor, !method_->isStatic()));
  Module* module = resolver->getModule();
  LLVMContext &ctx = module->getContext();
  function_ = dyn_cast<Function>(module->getOrInsertFunction(functionName, functionType));

  std::vector< Type*> argTypes;
  argTypes.reserve(2);
  argTypes.push_back(resolver->getObjectBaseType());
  argTypes.push_back(PointerType::get(Type::getInt8Ty(getGlobalContext()), 0));
   FunctionType* bridgeFunctionType =
    FunctionType::get(functionType->getReturnType(), argTypes, false);
  bridgeFunction_ = dyn_cast<Function>(module->getOrInsertFunction("bridge_to_" + functionName,
                                                bridgeFunctionType));
  BasicBlock* bb = BasicBlock::Create(module->getContext(), "entry", bridgeFunction_);
  std::vector<Value*> params;
  params.reserve(functionType->getNumParams());
  Value* objectArg = bridgeFunction_->arg_begin();
  Value* vaList = next(bridgeFunction_->arg_begin());

  if (!method_->isStatic())
    params.push_back(objectArg);
  for (unsigned i = !method_->isStatic(), e = functionType->getNumParams();
       i != e; ++i) {
     Type* paramType = functionType->getParamType(i);
     //Type* argType = paramType->getVAArgsPromotedType();
    Value* arg = new VAArgInst(vaList, paramType, "tmp", bb);
    /*if (paramType != argType)
      arg = CastInst::CreateIntegerCast(arg, paramType, true, "tmp", bb);*/
    params.push_back(arg);
  }
  if (functionType->getReturnType() == Type::getVoidTy(getGlobalContext())) {
    CallInst::Create(function_, params, "", bb);
    ReturnInst::Create(ctx, bb);
  }
  else {
    Value* result = CallInst::Create(function_, params, "result", bb);
    ReturnInst::Create(ctx, result, bb);
  }
}

VMMethod::VMMethod( VMClass* parent,  Method* method)
  : parent_(parent),
    method_(method),
    index_(-1)
{
  assert(isStaticallyBound() && "This should be a statically bound method!");
  init();
}

VMMethod::VMMethod( VMClass* parent,  Method* method, int index)
  : parent_(parent),
    method_(method),
    index_(index)
{
  assert(isDynamicallyBound() && "This should be a dynamically bound method!");
  init();
}

llvm::Constant* VMMethod::buildMethodDescriptor() 
{
  llvm::Constant* fd = ConstantDataArray::getString(getGlobalContext(), getName() + getDescriptor());

  // create a constant array with the string initializer.
  // then use the GetElementPtr to retreve the element of first element.
  GlobalVariable*gv = new GlobalVariable(
      *parent_->getResolver()->getModule(),
      fd->getType(),
      true,
      GlobalVariable::InternalLinkage,
      fd,
      getName() + getDescriptor());
  LLVMContext &ctx = getGlobalContext();
  Value* args[] = {ConstantInt::get(Type::getInt32Ty(ctx), 0), ConstantInt::get(Type::getInt32Ty(ctx), 0)};
  return ConstantExpr::getGetElementPtr(gv, makeArrayRef(args), true);
}

llvm::Constant* VMMethod::getBridgeFunction() 
{
  return bridgeFunction_;
}
