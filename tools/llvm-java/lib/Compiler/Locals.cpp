//===-- Locals.h - Java locals ----------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the abstraction of Java locals. We model the
// locals as an array of lazily created allocas.
//
//===----------------------------------------------------------------------===//

#include "Locals.h"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/ADT/StringExtras.h>

using namespace llvm;
using namespace llvm::Java;

Locals::Locals( Resolver* resolver, unsigned maxLocals)
  : resolver_(resolver),
    locals_(maxLocals)
{

}

void Locals::store(unsigned i, Value* value, BasicBlock* insertAtEnd)
{
  Type* valueTy = value->getType();
  Type* storageTy = resolver_->getStorageType(valueTy);
  if (valueTy != storageTy)
    value = CastInst::CreateZExtOrBitCast(value, storageTy, "to-storage-type", insertAtEnd);

  SlotMap& slotMap = locals_[i];
  SlotMap::iterator it = slotMap.find(storageTy);

  if (it == slotMap.end()) {
    // Insert the alloca at the end of the entry block.
    BasicBlock* entry = &insertAtEnd->getParent()->getEntryBlock();
    AllocaInst* alloca =
      new AllocaInst(storageTy, NULL, "local"+utostr(i), entry);
    it = slotMap.insert(it, std::make_pair(storageTy, alloca));
  }

  new StoreInst(value, it->second, insertAtEnd);
}

llvm::Value* Locals::load(unsigned i,  Type* valueTy,
                          BasicBlock* insertAtEnd)
{
   Type* storageTy = resolver_->getStorageType(valueTy);

  SlotMap& slotMap = locals_[i];
  SlotMap::iterator it = slotMap.find(storageTy);

  assert(it != slotMap.end() && "Attempt to load a non initialized global!");
  Value* value = new LoadInst(it->second, "load", insertAtEnd);
  if (valueTy != storageTy)
    value = CastInst::CreateZExtOrBitCast(value, const_cast<Type*>(valueTy), "from-storage-type", insertAtEnd);
  return value;
}
