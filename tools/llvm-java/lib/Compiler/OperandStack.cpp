//===-- OperandStack.cpp - Java operand stack -----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the abstraction of a Java operand stack. We
// model the java operand stack as a stack of LLVM allocas.
//
//===----------------------------------------------------------------------===//

#include "OperandStack.h"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/Java/Compiler.h>

using namespace llvm;
using namespace llvm::Java;

void OperandStack::copySlots( SlotMap& src,
                             SlotMap& dst,
                             BasicBlock* insertAtEnd)
{
  SlotMap::const_iterator it = src.begin();
  SlotMap::const_iterator end = src.end();

  for (; it != end; ++it) {
    AllocaInst* srcSlot = it->second;
    AllocaInst* dstSlot = getOrCreateSlot(dst, it->first, insertAtEnd);
    Value* value = new LoadInst(srcSlot, "tmp", insertAtEnd);
    new StoreInst(value, dstSlot, insertAtEnd);
  }
}

llvm::AllocaInst* OperandStack::getOrCreateSlot(SlotMap& slotMap,
                                                 Type* type,
                                                BasicBlock* bb)
{
  SlotMap::iterator it = slotMap.find(type);

  if (it == slotMap.end()) {
    // Insert the alloca at the beginning of the entry block.
    BasicBlock* entry = &bb->getParent()->getEntryBlock();
    AllocaInst* alloca = new AllocaInst(const_cast<Type*>(type), NULL, "opStack", entry);
    it = slotMap.insert(it, std::make_pair(type, alloca));
  }

  return it->second;
}

void OperandStack::push(Value* value, BasicBlock* insertAtEnd)
{
  assert(currentDepth_ < stack_.size() && "Pushing to a full stack!");
   Type* valueTy = value->getType();
//   std::cerr << "PUSH(" << insertAtEnd << "/"
//             << insertAtEnd->getParent()->getName() << " " << stack_.size()
//             << ") Depth: " << currentDepth_ << " type: " << *valueTy << '\n';
   Type* storageTy = resolver_->getStorageType(valueTy);
  if (valueTy != storageTy)
    value = CastInst::CreateZExtOrBitCast(value, const_cast<Type*>(storageTy), "to-storage-type", insertAtEnd);

  SlotMap& slotMap = stack_[currentDepth_];
  AllocaInst* slot = getOrCreateSlot(slotMap, storageTy, insertAtEnd);
  new StoreInst(value, slot, insertAtEnd);
  currentDepth_ += 1 + resolver_->isTwoSlotType(storageTy);
  assert(currentDepth_ < stack_.size() && "Pushed more than max stack depth!");
}

llvm::Value* OperandStack::pop( Type* valueTy, BasicBlock* insertAtEnd)
{
   Type* storageTy = resolver_->getStorageType(valueTy);

  assert(currentDepth_ != 0 && "Popping from an empty stack!");
  currentDepth_ -= 1 + resolver_->isTwoSlotType(storageTy);
//   std::cerr << "POP(" << insertAtEnd->getName() << "/"
//             << insertAtEnd->getParent()->getName() << " " << stack_.size()
//             << ")  Depth: " << currentDepth_ << " type: " << *valueTy << '\n';

  SlotMap& slotMap = stack_[currentDepth_];
  SlotMap::iterator it = slotMap.find(storageTy);

  assert(it != slotMap.end() && "Type mismatch on operand stack!");
  Value* value = new LoadInst(it->second, "pop", insertAtEnd);
  if (valueTy != storageTy)
    value = CastInst::CreateZExtOrBitCast(value, const_cast<Type*>(valueTy), "from-storage-type", insertAtEnd);
  return value;
}

/// ..., value -> ...
void OperandStack::do_pop(BasicBlock* insertAtEnd)
{
  assert(currentDepth_ != 0 && "Popping from an empty stack!");
  --currentDepth_;
}

/// ..., value2, value1 -> ...
/// ..., value -> ...
void OperandStack::do_pop2(BasicBlock* insertAtEnd)
{
  do_pop(insertAtEnd);
  do_pop(insertAtEnd);
}

/// ..., value -> ..., value, value
void OperandStack::do_dup(BasicBlock* insertAtEnd)
{
  assert(currentDepth_ != 0 && "Popping from an empty stack!");
  copySlots(stack_[currentDepth_-1], stack_[currentDepth_], insertAtEnd);
  ++currentDepth_;
}

/// ..., value2, value1 -> ..., value1, value2, value1
void OperandStack::do_dup_x1(BasicBlock* insertAtEnd)
{
  copySlots(stack_[currentDepth_-1], stack_[currentDepth_], insertAtEnd);
  copySlots(stack_[currentDepth_-2], stack_[currentDepth_-1], insertAtEnd);
  copySlots(stack_[currentDepth_], stack_[currentDepth_-2], insertAtEnd);
  ++currentDepth_;
}

/// ..., value3, value2, value1 -> ..., value1, value3, value2, value1
/// ..., value2, value1 -> ..., value1, value2, value1
void OperandStack::do_dup_x2(BasicBlock* insertAtEnd)
{
  copySlots(stack_[currentDepth_-1], stack_[currentDepth_], insertAtEnd);
  copySlots(stack_[currentDepth_-2], stack_[currentDepth_-1], insertAtEnd);
  copySlots(stack_[currentDepth_-3], stack_[currentDepth_-2], insertAtEnd);
  copySlots(stack_[currentDepth_], stack_[currentDepth_-3], insertAtEnd);
  ++currentDepth_;
}

/// ..., value2, value1 -> ..., value2, value1, value2, value1
void OperandStack::do_dup2(BasicBlock* insertAtEnd)
{
  copySlots(stack_[currentDepth_-2], stack_[currentDepth_], insertAtEnd);
  copySlots(stack_[currentDepth_-1], stack_[currentDepth_+1], insertAtEnd);
  currentDepth_ += 2;
}

/// ..., value3, value2, value1 -> ..., value2, value1, value3, value2, value1
/// ..., value2, value1 -> ..., value1, value2, value1
void OperandStack::do_dup2_x1(BasicBlock* insertAtEnd)
{
  copySlots(stack_[currentDepth_-1], stack_[currentDepth_+1], insertAtEnd);
  copySlots(stack_[currentDepth_-2], stack_[currentDepth_], insertAtEnd);
  copySlots(stack_[currentDepth_-3], stack_[currentDepth_-1], insertAtEnd);
  copySlots(stack_[currentDepth_+1], stack_[currentDepth_-2], insertAtEnd);
  copySlots(stack_[currentDepth_], stack_[currentDepth_-3], insertAtEnd);
  currentDepth_ += 2;
}

/// ..., value4, value3, value2, value1 -> ..., value2, value1, value4, value3, value2, value1
/// ..., value3, value2, value1 -> ..., value1, value3, value2, value1
/// ..., value3, value2, value1 -> ..., value2, value1, value3, value2, value1
/// ..., value2, value1 -> ..., value1, value2, value1
void OperandStack::do_dup2_x2(BasicBlock* insertAtEnd)
{
  copySlots(stack_[currentDepth_-1], stack_[currentDepth_+1], insertAtEnd);
  copySlots(stack_[currentDepth_-2], stack_[currentDepth_], insertAtEnd);
  copySlots(stack_[currentDepth_-3], stack_[currentDepth_-1], insertAtEnd);
  copySlots(stack_[currentDepth_-4], stack_[currentDepth_-2], insertAtEnd);
  copySlots(stack_[currentDepth_+1], stack_[currentDepth_-3], insertAtEnd);
  copySlots(stack_[currentDepth_], stack_[currentDepth_-4], insertAtEnd);
  currentDepth_ += 2;
}

void OperandStack::do_swap(BasicBlock* insertAtEnd)
{
  SlotMap tmp;
  copySlots(stack_[currentDepth_-1], tmp, insertAtEnd);
  copySlots(stack_[currentDepth_-2], stack_[currentDepth_-1], insertAtEnd);
  copySlots(tmp, stack_[currentDepth_-2], insertAtEnd);
}
