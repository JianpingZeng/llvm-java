//===-- Resolver.cpp - Class resolver for Java classes ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of the Java class resolver.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "javaresolver"

#include "Resolver.h"
#include <llvm/Java/ClassFile.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/Debug.h>
#include <iostream>

using namespace llvm;
using namespace llvm::Java;

Resolver::Resolver(Module* module)
  : module_(module),
    nextInterfaceIndex_(0)
{
  LLVMContext &ctx = module->getContext();

  // Compute the class record type. A class record looks like:
  //
  // struct class_record {
  //   struct type_info;
  // };
  //
  classRecordType_ = StructType::create(module_->getContext(), "struct.llvm_java_class_record");



  // struct type_info {
  //   char* name;
  //   int depth;
  //   struct class_record** superclasses;
  //   int interfaceIndex;
  //   struct class_record** interfaces;
  //   struct class_record* component;
  //   int elementSize;
  //   char** fieldDescriptors;
  //   unsigned* fieldOffsets;
  //   char** staticFieldDescriptors;
  //   void** staticFields;
  //   char** MethodDescriptors;
  //   void** Methods;
  //   char** staticMethodDescriptors;
  //   void** staticMethods;
  // };
  typeInfoType_ = StructType::create(ctx, "struct.llvm_java_typeinfo");
  // initialize all member types of type info.
  // Compute the type_info type.
  SmallVector<Type*, 8> elements;
  elements.push_back(PointerType::get(Type::getInt8Ty(ctx), 0));
  elements.push_back(Type::Type::getInt32Ty(ctx));
  elements.push_back(PointerType::get(PointerType::get(classRecordType_, 0), 0));
  elements.push_back(Type::Type::getInt32Ty(ctx));
  elements.push_back(PointerType::get(PointerType::get(classRecordType_, 0), 0));
  elements.push_back(PointerType::get(classRecordType_, 0));
  elements.push_back(Type::Type::getInt32Ty(ctx));
  elements.push_back(PointerType::get(PointerType::get(Type::getInt8Ty(ctx), 0), 0));
  elements.push_back(PointerType::get(Type::getInt32Ty(ctx), 0));
  elements.push_back(PointerType::get(PointerType::get(Type::getInt8Ty(ctx), 0), 0));
  elements.push_back(PointerType::get(PointerType::get(Type::getInt8Ty(ctx), 0), 0));
  elements.push_back(PointerType::get(PointerType::get(Type::getInt8Ty(ctx), 0), 0));
  elements.push_back(PointerType::get(PointerType::get(Type::getInt8Ty(ctx), 0), 0));
  elements.push_back(PointerType::get(PointerType::get(Type::getInt8Ty(ctx), 0), 0));
  elements.push_back(PointerType::get(PointerType::get(Type::getInt8Ty(ctx), 0), 0));
  typeInfoType_->setBody(elements);

  // Initialize the member type for class_record.
  classRecordType_->setBody(typeInfoType_);

  objectBaseLayoutType_ = (StructType::create(module_->getContext(), "struct.llvm_java_object_base"));
  objectBaseLayoutType_->setBody(PointerType::get(Type::getInt8PtrTy(ctx), 0));
  objectBaseType_ = (PointerType::get(objectBaseLayoutType_, 0));
  classRecordPtrType_ = (PointerType::get(classRecordType_, 0));
}

Type* Resolver::getType(std::string& descriptor,
                              bool memberMethod) 
{
  unsigned i = 0;
  return getTypeHelper(descriptor, i, memberMethod);
}

Type* Resolver::getTypeHelper(std::string& descr,
                                    unsigned& i,
                                    bool memberMethod) 
{
  assert(i < descr.size());
  switch (descr[i++]) {
  case 'B': return Type::getInt8Ty(getGlobalContext());
  case 'C': return Type::Type::getInt16Ty(getGlobalContext());
  case 'D': return Type::Type::getDoubleTy(getGlobalContext());
  case 'F': return Type::Type::getFloatTy(getGlobalContext());
  case 'I': return Type::Type::getInt32Ty(getGlobalContext());
  case 'J': return Type::Type::getInt64Ty(getGlobalContext());
  case 'S': return Type::Type::getInt16Ty(getGlobalContext());
  case 'Z': return Type::Type::getInt1Ty(getGlobalContext());
  case 'V': return Type::Type::getVoidTy(getGlobalContext());
  case 'L': {
    unsigned e = descr.find(';', i);
    i = e + 1;
    return getObjectBaseType();
  }
  case '[':
    // Skip '['s.
    if (descr[i] == '[')
      do { ++i; } while (descr[i] == '[');
    // Consume the element type
    getTypeHelper(descr, i);
    return getObjectBaseType();
  case '(': {
    SmallVector<Type*, 9> params;
    if (memberMethod)
      params.push_back(getObjectBaseType());
    while (descr[i] != ')')
      params.push_back(getTypeHelper(descr, i));
    return FunctionType::get(getTypeHelper(descr, ++i), params, false);
  }
    // FIXME: Throw something
  default:  assert(0 && "Cannot parse type descriptor!");
  }
  return 0; // not reached
}

VMClass* Resolver::getClassForDesc(std::string descriptor)
{
  LLVMContext &ctx = getGlobalContext();
  ClassMap::iterator it = classMap_.lower_bound(descriptor);
  if (it == classMap_.end() || it->first != descriptor) {
    switch (descriptor[0]) {
    case 'B':
      it = insertClass(it, VMClass(this, Type::getInt8Ty(ctx), ctx));
      break;
    case 'C':
      it = insertClass(it, VMClass(this, Type::Type::getInt16Ty(ctx), ctx));
      break;
    case 'D':
      it = insertClass(it, VMClass(this, Type::Type::getDoubleTy(ctx), ctx));
      break;
    case 'F':
      it = insertClass(it, VMClass(this, Type::Type::getFloatTy(ctx), ctx));
      break;
    case 'I':
      it = insertClass(it, VMClass(this, Type::Type::getInt32Ty(ctx), ctx));
      break;
    case 'J':
      it = insertClass(it, VMClass(this, Type::Type::getInt64Ty(ctx), ctx));
      break;
    case 'S':
      it = insertClass(it, VMClass(this, Type::Type::getInt16Ty(ctx), ctx));
      break;
    case 'Z':
      it = insertClass(it, VMClass(this, Type::Type::getInt1Ty(ctx), ctx));
      break;
    case 'V':
      it = insertClass(it, VMClass(this, Type::Type::getVoidTy(ctx), ctx));
      break;
    case 'L': {
      unsigned pos = descriptor.find(';', 1);
      std::string className = descriptor.substr(1, pos - 1);
      it = insertClass(it, VMClass(this, className, ctx));
      break;
    }
    case '[': {
      std::string componentDescriptor = descriptor.substr(1);
      it = insertClass(it, VMClass(this, getClassForDesc(componentDescriptor), ctx));
      break;
    }
    default:
      assert(0 && "Cannot parse type descriptor!");
      abort();
    }
    it->second.link();
    if (!it->second.isPrimitive() && !it->second.isInterface()) {
      dyn_cast<StructType>(it->second.getLayoutType())->setName("struct." + descriptor);
    }
    DEBUG(std::cerr << "Loaded class: " << it->second.getName());
    DEBUG(std::cerr << " (" << it->second.getInterfaceIndex() << ")\n");
  }

  return &it->second;
}

VMClass* Resolver::getClass(JType type)
{
  switch (type) {
  case BOOLEAN: return getClassForDesc("Z");
  case CHAR: return getClassForDesc("C");
  case FLOAT: return getClassForDesc("F");
  case DOUBLE: return getClassForDesc("D");
  case BYTE: return getClassForDesc("B");
  case SHORT: return getClassForDesc("S");
  case INT: return getClassForDesc("I");
  case LONG: return getClassForDesc("J");
  default: assert(0 && "Unhandled JType!"); abort();
  }
}

Type* Resolver::getStorageType(Type* type) 
{
  if (isa<PointerType>(type))
    return getObjectBaseType();
  else if (type == Type::Type::getInt1Ty(getGlobalContext()) ||
           type == Type::getInt8Ty(getGlobalContext()) ||
           type == Type::getInt8Ty(getGlobalContext()) ||
           type == Type::Type::getInt16Ty(getGlobalContext()) ||
           type == Type::Type::getInt16Ty(getGlobalContext()) ||
           type == Type::getInt32Ty(getGlobalContext()))
    return Type::Type::getInt32Ty(getGlobalContext());
  else if (type == Type::Type::getInt64Ty(getGlobalContext()))
    return Type::Type::getInt64Ty(getGlobalContext());
  else
    return type;
}

void Resolver::emitClassRecordsArray() 
{
  std::vector<llvm::Constant*> init;
  init.reserve(classMap_.size() + 1);

  for (ClassMap::iterator i = classMap_.begin(), e = classMap_.end();
       i != e; ++i) {
       if (i->second.getClassRecord()) {
         init.push_back(ConstantExpr::getPointerBitCastOrAddrSpaceCast(i->second.getClassRecord(),
                                                                       classRecordPtrType_));
       }
    }

  // Null terminate the array.
  init.push_back(llvm::Constant::getNullValue(classRecordPtrType_));

  ArrayType* arrayType = ArrayType::get(classRecordPtrType_, init.size());

  new GlobalVariable(
    *module_,
    arrayType,
    true,
    GlobalVariable::ExternalLinkage,
    ConstantArray::get(arrayType, init),
    "llvm_java_class_records");
}
