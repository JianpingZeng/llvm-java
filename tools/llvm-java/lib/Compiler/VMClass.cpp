//===-- VMClass.cpp - Compiler representation of a Java class ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the Class class that represents a
// compile time representation of a Java class (java.lang.Class). This unlike
// a classfile representation, it resolves the constant pool, creates global
// variables for the static members of this class and also creates the class
// record (vtable) of this class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "javaclass"

#include "VMClass.h"
#include "Resolver.h"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <llvm/Java/ClassFile.h>
#include <llvm/Support/Debug.h>
#include <iostream>
#include <llvm/IR/LLVMContext.h>

using namespace llvm;
using namespace llvm::Java;

int VMClass::INVALID_INTERFACE_INDEX = 1;

// On initialization we create a placeholder global for the class
// record that will be patched later when the class record is
// computed.
void VMClass::init()
{
  classRecord_ = new GlobalVariable(
    *resolver_->getModule(),
    StructType::get(ctx),
    false,
    GlobalVariable::ExternalLinkage,
    NULL,
    getName() + "<class_record>");
}

VMClass::VMClass(Resolver* resolver, std::string className, LLVMContext &ctx_)
  : name_(className),
    descriptor_(Resolver::canonicalizeClassName(className)),
    resolver_(resolver),
    classFile_(ClassFile::get(className)),
    componentClass_(NULL),
    layoutType_(StructType::get(ctx)),
    type_(PointerType::get(layoutType_, 0)),
    interfaceIndex_(INVALID_INTERFACE_INDEX),
    resolvedConstantPool_(classFile_->getNumConstants()),
    ctx(ctx_)
{
  init();
}

VMClass::VMClass(Resolver* resolver, VMClass* componentClass, LLVMContext &ctx_)
  : name_('[' + componentClass->getDescriptor()),
    descriptor_(name_),
    resolver_(resolver),
    classFile_(NULL),
    componentClass_(componentClass),
    layoutType_(StructType::get(ctx)),
    type_(PointerType::get(layoutType_, 0)),
    interfaceIndex_(INVALID_INTERFACE_INDEX),
    ctx(ctx_)
{
  init();
}

VMClass::VMClass(Resolver* resolver, Type* type, LLVMContext &ctx_)
  : name_(type->isIntegerTy(8) ? "byte" :
          type->isIntegerTy(16) ? "char" :
          type->isDoubleTy() ? "double" :
          type->isFloatTy()  ? "float" :
          type->isIntegerTy(32) ? "int" :
          type->isIntegerTy(64) ? "long" :
          type == Type::getInt16PtrTy(ctx_) ? "short" :
          type->isIntegerTy(1) ? "boolean" : "void"),
    descriptor_(type == Type::getInt8Ty(ctx_)  ? "B" :
                type == Type::Type::getInt16Ty(ctx_) ? "C" :
                type->isDoubleTy() ? "D" :
                type->isFloatTy()  ? "F" :
                type == Type::Type::getInt32Ty(ctx_)    ? "I" :
                type->isIntegerTy(64)   ? "J" :
                type->isIntegerTy(16)  ? "S" :
                type->isIntegerTy(1)   ? "Z" : "V"),
    resolver_(resolver),
    classFile_(NULL),
    componentClass_(NULL),
    layoutType_(static_cast<StructType*>(type)),
    type_(type),
    interfaceIndex_(INVALID_INTERFACE_INDEX),
    ctx(ctx_)
{
  init();
}

VMField* VMClass::lookupField(std::string name) 
{
  if (VMField* field = getField(name))
    return field;

  for (unsigned i = 0, e = getNumInterfaces(); i != e; ++i) {
    VMClass* interface = getInterface(i);
    if (VMField* field = interface->getField(name))
      return field;
  }

  for (unsigned i = 0, e = getNumSuperClasses(); i != e; ++i) {
    VMClass* superClass = getSuperClass(i);
    if (VMField* field = superClass->getField(name))
      return field;
  }

  return NULL;
}

VMMethod* VMClass::lookupMethod(std::string nameAndType) 
{
  if (VMMethod* method = getMethod(nameAndType))
    return method;

  if (isInterface())
    for (unsigned i = 0, e = getNumInterfaces(); i != e; ++i) {
      VMClass* interface = getInterface(i);
      if (VMMethod* method = interface->getMethod(nameAndType))
        return method;
    }
  else
    for (unsigned i = 0, e = getNumSuperClasses(); i != e; ++i) {
      VMClass* superClass = getSuperClass(i);
      if (VMMethod* method = superClass->getMethod(nameAndType))
        return method;
    }

  return NULL;
}

void VMClass::computeLayout()
{
  DEBUG(std::cerr << "Computing layout for: " << getName() << '\n');
  // The layout of primitive classes is already computed.
  if (isPrimitive()) {
    DEBUG(std::cerr << "Computed layout for: " << getName() << '\n');
    return;
  }

  SmallVector<Type*, 8> layout;
  if (isArray()) {
    layout.reserve(3);
    layout.push_back(resolver_->getClass("java/lang/Object")->getLayoutType());
    layout.push_back(Type::getInt32Ty(ctx));
    layout.push_back(ArrayType::get(const_cast<Type*>(componentClass_->getType()), 0));
  }
  else {
    if (VMClass* superClass = getSuperClass())
      layout.push_back(superClass->getLayoutType());
    else // This is java/lang/Object
      layout.push_back(resolver_->getObjectBaseLayoutType());
    // Now add the fields.
    Fields& fields = classFile_->getFields();
    for (unsigned i = 0, e = fields.size(); i != e; ++i) {
      Field* field = fields[i];
      std::string name = field->getName()->str();
      if (field->isStatic()) {
        FieldMap::iterator i = fieldMap_.insert(
          std::make_pair(name, VMField(this, field))).first;
        staticFields_.push_back(&i->second);
      }
      else {
        unsigned index = memberFields_.size() + 1;
        FieldMap::iterator i = fieldMap_.insert(
          std::make_pair(name, VMField(this, field, index))).first;
        VMField* vmf = &i->second;
        memberFields_.push_back(vmf);
        layout.push_back(vmf->getClass()->getType());
      }
    }
  }

  layoutType_ = StructType::get(ctx, layout);
  DEBUG(std::cerr << "Computed layout for: " << getName() << '\n');
}

llvm::Constant* VMClass::buildSuperClassRecords() 
{
  std::vector<llvm::Constant*> init;
  init.reserve(getNumSuperClasses());
  for (unsigned i = getNumSuperClasses(); i--; )
    init.push_back(ConstantExpr::getPointerBitCastOrAddrSpaceCast(
                     getSuperClass(i)->getClassRecord(),
                     resolver_->getClassRecordPtrType()));

  ArrayType* superClassRecordsType =
    ArrayType::get(resolver_->getClassRecordPtrType(), init.size());

  return new GlobalVariable(
      *resolver_->getModule(),
      superClassRecordsType,
      true,
      GlobalVariable::ExternalLinkage,
      ConstantArray::get(superClassRecordsType, init),
      getName() + "<super_class_records>");
}

llvm::Constant*
VMClass::buildInterfaceClassRecord(VMClass* interface) 
{
  assert(interface->isInterface() && "Must be passed an interface!");

  std::vector<llvm::Constant*> init;
  init.reserve(interface->getNumDynamicMethods() + 1);
  // Insert a null type info for this interface.
  init.push_back(llvm::Constant::getNullValue(resolver_->getTypeInfoType()));
  // For each method this interface declares, find the corresponding
  // method in this class and put it in its slot.
  for (unsigned i = 0, e = interface->getNumDynamicMethods(); i != e; ++i) {
    assert(init.size() == i+1 && "Interface method not found in class!");
    VMMethod* interfaceMethod = interface->getDynamicMethod(i);
    for (unsigned j = 0, f = getNumDynamicMethods(); j != f; ++j) {
      VMMethod* method = getDynamicMethod(j);
      if (method->getName() == interfaceMethod->getName() &&
          method->getDescriptor() == interfaceMethod->getDescriptor()) {
        init.push_back(method->getFunction());
        break;
      }
    }
  }

  llvm::Constant* classRecordInit = ConstantStruct::get(StructType::get(ctx), init);

  return ConstantExpr::getPointerBitCastOrAddrSpaceCast(
    new GlobalVariable(
      *resolver_->getModule(),
      classRecordInit->getType(),
      true,
      GlobalVariable::ExternalLinkage,
      classRecordInit,
      getName() + '+' + interface->getName() + "<class_record>"),
    resolver_->getClassRecordPtrType());
}

llvm::Constant* VMClass::buildInterfaceClassRecords() 
{
  // This is an interface or primitive class record so it doesn't
  // implement any interfaces. Thus the pointer to the array of
  // implemented interfaces is null.
  if (isInterface() || isPrimitive()) {
    Type* classRecordPtrPtrType =
      PointerType::get(resolver_->getClassRecordPtrType(), 0);

    return llvm::Constant::getNullValue(classRecordPtrPtrType);
  }

  // Otherwise this is a class or array class record so we have to
  // fill in the array of implemented interfaces up the max interface
  // index and build each individual interface class record for this
  // class.
  llvm::Constant* nullClassRecord =
    llvm::Constant::getNullValue(resolver_->getClassRecordPtrType());
  std::vector<llvm::Constant*> init(getInterfaceIndex()+1, nullClassRecord);

  for (unsigned i = 0, e = getNumInterfaces(); i != e; ++i) {
    VMClass* interface = getInterface(i);
    init[interface->getInterfaceIndex()] = buildInterfaceClassRecord(interface);
  }

  ArrayType* interfaceClassRecordsType =
    ArrayType::get(resolver_->getClassRecordPtrType(), init.size());

  return new GlobalVariable(
      *resolver_->getModule(),
      interfaceClassRecordsType,
      true,
      GlobalVariable::ExternalLinkage,
      ConstantArray::get(interfaceClassRecordsType, init),
      getName() + "<interface_class_records>");
}

llvm::Constant* VMClass::buildClassName() 
{
  llvm::Constant* name = ConstantDataArray::getString(ctx, getName());

  return new GlobalVariable(
      *resolver_->getModule(),
      name->getType(),
      true,
      GlobalVariable::ExternalLinkage,
      name,
      getName() + "<classname>");
}

llvm::Constant* VMClass::buildFieldDescriptors() 
{
  std::vector<llvm::Constant*> init;
  init.reserve(memberFields_.size()+1);

  for (unsigned i = 0, e = memberFields_.size(); i != e; ++i) {
    VMField* field = memberFields_[i];
    init.push_back(field->buildFieldDescriptor());
  }
  // Null terminate.
  init.push_back(llvm::Constant::getNullValue(PointerType::get(Type::getInt8Ty(getGlobalContext()), 0)));

  ArrayType* arrayType =
    ArrayType::get(init.back()->getType(), init.size());

  return new GlobalVariable(
      *resolver_->getModule(),
      arrayType,
      true,
      GlobalVariable::ExternalLinkage,
      ConstantArray::get(arrayType, init),
      getName() + "<field_descriptors>");
}

llvm::Constant* VMClass::buildFieldOffsets() 
{
  std::vector<llvm::Constant*> init;
  init.reserve(memberFields_.size());

  for (unsigned i = 0, e = memberFields_.size(); i != e; ++i) {
    VMField* field = memberFields_[i];
    init.push_back(field->buildFieldOffset());
  }

  ArrayType* arrayType = ArrayType::get(Type::getInt32Ty(getGlobalContext()), init.size());

  return new GlobalVariable(
      *resolver_->getModule(),
      arrayType,
      true,
      GlobalVariable::ExternalLinkage,
      ConstantArray::get(arrayType, init),
      getName() + "<field_offsets>");
}

llvm::Constant* VMClass::buildStaticFieldDescriptors() 
{
  std::vector<llvm::Constant*> init;
  init.reserve(staticFields_.size()+1);

  for (unsigned i = 0, e = staticFields_.size(); i != e; ++i) {
    VMField* field = staticFields_[i];
    init.push_back(field->buildFieldDescriptor());
  }
  // Null terminate.
  init.push_back(llvm::Constant::getNullValue(PointerType::get(Type::getInt8Ty(getGlobalContext()), 0)));

  ArrayType* arrayType =
    ArrayType::get(init.back()->getType(), init.size());

  return new GlobalVariable(
      *resolver_->getModule(),
      arrayType,
      true,
      GlobalVariable::ExternalLinkage,
      ConstantArray::get(arrayType, init),
      getName() + "<static_field_descriptors>");
}

llvm::Constant* VMClass::buildStaticFieldPointers() 
{
  std::vector<llvm::Constant*> init;
  init.reserve(staticFields_.size());

  Type* pointerType = PointerType::get(Type::getInt8Ty(getGlobalContext()), 0);
  for (unsigned i = 0, e = staticFields_.size(); i != e; ++i) {
    VMField* field = staticFields_[i];
    init.push_back(ConstantExpr::getPointerBitCastOrAddrSpaceCast(field->getGlobal(), pointerType));
  }

  ArrayType* arrayType = ArrayType::get(pointerType, init.size());

  return new GlobalVariable(
      *resolver_->getModule(),
      arrayType,
      true,
      GlobalVariable::ExternalLinkage,
      ConstantArray::get(arrayType, init),
      getName() + "<static_field_pointers>");
}

llvm::Constant* VMClass::buildMethodDescriptors() 
{
  std::vector<llvm::Constant*> init;
  init.reserve(getNumStaticMethods() + 1);

  for (unsigned i = 0, e = getNumDynamicMethods(); i != e; ++i) {
    VMMethod* method = getDynamicMethod(i);
    init.push_back(method->buildMethodDescriptor());
  }
  // Null terminate.
  init.push_back(llvm::Constant::getNullValue(PointerType::get(Type::getInt8Ty(getGlobalContext()), 0)));

  ArrayType* arrayType =
    ArrayType::get(init.back()->getType(), init.size());

  return new GlobalVariable(
      *resolver_->getModule(),
      arrayType,
      true,
      GlobalVariable::ExternalLinkage,
      ConstantArray::get(arrayType, init),
      getName() + "<method_descriptors>");
}

llvm::Constant* VMClass::buildMethodPointers() 
{
  std::vector<llvm::Constant*> init;
  init.reserve(getNumStaticMethods());

  Type* pointerType = PointerType::get(Type::getInt8Ty(getGlobalContext()), 0);
  for (unsigned i = 0, e = getNumDynamicMethods(); i != e; ++i) {
    VMMethod* method = getDynamicMethod(i);
    init.push_back(ConstantExpr::getPointerBitCastOrAddrSpaceCast(method->getBridgeFunction(),
                                         pointerType));
  }

  ArrayType* arrayType = ArrayType::get(pointerType, init.size());

  return new GlobalVariable(
      *resolver_->getModule(),
      arrayType,
      true,
      GlobalVariable::ExternalLinkage,
      ConstantArray::get(arrayType, init),
      getName() + "<method_pointers>");
}

llvm::Constant* VMClass::buildStaticMethodDescriptors() 
{
  std::vector<llvm::Constant*> init;
  init.reserve(getNumStaticMethods() + 1);

  for (unsigned i = 0, e = getNumStaticMethods(); i != e; ++i) {
    VMMethod* method = getStaticMethod(i);
    init.push_back(method->buildMethodDescriptor());
  }
  // Null terminate.
  init.push_back(llvm::Constant::getNullValue(PointerType::get(Type::getInt8Ty(getGlobalContext()), 0)));

  ArrayType* arrayType =
    ArrayType::get(init.back()->getType(), init.size());

  return new GlobalVariable(
      *resolver_->getModule(),
      arrayType,
      true,
      GlobalVariable::ExternalLinkage,
      ConstantArray::get(arrayType, init),
      getName() + "<static_method_descriptors>");
}

llvm::Constant* VMClass::buildStaticMethodPointers() 
{
  std::vector<llvm::Constant*> init;
  init.reserve(getNumStaticMethods());

  Type* pointerType = PointerType::get(Type::getInt8Ty(getGlobalContext()), 0);
  for (unsigned i = 0, e = getNumStaticMethods(); i != e; ++i) {
    VMMethod* method = getStaticMethod(i);
    init.push_back(ConstantExpr::getPointerBitCastOrAddrSpaceCast(method->getBridgeFunction(),
                                         pointerType));
  }

  ArrayType* arrayType = ArrayType::get(pointerType, init.size());

  return new GlobalVariable(
      *resolver_->getModule(),
      arrayType,
      true,
      GlobalVariable::ExternalLinkage,
      ConstantArray::get(arrayType, init),
      getName() + "<static_method_pointers>");
}

llvm::Constant* VMClass::buildClassTypeInfo() 
{
  std::vector<llvm::Constant*> init;
  init.reserve(5);

  init.push_back(buildClassName());
  init.push_back(ConstantInt::get(Type::Type::getInt32Ty(getGlobalContext()), getNumSuperClasses(), true));
  init.push_back(buildSuperClassRecords());
  init.push_back(ConstantInt::get(Type::Type::getInt32Ty(getGlobalContext()), getInterfaceIndex(), true));
  init.push_back(buildInterfaceClassRecords());
  if (isArray())
    init.push_back(ConstantExpr::getPointerBitCastOrAddrSpaceCast(getComponentClass()->getClassRecord(),
                                         resolver_->getClassRecordPtrType()));
  else
    init.push_back(
      llvm::Constant::getNullValue(resolver_->getClassRecordPtrType()));
  if (isArray())
    init.push_back(
      ConstantExpr::getIntegerCast(
        ConstantExpr::getSizeOf(getComponentClass()->getType()), Type::Type::getInt32Ty(getGlobalContext()), true));
  else if (isPrimitive())
    init.push_back(ConstantInt::get(Type::Type::getInt32Ty(getGlobalContext()), -2, true));
  else if (isInterface())
    init.push_back(ConstantInt::get(Type::Type::getInt32Ty(getGlobalContext()), -1, true));
  else // A class.
    init.push_back(ConstantInt::get(Type::Type::getInt32Ty(getGlobalContext()), 0, true));

  init.push_back(buildFieldDescriptors());
  init.push_back(buildFieldOffsets());
  init.push_back(buildStaticFieldDescriptors());
  init.push_back(buildStaticFieldPointers());
  init.push_back(buildMethodDescriptors());
  init.push_back(buildMethodPointers());
  init.push_back(buildStaticMethodDescriptors());
  init.push_back(buildStaticMethodPointers());

  return ConstantStruct::get(StructType::get(ctx), init);
}

void VMClass::computeClassRecord()
{
  DEBUG(std::cerr << "Computing class record for: " << getName() << '\n');
  // Find dynamically bound methods.
  if (!isPrimitive()) {
    if (VMClass* superClass = getSuperClass())
      dynamicMethods_ = superClass->dynamicMethods_;

    if (getClassFile()) {
      Methods& methods = classFile_->getMethods();
      for (unsigned i = 0, e = methods.size(); i != e; ++i) {
        Method* method = methods[i];
        std::string name = method->getName()->str();
        std::string descriptor = method->getDescriptor()->str();

        // If method is statically bound just create it.
        if (method->isPrivate() || method->isStatic() || name[0] == '<') {
          MethodMap::iterator i =
            methodMap_.insert(
              std::make_pair(name + descriptor, VMMethod(this, method))).first;
          staticMethods_.push_back(&i->second);
        }
        // Otherwise we need to assign an index for it and update the
        // dynamicMethods_ vector.
        else {
          VMMethod* overridenMethod = NULL;
          for (unsigned i = 0, e = getNumDynamicMethods(); i != e; ++i) {
            VMMethod* m = getDynamicMethod(i);
            if (m->getName() == name && m->getDescriptor() == descriptor)
              overridenMethod = m;
          }

          // If this is an overriden method reuse the method index
          // with the overriding one.
          if (overridenMethod) {
            int index = overridenMethod->getMethodIndex();
            MethodMap::iterator i = methodMap_.insert(
              std::make_pair(name + descriptor,
                             VMMethod(this, method, index))).first;
            dynamicMethods_[index] = &i->second;
          }
          // Otherwise assign it a new index.
          else {
            int index = dynamicMethods_.size();
            MethodMap::iterator i = methodMap_.insert(
              std::make_pair(
                name + descriptor, VMMethod(this, method, index))).first;
            dynamicMethods_.push_back(&i->second);
          }
        }
      }
    }
  }

  std::vector<llvm::Constant*> init;
  init.reserve(1 + getNumDynamicMethods());
  init.push_back(buildClassTypeInfo());
  for (unsigned i = 0, e = getNumDynamicMethods(); i != e; ++i) {
    VMMethod* method = getDynamicMethod(i);
    init.push_back(
      method->isAbstract() ?
      llvm::Constant::getNullValue(method->getFunction()->getType()) :
      method->getFunction());
  }

  llvm::Constant* classRecordInit = ConstantStruct::get(StructType::get(ctx), init);
  dyn_cast<StructType>(classRecordInit->getType())->setName("classRecord." + getName());
  // Set the initializer of the class record.
  classRecord_->setInitializer(classRecordInit);
  // Mark the class record as constant.
  classRecord_->setConstant(true);

  DEBUG(std::cerr << "Computed class record for: " << getName() << '\n');
}

void VMClass::link()
{
  // Primitive classes require no linking.
  if (isPrimitive())
    ;
  else if (isArray()) {
    superClasses_.reserve(1);
    superClasses_.push_back(resolver_->getClass("java/lang/Object"));

    interfaces_.reserve(2);
    interfaces_.push_back(resolver_->getClass("java/lang/Cloneable"));
    interfaces_.push_back(resolver_->getClass("java/io/Serializable"));
  }
  else {
    // This is any class but java/lang/Object.
    if (classFile_->getSuperClass()) {
      // Our direct super class.
      VMClass* superClass = getClass(classFile_->getSuperClassIndex());

      // Add the interfaces of our direct superclass.
      for (unsigned i = 0, e = superClass->getNumInterfaces(); i != e; ++i)
        interfaces_.push_back(superClass->getInterface(i));

      // The first class is the direct super class of this class.
      superClasses_.reserve(superClass->getNumSuperClasses() + 1);
      superClasses_.push_back(superClass);
      for (unsigned i = 0, e = superClass->getNumSuperClasses(); i != e; ++i)
        superClasses_.push_back(superClass->getSuperClass(i));
    }

    // For each of the interfaces we implement, load it and add that
    // interface and all the interfaces it inherits from.
    for (unsigned i = 0, e = classFile_->getNumInterfaces(); i != e; ++i) {
      VMClass* interface = getClass(classFile_->getInterfaceIndex(i));
      interfaces_.push_back(interface);
      for (unsigned j = 0, f = interface->getNumInterfaces(); j != f; ++j)
        interfaces_.push_back(interface->getInterface(j));
    }

    // Sort the interfaces array and remove duplicates.
    std::sort(interfaces_.begin(), interfaces_.end());
    interfaces_.erase(std::unique(interfaces_.begin(), interfaces_.end()),
                      interfaces_.end());
  }

  // The interface index for an interface is a unique number generated
  // from the resolver.
  if (isInterface())
    interfaceIndex_ = resolver_->getNextInterfaceIndex();
  // For a class it is the max index of all the interfaces it implements.
  else {
    for (unsigned i = 0, e = getNumInterfaces(); i != e; ++i)
      interfaceIndex_ =
        std::max(interfaceIndex_, getInterface(i)->getInterfaceIndex());
  }

  computeLayout();
  computeClassRecord();

  //assert(!isa<OpaqueType>(getLayoutType()) &&"Class not initialized properly!");
}

llvm::Constant* VMClass::getConstant(unsigned index) 
{
  assert(classFile_ && "No constant pool!");
  /*assert((dynamic_cast<ConstantString*>(classFile_->getConstant(index)) ||
          dynamic_cast<ConstantInteger*>(classFile_->getConstant(index)) ||
          dynamic_cast<ConstantFloat*>(classFile_->getConstant(index)) ||
          dynamic_cast<ConstantLong*>(classFile_->getConstant(index)) ||
          dynamic_cast<ConstantDouble*>(classFile_->getConstant(index))) &&
         "Not an index to a constant!");*/

  // If we haven't resolved this constant already, do so now.
  if (!resolvedConstantPool_[index]) {
    Constant* jc = classFile_->getConstant(index);
    if (ConstantString *s = dyn_cast_or_null<ConstantString>(jc)) {
      VMClass* stringClass = resolver_->getClass("java/lang/String");
      Type* stringType = stringClass->getLayoutType();
      resolvedConstantPool_[index] =
        new GlobalVariable(*resolver_->getModule(),
                           stringType,
                           false,
                           GlobalVariable::LinkOnceAnyLinkage,
                           llvm::Constant::getNullValue(stringType),
                           s->getValue()->str() + ".java/lang/String");
    }
    else if (ConstantInteger* i = dyn_cast_or_null<ConstantInteger>(jc))
      resolvedConstantPool_[index] =
        ConstantInt::get(Type::Type::getInt32Ty(getGlobalContext()), i->getValue(), true);
    else if (ConstantFloat* f = dyn_cast_or_null<ConstantFloat>(jc))
      resolvedConstantPool_[index] =
        ConstantFP::get(Type::getFloatTy(ctx), f->getValue());
    else if (ConstantLong* l = dyn_cast_or_null<ConstantLong>(jc))
      resolvedConstantPool_[index] =
        ConstantInt::get(Type::getInt64Ty(ctx), l->getValue(), true);
    else if (ConstantDouble* d = dyn_cast_or_null<ConstantDouble>(jc))
      resolvedConstantPool_[index] =
        ConstantFP::get(Type::getDoubleTy(ctx), d->getValue());
    else
      assert(0 && "Not a constant!");
  }

  return static_cast<llvm::Constant*>(resolvedConstantPool_[index]);
}

VMClass* VMClass::getClass(unsigned index) 
{
  assert(classFile_ && "No constant pool!");
  /*assert((dynamic_cast<ConstantClass*>(classFile_->getConstant(index)) ||
          dynamic_cast<ConstantUtf8*>(classFile_->getConstant(index))) &&
         "Not an index to a class or descriptor reference!");*/

  // If we haven't resolved this constant already, do so now.
  if (!resolvedConstantPool_[index]) {
    Constant* jc = classFile_->getConstant(index);
    if (ConstantClass* c = dyn_cast<ConstantClass>(jc))
      resolvedConstantPool_[index] =
        const_cast<VMClass*>(resolver_->getClass(c->getName()->str()));
    else if (ConstantUtf8* d = dyn_cast<ConstantUtf8>(jc))
      resolvedConstantPool_[index] =
        const_cast<VMClass*>(resolver_->getClassForDesc(d->str()));
    else
      assert(0 && "Not a class!");
  }

  return static_cast<VMClass*>(resolvedConstantPool_[index]);
}

VMField* VMClass::getField(unsigned index) 
{
  assert(classFile_ && "No constant pool!");
  /*assert(dynamic_cast<ConstantFieldRef*>(classFile_->getConstant(index)) &&
         "Not an index to a field reference!");*/

  // If we haven't resolved this constant already, do so now.
  if (!resolvedConstantPool_[index]) {
    ConstantFieldRef* jc = classFile_->getConstantFieldRef(index);
    VMClass* clazz = getClass(jc->getClassIndex());
    std::string name = jc->getNameAndType()->getName()->str();
    resolvedConstantPool_[index] =
      const_cast<VMField*>(clazz->lookupField(name));
  }

  return static_cast<VMField*>(resolvedConstantPool_[index]);
}

VMMethod* VMClass::getMethod(unsigned index) 
{
  assert(classFile_ && "No constant pool!");
  /*assert((dynamic_cast<ConstantMethodRef*>(classFile_->getConstant(index)) ||
          dynamic_cast<ConstantInterfaceMethodRef*>(classFile_->getConstant(index))) &&
         "Not an index to a method reference!");*/

  // If we haven't resolved this constant already, do so now.
  if (!resolvedConstantPool_[index]) {
    ConstantMemberRef* jc = classFile_->getConstantMemberRef(index);
    VMClass* clazz = getClass(jc->getClassIndex());
    ConstantNameAndType* ntc = jc->getNameAndType();
    std::string name = ntc->getName()->str();
    std::string descriptor = ntc->getDescriptor()->str();
    resolvedConstantPool_[index] =
      const_cast<VMMethod*>(clazz->lookupMethod(name + descriptor));
  }

  return static_cast<VMMethod*>(resolvedConstantPool_[index]);
}
