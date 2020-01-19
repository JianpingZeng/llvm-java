//===-- VMClass.h - Compiler representation of a Java class -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the VMClass class that represents a
// compile time representation of a Java class (java.lang.Class).
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_JAVA_VMCLASS_H
#define LLVM_JAVA_VMCLASS_H

#include "VMField.h"
#include "VMMethod.h"
#include <llvm/IR/Constant.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Java/ClassFile.h>
#include <map>
#include <string>
#include <vector>

namespace llvm { namespace Java {

  class Resolver;

  class VMClass {
    static int INVALID_INTERFACE_INDEX;

    std::string name_;
    std::string descriptor_;
    Resolver* resolver_;
    ClassFile* classFile_;
    VMClass* componentClass_;
    StructType* layoutType_;
    Type* type_;
    int interfaceIndex_;
    typedef std::map<std::string, VMField> FieldMap;
    FieldMap fieldMap_;
    typedef std::map<std::string, VMMethod> MethodMap;
    MethodMap methodMap_;
    mutable std::vector<void*> resolvedConstantPool_;
    std::vector<VMClass*> superClasses_;
    std::vector<VMClass*> interfaces_;
    std::vector<VMField*> memberFields_;
    std::vector<VMField*> staticFields_;
    std::vector<VMMethod*> dynamicMethods_;
    std::vector<VMMethod*> staticMethods_;
    GlobalVariable* classRecord_;
    LLVMContext &ctx;

    void init();

    void computeLayout();
    void computeClassRecord();

    llvm::Constant* buildClassName() ;
    llvm::Constant* buildSuperClassRecords() ;
    llvm::Constant* buildInterfaceClassRecord(VMClass* interface) ;
    llvm::Constant* buildInterfaceClassRecords() ;
    llvm::Constant* buildFieldDescriptors() ;
    llvm::Constant* buildFieldOffsets() ;
    llvm::Constant* buildStaticFieldDescriptors() ;
    llvm::Constant* buildStaticFieldPointers() ;
    llvm::Constant* buildMethodDescriptors() ;
    llvm::Constant* buildMethodPointers() ;
    llvm::Constant* buildStaticMethodDescriptors() ;
    llvm::Constant* buildStaticMethodPointers() ;
    llvm::Constant* buildClassTypeInfo() ;

    VMField* lookupField(std::string name) ;
    VMMethod* lookupMethod(std::string nameAndType) ;
    
    friend class Resolver;

    // Resolver interface.

    // Load primitive class for type.
    VMClass(Resolver* resolver, Type* type, LLVMContext &);

    // Load class by name.
    VMClass(Resolver* resolver, std::string className, LLVMContext &);

    // Load array class of component the passed class.
    VMClass(Resolver* resolver, VMClass* componentClass, LLVMContext &);

    // Link the class.
    void link();
    // Resolve the class.
    void resolve();
    // Initialize the class.
    void initialize();

  public:
    std::string getName() { return name_; }
    std::string getDescriptor() { return descriptor_; }
    Resolver* getResolver() { return resolver_; }
    StructType* getLayoutType() { return layoutType_; }
    Type* getType() { return type_; }
    ClassFile* getClassFile() { return classFile_; }
    unsigned getNumSuperClasses() { return superClasses_.size(); }
    VMClass* getSuperClass(unsigned i) { return superClasses_[i]; }
    VMClass* getSuperClass() {
      return getNumSuperClasses() ? getSuperClass(0) : NULL;
    }
    unsigned getNumInterfaces() { return interfaces_.size(); }
    VMClass* getInterface(unsigned i) { return interfaces_[i]; }
    VMClass* getComponentClass() { return componentClass_; }
    bool isArray() { return getComponentClass(); }
    bool isPrimitive() { return getType() == getLayoutType(); }
    bool isInterface() { return classFile_ && classFile_->isInterface(); }
    int getInterfaceIndex() { return interfaceIndex_; }
    unsigned getNumDynamicMethods() {
      return dynamicMethods_.size();
    }
    VMMethod* getDynamicMethod(unsigned i) {
      return dynamicMethods_[i];
    }
    unsigned getNumStaticMethods() {
      return staticMethods_.size();
    }
    VMMethod* getStaticMethod(unsigned i) {
      return staticMethods_[i];
    }
    llvm::Constant* getClassRecord() { return classRecord_; }

    llvm::Constant* getConstant(unsigned index) ;
    VMClass* getClass(unsigned index) ;
    VMField* getField(unsigned index) ;
    VMField* getField(std::string name) {
      FieldMap::iterator it = fieldMap_.find(name);
      return it == fieldMap_.end() ? NULL : &it->second;
    }
    VMMethod* getMethod(unsigned index) ;
    VMMethod* getMethod(std::string nameAndType) {
      MethodMap::iterator it = methodMap_.find(nameAndType);
      return it == methodMap_.end() ? NULL : &it->second;
    }
  };

} } // namespace llvm::Java

#endif//LLVM_JAVA_VMCLASS_H
