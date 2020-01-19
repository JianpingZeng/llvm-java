//===-- Resolver.h - Class resolver for Java classes ------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of a Java class resolver. This
// object creates Class objects out of loaded ClassFiles.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_JAVA_RESOLVER_H
#define LLVM_JAVA_RESOLVER_H

#include "VMClass.h"
#include <llvm/Java/Bytecode.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <map>
#include <string>

namespace llvm { namespace Java {

  class Resolver {
    Module* module_;
    typedef std::map<std::string, VMClass> ClassMap;
    ClassMap classMap_;
    unsigned nextInterfaceIndex_;
    StructType* objectBaseLayoutType_;
    Type* objectBaseType_;
    StructType* typeInfoType_;
    StructType* classRecordType_;
    Type* classRecordPtrType_;

    VMClass* getClassForDesc(std::string descriptor);

    Type* getTypeHelper(std::string&,
                              unsigned& i,
                              bool memberMethod = false) ;

    ClassMap::iterator insertClass(ClassMap::iterator i, VMClass clazz) {
      return classMap_.insert(i, std::make_pair(clazz.getDescriptor(), clazz));
    }

    friend class VMClass;

  public:
    static std::string canonicalizeClassName(std::string& className) {
      if (className[0] == '[')
        return className;
      else
        return 'L' + className + ';';
    }

    Resolver(Module* module);

    Type* getObjectBaseLayoutType() {return objectBaseLayoutType_; }
    Type* getObjectBaseType() { return objectBaseType_; }
    Type* getTypeInfoType() { return typeInfoType_; }
    Type* getClassRecordType() { return classRecordType_; }
    Type* getClassRecordPtrType() { return classRecordPtrType_; }

    Type* getType(std::string& descriptor,
                        bool memberMethod = false) ;
    Type* getStorageType(Type* type) ;

    inline bool isTwoSlotType(Type* type) {
      return type->isIntegerTy(64) || type->isDoubleTy();
    }

    inline bool isOneSlotType(Type* type) {
      return !isTwoSlotType(type);
    }

    VMClass* getClass(std::string className) {
      return getClassForDesc(canonicalizeClassName(className));
    }

    VMClass* getClass(JType type);

    VMClass* getArrayClass(VMClass* clazz) {
      return getClassForDesc('[' + clazz->getDescriptor());
    }

    unsigned getNextInterfaceIndex() { return nextInterfaceIndex_++; }
    Module* getModule() { return module_; }
    void emitClassRecordsArray() ;
  };

} } // namespace llvm::Java

#endif//LLVM_JAVA_RESOLVER_H
