//===-- VMMethod.h - Compiler representation of a Java method ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the Method class that represents a
// compile time representation of a Java class method (java.lang.Method).
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_JAVA_VMMETHOD_H
#define LLVM_JAVA_VMMETHOD_H

#include <llvm/Java/ClassFile.h>

namespace llvm {

  class Constant;
  class Function;
  class FunctionType;

}

namespace llvm { namespace Java {

  class VMClass;

  class VMMethod {
     VMClass* parent_;
     Method* method_;
    Function* function_;
    Function* bridgeFunction_;

    int index_;

    void init();

    friend class VMClass;
    // Interface for VMClass.

    // Create statically bound method reference.
    VMMethod( VMClass* parent,  Method* method);

    // Create dynamically bound method reference.
    VMMethod( VMClass* parent,  Method* method, int index);

  public:
    VMClass* getParent()  { return parent_; }
    Method* getMethod()  { return method_; }
    Function* getFunction()  { return function_; }
    int getMethodIndex()  { return index_; }

    bool isStaticallyBound()  {
      return isStatic() || isPrivate() || getName()[0] == '<';
    }
    bool isDynamicallyBound()  { return !isStaticallyBound(); }
    bool isAbstract()  { return method_->isAbstract(); }
    bool isNative()  { return method_->isNative(); }
    bool isPrivate()  { return method_->isPrivate(); }
    bool isStatic()  { return method_->isStatic(); }

    // FIXME: remove when transition is complete.
     std::string getName()  { return method_->getName()->str(); }
     std::string getDescriptor()  {
      return method_->getDescriptor()->str();
    }
    std::string getNameAndDescriptor()  {
      return getName() + getDescriptor();
    }

    llvm::Constant* buildMethodDescriptor() ;
    llvm::Constant* getBridgeFunction() ;
  };

} } // namespace llvm::Java

#endif//LLVM_JAVA_VMMETHOD_H
