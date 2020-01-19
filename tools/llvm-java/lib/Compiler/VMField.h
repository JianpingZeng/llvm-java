//===-- VMField.h - Compiler representation of a Java field -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the Field class that represents a
// compile time representation of a Java class field (java.lang.Field).
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_JAVA_VMFIELD_H
#define LLVM_JAVA_VMFIELD_H

#include <llvm/Java/ClassFile.h>
#include <cassert>

namespace llvm {

  class Constant;
  class GlobalVariable;

}

namespace llvm { namespace Java {

  class VMClass;

  class VMField {
     VMClass* parent_;
     VMClass* class_;
     Field* field_;
    union {
      int index;
      GlobalVariable* global;
    } data_;

    friend class VMClass;
    // Interface for VMClass.

    // Create static field reference.
    VMField( VMClass* parent,  Field* field);

    // Create member field reference.
    VMField( VMClass* parent,  Field* field, int index);

  public:
     std::string getName()  { return field_->getName()->str(); }
     std::string getDescriptor()  {
      return field_->getDescriptor()->str();
    }
    bool isStatic()  { return field_->isStatic(); }

     VMClass* getParent()  { return parent_; }
     VMClass* getClass()  { return class_; }
    int getMemberIndex()  {
      assert(!isStatic() && "Field should not be static!");
      return data_.index;
    }
    GlobalVariable* getGlobal()  {
      assert(isStatic() && "Field should be static!");
      return data_.global;
    }

    llvm::Constant* buildFieldDescriptor() ;
    llvm::Constant* buildFieldOffset() ;
  };

} } // namespace llvm::Java

#endif//LLVM_JAVA_VMFIELD_H
