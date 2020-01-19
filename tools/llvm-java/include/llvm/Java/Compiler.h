//===-- Compiler.h - Java bytecode compiler ---------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains Java bytecode to LLVM bytecode compiler.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_JAVA_COMPILER_H
#define LLVM_JAVA_COMPILER_H

#include <llvm/IR/Module.h>
#include <memory>

namespace llvm { namespace Java {

  Module* compile(const std::string className);

} } // namespace llvm::Java

#endif//LLVM_JAVA_COMPILER_H
