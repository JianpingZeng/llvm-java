//===-- class2llvm.cpp - class2llvm utility ---------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is a sample class reader driver. It is used to drive class
// reader tests.
//
//===----------------------------------------------------------------------===//

#include <llvm/Java/ClassFile.h>
#include <llvm/Java/Compiler.h>
#include <llvm/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Signals.h>

#include <cstddef>
#include <fstream>
#include <iostream>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/ToolOutputFile.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Bitcode/ReaderWriter.h>

using namespace llvm;

static cl::opt<std::string>
InputClass(cl::Positional, cl::desc("<input class>"));

static cl::opt<std::string>
OutputFilename("o", cl::desc("Output filename"), cl::value_desc("filename"));

int main(int argc, char* argv[])
{
  sys::PrintStackTraceOnErrorSignal();
  cl::ParseCommandLineOptions(argc, argv,
                              "java .class -> llvm .ll utility");

  Module* module = Java::compile(InputClass);
  SMDiagnostic Err;
  if (!module) {
    Err.print(argv[0], errs());
    return 1;
  }

  // Infer the output filename if needed.
  if (OutputFilename.empty()) {
    if (InputClass == "-") {
      OutputFilename = "-";
    } else {
      OutputFilename = InputClass + ".ll";
    }
  }

  std::error_code EC;
  std::unique_ptr<tool_output_file> Out(
      new tool_output_file(OutputFilename, EC, sys::fs::F_None));
  if (EC) {
    errs() << EC.message() << '\n';
    exit(1);
  }

  PassManager passes;
  passes.add(createVerifierPass());
  passes.run(*module);

  WriteBitcodeToFile(module, Out->os());
  // Declare success.
  Out->keep();
  return 0;
}
