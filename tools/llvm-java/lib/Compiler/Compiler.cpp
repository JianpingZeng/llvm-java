//===-- Compiler.cpp - Java bytecode compiler -------------------*- C++ -*-===//
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

#define DEBUG_TYPE "javacompiler"

#include <llvm/Java/Compiler.h>
#include "BasicBlockBuilder.h"
#include "Locals.h"
#include "OperandStack.h"
#include "Resolver.h"
#include <llvm/Java/Bytecode.h>
#include <llvm/Java/BytecodeParser.h>
#include <llvm/Java/ClassFile.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/SetVector.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/Support/Debug.h>
#include <iostream>
#include <list>
#include <vector>
#include <llvm/IR/CFG.h>

using namespace llvm;
using namespace llvm::Java;

namespace llvm { namespace Java { namespace {

   std::string TMP("tmp");

  llvm::Constant* ONE = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 1);
  llvm::Constant* ZERO = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0);
  llvm::Constant* MINUS_ONE = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), -1);

  llvm::Constant* INT_SHIFT_MASK = ConstantInt::get(Type::getInt8Ty(getGlobalContext()), 0x1f);
  llvm::Constant* LONG_SHIFT_MASK = ConstantInt::get(Type::getInt8Ty(getGlobalContext()), 0x3f);

  class Compiler : public BytecodeParser<Compiler> {
    Module* module_;
    Resolver* resolver_;
    GlobalVariable* JNIEnvPtr_;
    VMClass* class_;
    BasicBlockBuilder* bbBuilder_;
    std::list<BasicBlock*> bbWorkList_;
    typedef std::map<BasicBlock*, unsigned> OpStackDepthMap;
    OpStackDepthMap opStackDepthMap_;
    typedef std::map<std::string, GlobalVariable*> StringMap;
    StringMap stringMap_;
    BasicBlock* currentBB_;
    Locals locals_;
    OperandStack opStack_;
    Function *getClassRecord_, *setClassRecord_, *throw_, *isInstanceOf_,
      *memcpy_, *memset_;
    std::vector<llvm::Constant*> classInitializers_;

    SetVector< VMMethod*> toCompileMethods_;

  public:
    Compiler(Module* m)
      : module_(m),
        resolver_(new Resolver(module_)),
        locals_(resolver_, 0),
        opStack_(resolver_, 0) {
      StructType* JNIEnvTy = StructType::get(module_->getContext(), false);
      JNIEnvTy->setName("JNIEnv");
      JNIEnvPtr_ = new GlobalVariable(*module_, JNIEnvTy,
                                      true,
                                      GlobalVariable::ExternalLinkage,
                                      nullptr,
                                      "llvm_java_JNIEnv");
      Type* classRecordPtrType = resolver_->getClassRecordPtrType();
      Type* objectBaseType = resolver_->getObjectBaseType();

      getClassRecord_ = dyn_cast<Function>(module_->getOrInsertFunction(
        "llvm_java_get_class_record", classRecordPtrType,
        objectBaseType, NULL));
      setClassRecord_ = dyn_cast<Function>(module_->getOrInsertFunction(
        "llvm_java_set_class_record", Type::getVoidTy(getGlobalContext()),
        objectBaseType, classRecordPtrType, NULL));
      throw_ = dyn_cast<Function>(module_->getOrInsertFunction(
        "llvm_java_throw", Type::getInt32Ty(getGlobalContext()),
        objectBaseType, NULL));
      isInstanceOf_ = dyn_cast<Function>(module_->getOrInsertFunction(
        "llvm_java_is_instance_of", Type::getInt32Ty(getGlobalContext()),
        objectBaseType, classRecordPtrType, NULL));
      memcpy_ = dyn_cast<Function>(module_->getOrInsertFunction(
        "llvm.memcpy", Type::getVoidTy(getGlobalContext()),
        PointerType::get(Type::getInt8Ty(getGlobalContext()), 0),
        PointerType::get(Type::getInt8Ty(getGlobalContext()), 0),
        Type::getInt64Ty(getGlobalContext()), Type::getInt32Ty(getGlobalContext()), NULL));
      memset_ = dyn_cast<Function>(module_->getOrInsertFunction(
        "llvm.memset", Type::getVoidTy(getGlobalContext()),
        PointerType::get(Type::getInt8Ty(getGlobalContext()), 0),
        Type::getInt8Ty(getGlobalContext()), Type::getInt64Ty(getGlobalContext()), Type::getInt32Ty(getGlobalContext()), NULL));
    }

  private:
    void push(Value* value) {
      opStack_.push(value, currentBB_);
    }

    Value* pop( Type* type) {
      return opStack_.pop(type, currentBB_);
    }

    /// Schedule a method for compilation. Returns true if this is the
    /// first time this function was scheduled.
    bool scheduleMethod( VMMethod* method) {
      if (toCompileMethods_.insert(method)) {
        DEBUG(std::cerr << "Scheduling function: "
              << method->getFunction()->getName().str() << " for compilation\n");
        return true;
      }
      return false;
    }

    template <typename InsertionPointTy>
    void initializeString(Value* globalString,
                           std::string str,
                          InsertionPointTy* ip) {
      // Create a new byte[] object and initialize it with the
      // contents of this string ant.
      LLVMContext &ctx = getGlobalContext();
      Value* count = ConstantInt::get(Type::getInt32Ty(ctx), str.size());
      Value* arrayRef = allocateArray(resolver_->getClass("[B"), count, ip);
      // Copy string data.
      std::vector<Value*> indices;
      indices.reserve(3);
      indices.push_back(ConstantInt::get(Type::getInt32Ty(ctx), 0));
      indices.push_back(ConstantInt::get(Type::getInt32Ty(ctx), 2));
      indices.push_back(ConstantInt::get(Type::getInt32Ty(ctx), 0));
      Value* arrayData = GetElementPtrInst::Create(arrayRef, indices, TMP, ip);
      llvm::Constant* init = ConstantDataArray::getString(ctx, str);
      GlobalVariable* chars = new GlobalVariable(
        *module_,
        init->getType(),
        true,
        GlobalVariable::InternalLinkage,
        init,
        str + ".str");

      std::vector<Value*> params;
      params.reserve(4);
      params.clear();
      params.push_back(arrayData);
      params.push_back(chars);
      params.push_back(CastInst::CreateZExtOrBitCast(count, Type::getInt64Ty(ctx), TMP, ip));
      params.push_back(ConstantInt::get(Type::getInt32Ty(ctx), 0));
      CallInst::Create(memcpy_, params, "", ip);

      // Get class information for java/lang/String.
       VMClass* clazz = resolver_->getClass("java/lang/String");
      emitClassInitializers(clazz);

      // Install the class record.
      Value* objBase =
        CastInst::CreateBitOrPointerCast(globalString, resolver_->getObjectBaseType(), TMP, ip);
       Type* classRecordPtrType = resolver_->getClassRecordPtrType();
      Value* classRecord =
          CastInst::CreateBitOrPointerCast(clazz->getClassRecord(), classRecordPtrType, TMP, ip);
      Value* args[] = {objBase, classRecord};
      CallInst::Create(setClassRecord_, args, "", ip);

      // Initialize it: call java/lang/String/<init>(byte[],int)
       VMMethod* method = clazz->getMethod("<init>([BI)V");

      params.reserve(3);
      params.clear();
      params.push_back(objBase);
      params.push_back(
          CastInst::CreateBitOrPointerCast(arrayRef, resolver_->getObjectBaseType(), TMP, ip));
      params.push_back(ConstantInt::get(Type::getInt32Ty(ctx), 0));
      CallInst::Create(method->getFunction(), params, "", ip);
    }

    /// Returns the type of the Java string descriptor for JNI.
     Type* getJNIType(std::string descr) {
      unsigned i = 0;
      return getJNITypeHelper(descr, i);
    }

    Type* getJNITypeHelper(std::string descr, unsigned& i) {
      assert(i < descr.size());
      switch (descr[i++]) {
      case 'B': return Type::getInt8Ty(getGlobalContext());
      case 'C': return Type::getInt16Ty(getGlobalContext());
      case 'D': return Type::getDoubleTy(getGlobalContext());
      case 'F': return Type::getFloatTy(getGlobalContext());
      case 'I': return Type::getInt32Ty(getGlobalContext());
      case 'J': return Type::getInt64Ty(getGlobalContext());
      case 'S': return Type::getInt16Ty(getGlobalContext());
      case 'Z': return Type::getInt1Ty(getGlobalContext());
      case 'V': return Type::getVoidTy(getGlobalContext());
        // Both array and object types are pointers to llvm_object_base
      case 'L': {
        unsigned e = descr.find(';', i);
        i = e + 1;
        return resolver_->getObjectBaseType();
      }
      case '[':
        // Skip '['s.
        if (descr[i] == '[')
          do { ++i; } while (descr[i] == '[');
        // Consume the element type
        getJNITypeHelper(descr, i);
        return resolver_->getObjectBaseType();
      case '(': {
        SmallVector<Type*, 8> params;
        // JNIEnv*
        params.push_back(JNIEnvPtr_->getType());
        params.push_back(resolver_->getObjectBaseType());
        while (descr[i] != ')')
          params.push_back(getJNITypeHelper(descr, i));
        return FunctionType::get(getJNITypeHelper(descr, ++i), params, false);
      }
        // FIXME: Throw something
      default:  assert(0 && "Cannot parse type descriptor!");
      }
      return 0; // not reached
    }

    std::string getMangledString( std::string str) {
      std::string mangledStr;

      for (unsigned i = 0, e = str.size(); i != e; ++i) {
        if (str[i] == '/')
          mangledStr += '_';
        else if (str[i] == '_')
          mangledStr += "_1";
        else if (str[i] == ';')
          mangledStr += "_2";
        else if (str[i] == '[')
          mangledStr += "_3";
        else
          mangledStr += str[i];
      }
      return mangledStr;
    }

    /// Compiles the passed method only (it does not compile any
    /// callers or methods of objects it creates).
    void compileMethodOnly( VMMethod* method) {
      class_ = method->getParent();

      Function* function = method->getFunction();
      if (!function->empty()) {
        DEBUG(std::cerr << "Function: " << function->getName().str()
              << " is already compiled!\n");
        return;
      }

      if (method->isNative()) {
        DEBUG(std::cerr << "Adding stub for natively implemented method: "
              << function->getName().str() << '\n');
        FunctionType* jniFuncTy =
          dyn_cast<FunctionType>(getJNIType(method->getDescriptor()));

        std::string funcName =
          "Java_" +
          getMangledString(class_->getName()) + '_' +
          getMangledString(method->getName());
        if (class_->getClassFile()->isNativeMethodOverloaded(*method->getMethod())) {
          // We need to add two underscores and a mangled argument signature
          funcName += "__";
           std::string descr = method->getDescriptor();
          funcName += getMangledString(
            std::string(descr.begin() + descr.find('(') + 1,
                        descr.begin() + descr.find(')')));
        }

        Function* jniFunction =
          dyn_cast<Function>(module_->getOrInsertFunction(funcName, jniFuncTy));
        LLVMContext &ctx = function->getContext();
        BasicBlock* bb = BasicBlock::Create(ctx, "entry", function);
        SmallVector<Value*, 8> params;
        params.push_back(JNIEnvPtr_);
        if (method->isStatic())
          params.push_back(
            CastInst::CreatePointerCast(method->getParent()->getClassRecord(),
                                             resolver_->getObjectBaseType(), TMP, bb));
        for (Function::arg_iterator A = function->arg_begin(),
               E = function->arg_end(); A != E; ++A) {
          params.push_back(
              CastInst::CreatePointerCast(A, jniFuncTy->getParamType(params.size()), TMP, bb));
        }
        Value* result = CallInst::Create(jniFunction, params, "", bb);
        if (result->getType() != Type::getVoidTy(getGlobalContext()))
          result = CastInst::CreatePointerCast(result, function->getReturnType(), TMP,bb);
        ReturnInst::Create(ctx, result, bb);
        return;
      }

      assert (!method->isAbstract() && "Trying to compile an abstract method!");

      // HACK: skip most of the class libraries.
       std::string funcName = function->getName();
      if ((funcName.find("java/") == 0 &&
           funcName.find("java/lang/Object") != 0 &&
           (funcName.find("java/lang/Throwable") != 0 ||
            funcName.find("java/lang/Throwable$StaticData/<cl") == 0) &&
           funcName.find("java/lang/Exception") != 0 &&
           funcName.find("java/lang/IllegalArgumentException") != 0 &&
           funcName.find("java/lang/IllegalStateException") != 0 &&
           funcName.find("java/lang/IndexOutOfBoundsException") != 0 &&
           funcName.find("java/lang/RuntimeException") != 0 &&
           (funcName.find("java/lang/Math") != 0 ||
            funcName.find("java/lang/Math/<cl") == 0) &&
           funcName.find("java/lang/Number") != 0 &&
           funcName.find("java/lang/Byte") != 0 &&
           funcName.find("java/lang/Float") != 0 &&
           funcName.find("java/lang/Integer") != 0 &&
           funcName.find("java/lang/Long") != 0 &&
           funcName.find("java/lang/Short") != 0 &&
           (funcName.find("java/lang/String") != 0 ||
            funcName.find("java/lang/String/<cl") == 0) &&
           funcName.find("java/lang/StringBuffer") != 0 &&
           (funcName.find("java/lang/System") != 0 ||
            funcName.find("java/lang/System/loadLibrary") == 0) &&
           funcName.find("java/lang/VMSystem") != 0 &&
           (funcName.find("java/util/") != 0 ||
            funcName.find("java/util/Locale/<cl") == 0 ||
            funcName.find("java/util/ResourceBundle/<cl") == 0 ||
            funcName.find("java/util/Calendar/<cl") == 0) ||
            funcName.find("java/util/PropertyPermission/<cl") == 0) ||
          (funcName.find("gnu/") == 0)) {
        DEBUG(std::cerr << "Skipping compilation of method: "
              << funcName << '\n');
        return;
      }

      DEBUG(std::cerr << "Compiling method: " << funcName << '\n');

      Java::CodeAttribute* codeAttr = method->getMethod()->getCodeAttribute();

      opStackDepthMap_.clear();
      bbBuilder_ = new BasicBlockBuilder(function, codeAttr);

      // Put arguments into locals.
      locals_ = Locals(resolver_, codeAttr->getMaxLocals());

      unsigned index = 0;
      for (Function::arg_iterator a = function->arg_begin(),
             ae = function->arg_end(); a != ae; ++a) {
        locals_.store(index, a, &function->getEntryBlock());
        index += resolver_->isTwoSlotType(a->getType()) ? 2 : 1;
      }

      BasicBlock* bb0 = bbBuilder_->getBasicBlock(0);

      // For bb0 the operand stack is empty and the locals contain the
      // arguments to the function.
      //
      // NOTE: We create an operand stack one size too big because we
      // push extra values on the stack to simplify code generation
      // (see implementation of ifne).
      opStack_ = OperandStack(resolver_, codeAttr->getMaxStack()+2);
      opStackDepthMap_.insert(std::make_pair(bb0, 0));

      // Insert bb0 in the work list.
      bbWorkList_.push_back(bb0);

      // Process the work list until we compile the whole function.
      while (!bbWorkList_.empty()) {
        currentBB_ = bbWorkList_.front();
        bbWorkList_.pop_front();

        OpStackDepthMap::iterator opStackDepth =
          opStackDepthMap_.find(currentBB_);
        assert(opStackDepth != opStackDepthMap_.end() &&
               "Unknown operand stack depth for basic block in work list!");

        opStack_.setDepth(opStackDepth->second);

        unsigned start, end;
        std::tie(start, end) = bbBuilder_->getBytecodeIndices(currentBB_);

        // Compile this basic block.
        parse(codeAttr->getCode(), start, end);

        // If this basic block does not have a terminator, it should
        // have an unconditional branch to the next basic block
        // (fallthrough).
        if (!currentBB_->getTerminator())
          BranchInst::Create(bbBuilder_->getBasicBlock(end), currentBB_);

        // For each successor of this basic block we can compute its
        // entry operand stack depth. We do so, and add it to the work
        // list. If a successor already has an entry operand stack and
        // locals we assume the computation was correct and do not add
        // it to the work list.
        for (succ_iterator
               SI = succ_begin(currentBB_), SE = succ_end(currentBB_);
             SI != SE; ++SI) {
          BasicBlock* Succ = *SI;
          OpStackDepthMap::iterator succOpStackDepth =
            opStackDepthMap_.lower_bound(Succ);
          if (succOpStackDepth == opStackDepthMap_.end() ||
              succOpStackDepth->first != Succ) {
            opStackDepthMap_.insert(succOpStackDepth,
                                    std::make_pair(Succ, opStack_.getDepth()));
            bbWorkList_.push_back(Succ);
          }
        }
      }

      // Add an unconditional branch from the entry block to bb0.
      BranchInst::Create(bb0, &function->getEntryBlock());

      // FIXME: remove empty basic blocks (we have empty basic blocks
      // because of our lack of exception support).
      for (Function::iterator bb = function->begin(), be = function->end();
           bb != be; )
        if (bb->empty())
          bb = function->getBasicBlockList().erase(bb);
        else
          ++bb;

      DEBUG(std::cerr << "Finished compilation of method: "<< funcName << '\n');
      // DEBUG(function->dump());
    }

    /// Emits static initializers for this class if not done already.
    void emitClassInitializers( VMClass* clazz) {
      static SetVector< VMClass*> toInitClasses;

      // If this is a primitive class we are done.
      if (clazz->isPrimitive())
        return;

      // If this class is already initialized, we are done.
      if (!toInitClasses.insert(clazz))
        return;

      // If this class has a super class, initialize that first.
      if ( VMClass* superClass = clazz->getSuperClass())
        emitClassInitializers(superClass);

      // If this class is an array, initialize its component class now.
      if ( VMClass* componentClass = clazz->getComponentClass())
        emitClassInitializers(componentClass);

      // Schedule all its dynamically bound non abstract methods for
      // compilation.
      for (unsigned i = 0, e = clazz->getNumDynamicMethods(); i != e; ++i) {
         VMMethod* method = clazz->getDynamicMethod(i);
        if (!method->isAbstract())
          scheduleMethod(method);
      }

      // Schedule all its statically bound non abstract methods for
      // compilation.
      for (unsigned i = 0, e = clazz->getNumStaticMethods(); i != e; ++i) {
         VMMethod* method = clazz->getStaticMethod(i);
        if (!method->isAbstract())
          scheduleMethod(method);
      }
      
      // If this class has a ant pool (was loaded from a
      // classfile), create ant strings for it.
      LLVMContext &ctx = getGlobalContext();
      if ( ClassFile* classfile = clazz->getClassFile()) {
        Function* stringConstructors = dyn_cast<Function>(module_->getOrInsertFunction(
          clazz->getName() + "<strinit>",
          FunctionType::get(Type::getVoidTy(getGlobalContext()), SmallVector<Type*, 4>(), false)));
        Instruction* I =
          ReturnInst::Create(ctx, NULL, BasicBlock::Create(ctx, "entry", stringConstructors));
        for (unsigned i = 0, e = classfile->getNumConstants(); i != e; ++i)
          if (ConstantString* s = dyn_cast_or_null<ConstantString>(classfile->getConstant(i)))
            initializeString(clazz->getConstant(i), s->getValue()->str(), I);

        // Insert string ructors method in class initializers array.
        classInitializers_.push_back(stringConstructors);

        // Call its class initialization method if it exists.
        if ( VMMethod* method = clazz->getMethod("<clinit>()V"))
          classInitializers_.push_back(method->getFunction());
      }
    }

  public:
    /// Compiles the specified method given a <class,method>
    /// descriptor and the transitive closure of all methods
    /// (possibly) called by it.
     VMMethod* compileMethod(std::string className,
                                  std::string methodDesc) {
      // Load the class.
       VMClass* clazz = resolver_->getClass(className);
      emitClassInitializers(clazz);

      // Find the method.
       VMMethod* method = clazz->getMethod(methodDesc);
      // Compile the transitive closure of methods called by this method.
      for (unsigned i = 0; i != toCompileMethods_.size(); ++i) {
         VMMethod* m = toCompileMethods_[i];
        compileMethodOnly(m);
        DEBUG(std::cerr << i+1 << '/' << toCompileMethods_.size()
              << " functions compiled\n");
      }

      // Null terminate the static initializers array and add the
      // global to the module.
      Type* classInitializerType = PointerType::get(
          FunctionType::get(Type::getVoidTy(getGlobalContext()), SmallVector<Type*, 4>(), false), 0);
      classInitializers_.push_back(
        llvm::Constant::getNullValue(classInitializerType));

      ArrayType* classInitializersType =
        ArrayType::get(classInitializerType, classInitializers_.size());
      new GlobalVariable(*module_, classInitializersType,
                         true,
                         GlobalVariable::ExternalLinkage,
                         ConstantArray::get(classInitializersType,
                                            classInitializers_),
                         "llvm_java_class_initializers");

      // Emit the array of all class records.
      resolver_->emitClassRecordsArray();

      return method;
    }

    void do_a_null() {
      push(llvm::Constant::getNullValue(resolver_->getObjectBaseType()));
    }

    void do_i(int value) {
      push(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), value));
    }

    void do_l(long long value) {
      push(ConstantInt::get(Type::getInt64Ty(getGlobalContext()), value));
    }

    void do_f(float value) {
      push(ConstantFP::get(Type::getFloatTy(getGlobalContext()), value));
    }

    void do_d(double value) {
      push(ConstantFP::get(Type::getDoubleTy(getGlobalContext()), value));
    }

    void do_ldc(unsigned index) {
      llvm::Constant* c = class_->getConstant(index);
      assert(c && "Java ant not handled!");
      push(c);
    }

    void do_ldc2(unsigned index) {
      do_ldc(index);
    }

    void do_iload(unsigned index) { do_load_common(index, Type::getInt32Ty(getGlobalContext())); }
    void do_lload(unsigned index) { do_load_common(index, Type::getInt64Ty(getGlobalContext())); }
    void do_fload(unsigned index) { do_load_common(index, Type::getFloatTy(getGlobalContext())); }
    void do_dload(unsigned index) { do_load_common(index, Type::getDoubleTy(getGlobalContext())); }
    void do_aload(unsigned index) { do_load_common(index, resolver_->getObjectBaseType()); }

    void do_load_common(unsigned index,  Type* type) {
      Value* val = locals_.load(index, type, currentBB_);
      push(val);
    }

    void do_iaload() { do_aload_common("[I"); }
    void do_laload() { do_aload_common("[J"); }
    void do_faload() { do_aload_common("[F"); }
    void do_daload() { do_aload_common("[D"); }
    void do_aaload() { do_aload_common("[Ljava/lang/Object;"); }
    void do_baload() { do_aload_common("[B"); }
    void do_caload() { do_aload_common("[C"); }
    void do_saload() { do_aload_common("[S"); }

    void do_aload_common(std::string className) {
       VMClass* arrayClass = resolver_->getClass(className);
      assert(arrayClass->isArray() && "Not an array class!");
      Value* index = pop(Type::getInt32Ty(getGlobalContext()));
      Value* arrayRef = pop(arrayClass->getType());

      std::vector<Value*> indices;
      indices.reserve(3);
      indices.push_back(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0));
      indices.push_back(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 2));
      indices.push_back(index);
      Value* elementPtr =
        GetElementPtrInst::Create(arrayRef, indices, TMP, currentBB_);
      Value* result = new LoadInst(elementPtr, TMP, currentBB_);
      push(result);
    }

    void do_istore(unsigned index) { do_store_common(index, Type::getInt32Ty(getGlobalContext())); }
    void do_lstore(unsigned index) { do_store_common(index, Type::getInt64Ty(getGlobalContext())); }
    void do_fstore(unsigned index) { do_store_common(index, Type::getFloatTy(getGlobalContext())); }
    void do_dstore(unsigned index) { do_store_common(index, Type::getDoubleTy(getGlobalContext())); }
    void do_astore(unsigned index) { do_store_common(index, resolver_->getObjectBaseType()); }

    void do_store_common(unsigned index,  Type* type) {
      Value* val = pop(type);
      locals_.store(index, val, currentBB_);
    }

    void do_iastore() { do_astore_common("[I"); }
    void do_lastore() { do_astore_common("[J"); }
    void do_fastore() { do_astore_common("[F"); }
    void do_dastore() { do_astore_common("[D"); }
    void do_aastore() { do_astore_common("[Ljava/lang/Object;"); }
    void do_bastore() { do_astore_common("[B"); }
    void do_castore() { do_astore_common("[C"); }
    void do_sastore() { do_astore_common("[S"); }

    void do_astore_common(std::string className) {
       VMClass* arrayClass = resolver_->getClass(className);
      assert(arrayClass->isArray() && "Not an array class!");
       VMClass* componentClass = arrayClass->getComponentClass();
      Value* value = pop(componentClass->getType());
      Value* index = pop(Type::getInt32Ty(getGlobalContext()));
      Value* arrayRef = pop(arrayClass->getType());

      std::vector<Value*> indices;
      indices.reserve(3);
      indices.push_back(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0));
      indices.push_back(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 2));
      indices.push_back(index);
      Value* elementPtr =
        GetElementPtrInst::Create(arrayRef, indices, TMP, currentBB_);

      new StoreInst(value, elementPtr, currentBB_);
    }

    void do_pop() {
      opStack_.do_pop(currentBB_);
    }

    void do_pop2() {
      opStack_.do_pop2(currentBB_);
    }

    void do_dup() {
      opStack_.do_dup(currentBB_);
    }

    void do_dup_x1() {
      opStack_.do_dup_x1(currentBB_);
    }

    void do_dup_x2() {
      opStack_.do_dup_x2(currentBB_);
    }

    void do_dup2() {
      opStack_.do_dup2(currentBB_);
    }

    void do_dup2_x1() {
      opStack_.do_dup2_x1(currentBB_);
    }

    void do_dup2_x2() {
      opStack_.do_dup2_x2(currentBB_);
    }

    void do_swap() {
      opStack_.do_swap(currentBB_);
    }

    void do_iadd() { do_binary_op_common(Instruction::Add, Type::getInt32Ty(getGlobalContext())); }
    void do_ladd() { do_binary_op_common(Instruction::Add, Type::getInt64Ty(getGlobalContext())); }
    void do_fadd() { do_binary_op_common(Instruction::Add, Type::getFloatTy(getGlobalContext())); }
    void do_dadd() { do_binary_op_common(Instruction::Add, Type::getDoubleTy(getGlobalContext())); }

    void do_isub() { do_binary_op_common(Instruction::Sub, Type::getInt32Ty(getGlobalContext())); }
    void do_lsub() { do_binary_op_common(Instruction::Sub, Type::getInt64Ty(getGlobalContext())); }
    void do_fsub() { do_binary_op_common(Instruction::Sub, Type::getFloatTy(getGlobalContext())); }
    void do_dsub() { do_binary_op_common(Instruction::Sub, Type::getDoubleTy(getGlobalContext())); }

    void do_imul() { do_binary_op_common(Instruction::Mul, Type::getInt32Ty(getGlobalContext())); }
    void do_lmul() { do_binary_op_common(Instruction::Mul, Type::getInt64Ty(getGlobalContext())); }
    void do_fmul() { do_binary_op_common(Instruction::Mul, Type::getFloatTy(getGlobalContext())); }
    void do_dmul() { do_binary_op_common(Instruction::Mul, Type::getDoubleTy(getGlobalContext())); }

    void do_idiv() { do_binary_op_common(Instruction::UDiv, Type::getInt32Ty(getGlobalContext())); }
    void do_ldiv() { do_binary_op_common(Instruction::UDiv, Type::getInt64Ty(getGlobalContext())); }
    void do_fdiv() { do_binary_op_common(Instruction::FDiv, Type::getFloatTy(getGlobalContext())); }
    void do_ddiv() { do_binary_op_common(Instruction::FDiv, Type::getDoubleTy(getGlobalContext())); }

    void do_irem() { do_binary_op_common(Instruction::URem, Type::getInt32Ty(getGlobalContext())); }
    void do_lrem() { do_binary_op_common(Instruction::URem, Type::getInt64Ty(getGlobalContext())); }
    void do_frem() { do_binary_op_common(Instruction::FRem, Type::getFloatTy(getGlobalContext())); }
    void do_drem() { do_binary_op_common(Instruction::FRem, Type::getDoubleTy(getGlobalContext())); }

    void do_ineg() { do_neg_common(Type::getInt32Ty(getGlobalContext())); }
    void do_lneg() { do_neg_common(Type::getInt64Ty(getGlobalContext())); }
    void do_fneg() { do_neg_common(Type::getFloatTy(getGlobalContext())); }
    void do_dneg() { do_neg_common(Type::getDoubleTy(getGlobalContext())); }

    void do_neg_common( Type* type) {
      Value* v1 = pop(type);
      Value* r = BinaryOperator::CreateNeg(v1, TMP, currentBB_);
      push(r);
    }

    void do_ishl() { do_shift_common(Instruction::Shl, Type::getInt32Ty(getGlobalContext())); }
    void do_lshl() { do_shift_common(Instruction::Shl, Type::getInt64Ty(getGlobalContext())); }
    void do_ishr() { do_shift_common(Instruction::AShr, Type::getInt32Ty(getGlobalContext())); }
    void do_lshr() { do_shift_common(Instruction::AShr, Type::getInt64Ty(getGlobalContext())); }

    void do_iushr() { do_shift_common(Instruction::LShr, Type::getInt32Ty(getGlobalContext())); }
    void do_lushr() { do_shift_common(Instruction::LShr, Type::getInt64Ty(getGlobalContext())); }

    void do_shift_common(Instruction::BinaryOps op,  Type* type) {
      llvm::Constant* mask =
        type == Type::getInt32Ty(getGlobalContext()) ? INT_SHIFT_MASK : LONG_SHIFT_MASK;
      Value* a = pop(Type::getInt8Ty(getGlobalContext()));
      a = BinaryOperator::Create(Instruction::And, a, mask, TMP, currentBB_);
      Value* v = pop(type);
      Value* r = BinaryOperator::Create(op, v, a, TMP, currentBB_);
      push(r);
    }

    void do_iand() { do_binary_op_common(Instruction::And, Type::getInt32Ty(getGlobalContext())); }
    void do_land() { do_binary_op_common(Instruction::And, Type::getInt64Ty(getGlobalContext())); }
    void do_ior() { do_binary_op_common(Instruction::Or, Type::getInt32Ty(getGlobalContext())); }
    void do_lor() { do_binary_op_common(Instruction::Or, Type::getInt64Ty(getGlobalContext())); }
    void do_ixor() { do_binary_op_common(Instruction::Xor, Type::getInt32Ty(getGlobalContext())); }
    void do_lxor() { do_binary_op_common(Instruction::Xor, Type::getInt64Ty(getGlobalContext())); }

    void do_binary_op_common(Instruction::BinaryOps op,  Type* type) {
      Value* v2 = pop(type);
      Value* v1 = pop(type);
      Value* r = BinaryOperator::Create(op, v1, v2, TMP, currentBB_);
      push(r);
    }

    void do_iinc(unsigned index, int amount) {
      Value* v = locals_.load(index, Type::getInt32Ty(getGlobalContext()), currentBB_);
      Value* a = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), amount);
      v = BinaryOperator::CreateAdd(v, a, TMP, currentBB_);
      locals_.store(index, v, currentBB_);
    }

    void do_i2l() { do_cast_common(Type::getInt32Ty(getGlobalContext()), Type::getInt64Ty(getGlobalContext())); }
    void do_i2f() { do_cast_common(Type::getInt32Ty(getGlobalContext()), Type::getFloatTy(getGlobalContext())); }
    void do_i2d() { do_cast_common(Type::getInt32Ty(getGlobalContext()), Type::getDoubleTy(getGlobalContext())); }
    void do_l2i() { do_cast_common(Type::getInt64Ty(getGlobalContext()), Type::getInt32Ty(getGlobalContext())); }
    void do_l2f() { do_cast_common(Type::getInt64Ty(getGlobalContext()), Type::getFloatTy(getGlobalContext())); }
    void do_l2d() { do_cast_common(Type::getInt64Ty(getGlobalContext()), Type::getDoubleTy(getGlobalContext())); }
    void do_f2i() { do_cast_common(Type::getFloatTy(getGlobalContext()), Type::getInt32Ty(getGlobalContext())); }
    void do_f2l() { do_cast_common(Type::getFloatTy(getGlobalContext()), Type::getInt64Ty(getGlobalContext())); }
    void do_f2d() { do_cast_common(Type::getFloatTy(getGlobalContext()), Type::getDoubleTy(getGlobalContext())); }
    void do_d2i() { do_cast_common(Type::getDoubleTy(getGlobalContext()), Type::getInt32Ty(getGlobalContext())); }
    void do_d2l() { do_cast_common(Type::getDoubleTy(getGlobalContext()), Type::getInt64Ty(getGlobalContext())); }
    void do_d2f() { do_cast_common(Type::getDoubleTy(getGlobalContext()), Type::getFloatTy(getGlobalContext())); }
    void do_i2b() { do_cast_common(Type::getInt32Ty(getGlobalContext()), Type::getInt8Ty(getGlobalContext())); }
    void do_i2c() { do_cast_common(Type::getInt32Ty(getGlobalContext()), Type::getInt16Ty(getGlobalContext())); }
    void do_i2s() { do_cast_common(Type::getInt32Ty(getGlobalContext()), Type::getInt16Ty(getGlobalContext())); }

    void do_cast_common( Type* from,  Type* to) {
      Value* v1 = pop(from);
      if (from->isFloatingPointTy() && to->isFloatingPointTy()) {
        push(CastInst::CreateFPCast(v1, to, TMP, currentBB_));
      }
      else if (from->isIntegerTy() && to->isIntegerTy()) {
        push(CastInst::CreateIntegerCast(v1, to, true, TMP, currentBB_));
      }
      else {
        CastInst::CastOps opc = from->isFloatingPointTy() ? CastInst::FPToSI : CastInst::SIToFP;
        push(CastInst::Create(opc, v1, to, TMP, currentBB_));
      }
    }

    void do_lcmp() {
      Value* v2 = pop(Type::getInt64Ty(getGlobalContext()));
      Value* v1 = pop(Type::getInt64Ty(getGlobalContext()));
      Value* c = new ICmpInst(*currentBB_, ICmpInst::ICMP_UGT, v1, v2, TMP);
      Value* r = SelectInst::Create(c, ONE, ZERO, TMP, currentBB_);
      c = new ICmpInst(*currentBB_, ICmpInst::ICMP_ULT, v1, v2, TMP);
      r = SelectInst::Create(c, MINUS_ONE, r, TMP, currentBB_);
      push(r);
    }

    void do_fcmpl() { do_cmp_common(Type::getFloatTy(getGlobalContext()), false); }
    void do_dcmpl() { do_cmp_common(Type::getDoubleTy(getGlobalContext()), false); }
    void do_fcmpg() { do_cmp_common(Type::getFloatTy(getGlobalContext()), true); }
    void do_dcmpg() { do_cmp_common(Type::getDoubleTy(getGlobalContext()), true); }

    void do_cmp_common( Type* type, bool pushOne) {
      Value* v2 = pop(type);
      Value* v1 = pop(type);
      Value* c = new ICmpInst(*currentBB_, ICmpInst::ICMP_UGT, v1, v2, TMP);
      Value* r = SelectInst::Create(c, ONE, ZERO, TMP, currentBB_);
      c = new ICmpInst(*currentBB_, ICmpInst::ICMP_ULT, v1, v2, TMP);
      r = SelectInst::Create(c, MINUS_ONE, r, TMP, currentBB_);
      Value* args[] = {v1, v2};
      c = CallInst::Create(module_->getOrInsertFunction
                       ("llvm.isunordered",
                        Type::getInt1Ty(getGlobalContext()), v1->getType(), v2->getType(), NULL),
                       args, TMP, currentBB_);
      r = SelectInst::Create(c, pushOne ? ONE : MINUS_ONE, r, TMP, currentBB_);
      push(r);
    }

    void do_ifeq(unsigned t, unsigned f) {
      do_i(0);
      do_if_common(ICmpInst::ICMP_EQ, Type::getInt32Ty(getGlobalContext()), t, f);
    }
    void do_ifne(unsigned t, unsigned f) {
      do_i(0);
      do_if_common(ICmpInst::ICMP_NE, Type::getInt32Ty(getGlobalContext()), t, f);
    }
    void do_iflt(unsigned t, unsigned f) {
      do_i(0);
      do_if_common(ICmpInst::ICMP_ULT, Type::getInt32Ty(getGlobalContext()), t, f);
    }
    void do_ifge(unsigned t, unsigned f) {
      do_i(0);
      do_if_common(ICmpInst::ICMP_UGE, Type::getInt32Ty(getGlobalContext()), t, f);
    }
    void do_ifgt(unsigned t, unsigned f) {
      do_i(0);
      do_if_common(ICmpInst::ICMP_UGT, Type::getInt32Ty(getGlobalContext()), t, f);
    }
    void do_ifle(unsigned t, unsigned f) {
      do_i(0);
      do_if_common(ICmpInst::ICMP_ULE, Type::getInt32Ty(getGlobalContext()), t, f);
    }
    void do_if_icmpeq(unsigned t, unsigned f) {
      do_if_common(ICmpInst::ICMP_EQ, Type::getInt32Ty(getGlobalContext()), t, f);
    }
    void do_if_icmpne(unsigned t, unsigned f) {
      do_if_common(ICmpInst::ICMP_NE, Type::getInt32Ty(getGlobalContext()), t, f);
    }
    void do_if_icmplt(unsigned t, unsigned f) {
      do_if_common(ICmpInst::ICMP_ULT, Type::getInt32Ty(getGlobalContext()), t, f);
    }
    void do_if_icmpge(unsigned t, unsigned f) {
      do_if_common(ICmpInst::ICMP_UGE, Type::getInt32Ty(getGlobalContext()), t, f);
    }
    void do_if_icmpgt(unsigned t, unsigned f) {
      do_if_common(ICmpInst::ICMP_UGT, Type::getInt32Ty(getGlobalContext()), t, f);
    }
    void do_if_icmple(unsigned t, unsigned f) {
      do_if_common(ICmpInst::ICMP_ULE, Type::getInt32Ty(getGlobalContext()), t, f);
    }
    void do_if_acmpeq(unsigned t, unsigned f) {
      do_if_common(ICmpInst::ICMP_EQ, resolver_->getObjectBaseType(), t, f);
    }
    void do_if_acmpne(unsigned t, unsigned f) {
      do_if_common(ICmpInst::ICMP_NE, resolver_->getObjectBaseType(), t, f);
    }
    void do_ifnull(unsigned t, unsigned f) {
      do_a_null();
      do_if_common(ICmpInst::ICMP_EQ, resolver_->getObjectBaseType(), t, f);
    }
    void do_ifnonnull(unsigned t, unsigned f) {
      do_a_null();
      do_if_common(ICmpInst::ICMP_NE, resolver_->getObjectBaseType(), t, f);
    }

    void do_if_common(ICmpInst::Predicate cc,  Type* type,
                      unsigned t, unsigned f) {
      Value* v2 = pop(type);
      Value* v1 = pop(type);
      Value* c = new ICmpInst(*currentBB_, cc, v1, v2, TMP);
      BranchInst::Create(bbBuilder_->getBasicBlock(t),
                     bbBuilder_->getBasicBlock(f),
                     c, currentBB_);
    }

    void do_goto(unsigned target) {
      BranchInst::Create(bbBuilder_->getBasicBlock(target), currentBB_);
    }

    void do_ireturn() { do_return_common(Type::getInt32Ty(getGlobalContext())); }
    void do_lreturn() { do_return_common(Type::getInt64Ty(getGlobalContext())); }
    void do_freturn() { do_return_common(Type::getFloatTy(getGlobalContext())); }
    void do_dreturn() { do_return_common(Type::getDoubleTy(getGlobalContext())); }
    void do_areturn() { do_return_common(resolver_->getObjectBaseType()); }

    void do_return_common( Type* type) {
      Value* r = pop(type);
       Type* retTy = currentBB_->getParent()->getReturnType();
      if (retTy != r->getType())
        r = CastInst::CreateSExtOrBitCast(r, retTy, TMP, currentBB_);
      ReturnInst::Create(type->getContext(), r, currentBB_);
    }

    void do_return() {
      ReturnInst::Create(getGlobalContext(), NULL, currentBB_);
    }

    void do_jsr(unsigned target, unsigned retAddress) {
      // FIXME: this is currently a noop.
      push(llvm::Constant::getNullValue(Type::getInt32Ty(getGlobalContext())));
    }

    void do_ret(unsigned index) {
      // FIXME: this is currently a noop.
    }

    void do_tableswitch(unsigned defTarget,  SwitchCases& sw) {
      do_switch_common(defTarget, sw);
    }

    void do_lookupswitch(unsigned defTarget,  SwitchCases& sw) {
      do_switch_common(defTarget, sw);
    }

    void do_switch_common(unsigned defTarget,  SwitchCases& sw) {
      Value* v = pop(Type::getInt32Ty(getGlobalContext()));
      SwitchInst* in =
        SwitchInst::Create(v, bbBuilder_->getBasicBlock(defTarget), sw.size(),
                       currentBB_);
      for (unsigned i = 0, e = sw.size(); i != e; ++i)
        in->addCase(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), sw[i].first),
                    bbBuilder_->getBasicBlock(sw[i].second));
    }

    void do_getstatic(unsigned index) {
       VMField* field = class_->getField(index);
      emitClassInitializers(field->getParent());

      Value* v = new LoadInst(field->getGlobal(), TMP, currentBB_);
      push(v);
    }

    void do_putstatic(unsigned index) {
       VMField* field = class_->getField(index);
      emitClassInitializers(field->getParent());

      Value* v = pop(field->getClass()->getType());
      new StoreInst(v, field->getGlobal(), currentBB_);
    }

    void do_getfield(unsigned index) {
       VMField* field = class_->getField(index);

      Value* p = pop(field->getParent()->getType());
      std::vector<Value*> indices(2);
      indices[0] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0);
      indices[1] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), field->getMemberIndex());
      Value* fieldPtr =
        GetElementPtrInst::Create(p, indices, field->getName()+'*', currentBB_);
      Value* v = new LoadInst(fieldPtr, field->getName(), currentBB_);
      push(v);
    }

    void do_putfield(unsigned index) {
       VMField* field = class_->getField(index);

      Value* v = pop(field->getClass()->getType());
      Value* p = pop(field->getParent()->getType());
      std::vector<Value*> indices(2);
      indices[0] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0);
      indices[1] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), field->getMemberIndex());
      Value* fieldPtr = GetElementPtrInst::Create(p, indices, field->getName()+'*', currentBB_);
      new StoreInst(v, fieldPtr, currentBB_);
    }

    void makeCall(Value* fun,  std::vector<Value*> params) {
       PointerType* funPtrTy = cast<PointerType>(fun->getType());
       FunctionType* funTy =
        cast<FunctionType>(funPtrTy->getElementType());

      if (funTy->getReturnType() == Type::getVoidTy(getGlobalContext()))
        CallInst::Create(fun, params, "", currentBB_);
      else {
        Value* r = CallInst::Create(fun, params, TMP, currentBB_);
        push(r);
      }
    }

    std::vector<Value*> getParams( FunctionType* funTy) {
      unsigned numParams = funTy->getNumParams();
      std::vector<Value*> params(numParams);
      while (numParams--)
        params[numParams] = pop(funTy->getParamType(numParams));

      return params;
    }

    void do_invokevirtual(unsigned index) {
       VMMethod* method = class_->getMethod(index);
       VMClass* clazz = method->getParent();

      Function* function = method->getFunction();
      std::vector<Value*> params(getParams(function->getFunctionType()));

      Value* objRef = params.front();
      objRef = CastInst::CreatePointerBitCastOrAddrSpaceCast(objRef, clazz->getType(), "this", currentBB_);
      Value* objBase =
          CastInst::CreatePointerBitCastOrAddrSpaceCast(objRef, resolver_->getObjectBaseType(), TMP, currentBB_);
      Value* classRecord =
        CallInst::Create(getClassRecord_, objBase, TMP, currentBB_);
      classRecord = CastInst::CreatePointerBitCastOrAddrSpaceCast(classRecord,
                                 clazz->getClassRecord()->getType(),
                                 clazz->getName() + ".classRecord", currentBB_);
      std::vector<Value*> indices(1, ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0));
      assert(method->getMethodIndex() != -1 &&
             "Method index not found for dynamically bound method!");
      indices.push_back(
        ConstantInt::get(Type::getInt32Ty(getGlobalContext()), method->getMethodIndex()+1));
      Value* funPtr =
        GetElementPtrInst::Create(classRecord, indices, TMP, currentBB_);
      Value* fun = new LoadInst(funPtr, function->getName(), currentBB_);

      makeCall(fun, params);
    }

    void do_invokespecial(unsigned index) {
       VMMethod* method = class_->getMethod(index);
      Function* function = method->getFunction();
      makeCall(function, getParams(function->getFunctionType()));
    }

    void do_invokestatic(unsigned index) {
       VMMethod* method = class_->getMethod(index);
      emitClassInitializers(method->getParent());
      Function* function = method->getFunction();
      makeCall(function, getParams(function->getFunctionType()));
    }

    void do_invokeinterface(unsigned index) {
       VMMethod* method = class_->getMethod(index);
       VMClass* clazz = method->getParent();
      assert(clazz->isInterface() && "Class must be an interface!");

      Function* function = method->getFunction();
      std::vector<Value*> params(getParams(function->getFunctionType()));

      Value* objRef = params.front();
      objRef = CastInst::CreatePointerBitCastOrAddrSpaceCast(objRef, clazz->getType(), "this", currentBB_);
      Value* objBase =
          CastInst::CreatePointerBitCastOrAddrSpaceCast(objRef, resolver_->getObjectBaseType(), TMP, currentBB_);
      Value* classRecord =
        CallInst::Create(getClassRecord_, objBase, TMP, currentBB_);
      classRecord = CastInst::CreatePointerBitCastOrAddrSpaceCast(classRecord,
                                 resolver_->getClassRecordPtrType(),
                                 TMP, currentBB_);
      // get the interfaces array of class records
      std::vector<Value*> indices(2, ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0));
      indices.push_back(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 4));
      Value* interfaceClassRecords =
        GetElementPtrInst::Create(classRecord, indices, TMP, currentBB_);
      interfaceClassRecords =
        new LoadInst(interfaceClassRecords, TMP, currentBB_);
      // Get the actual interface class record.
      indices.clear();
      indices.push_back(
        ConstantInt::get(Type::getInt32Ty(getGlobalContext()), clazz->getInterfaceIndex()));
      Value* interfaceClassRecord =
        GetElementPtrInst::Create(interfaceClassRecords, indices, TMP, currentBB_);
      interfaceClassRecord =
        new LoadInst(interfaceClassRecord,
                     clazz->getName() + ".classRecord", currentBB_);
      interfaceClassRecord =
          CastInst::CreatePointerBitCastOrAddrSpaceCast(interfaceClassRecord,
                     clazz->getClassRecord()->getType(), TMP, currentBB_);
      // Get the function pointer.
      assert(method->getMethodIndex() != -1 &&
             "Method index not found for dynamically bound method!");
      indices.resize(2);
      indices[0] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0);
      indices[1] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), method->getMethodIndex()+1);
      Value* funPtr =
        GetElementPtrInst::Create(interfaceClassRecord, indices, TMP, currentBB_);
      Value* fun = new LoadInst(funPtr, function->getName(), currentBB_);

      makeCall(fun, params);
    }

    template<typename InsertionPointTy>
    Value* allocateObject( VMClass& clazz, InsertionPointTy* ip) {
      static std::vector<Value*> params(4);

      Value* objRef = new AllocaInst(clazz.getLayoutType(), NULL, TMP, ip);
      params[0] =
          CastInst::CreatePointerBitCastOrAddrSpaceCast(objRef, PointerType::get(Type::getInt8Ty(getGlobalContext()), 0), TMP, ip); // dest
      params[1] = ConstantInt::get(Type::getInt8Ty(getGlobalContext()), 0); // value
      params[2] = ConstantExpr::getSizeOf(clazz.getLayoutType()); // size
      params[3] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0); // alignment
      CallInst::Create(memset_, params, "", ip);

      // Install the class record.
      Value* objBase =
        CastInst::CreatePointerBitCastOrAddrSpaceCast(objRef, resolver_->getObjectBaseType(), TMP, ip);
       Type* classRecordPtrType = resolver_->getClassRecordPtrType();
      Value* classRecord =
          CastInst::CreatePointerBitCastOrAddrSpaceCast(clazz.getClassRecord(), classRecordPtrType, TMP, ip);
      Value* args[] = {objBase, classRecord};
      CallInst::Create(setClassRecord_, args, "", ip);

      return objRef;
    }

    void do_new(unsigned index) {
       VMClass* clazz = class_->getClass(index);
      emitClassInitializers(clazz);
      push(allocateObject(*clazz, currentBB_));
    }

    template <typename InsertionPointTy>
    Value* getArrayLengthPtr(Value* arrayRef, InsertionPointTy* ip)  {
      std::vector<Value*> indices;
      indices.reserve(2);
      indices.push_back(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0));
      indices.push_back(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 1));

      return GetElementPtrInst::Create(arrayRef, indices, TMP, ip);
    }

    template <typename InsertionPointTy>
    Value* getArrayObjectBasePtr(Value* arrayRef, InsertionPointTy* ip)  {
      std::vector<Value*> indices;
      indices.reserve(2);
      indices.push_back(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0));
      indices.push_back(ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0));

      return GetElementPtrInst::Create(arrayRef, indices, TMP, ip);
    }

    template<typename InsertionPointTy>
    Value* allocateArray( VMClass* clazz,
                         Value* count,
                         InsertionPointTy* ip) {
      static std::vector<Value*> params(4);

      assert(clazz->isArray() && "Not an array class!");
       VMClass* componentClass = clazz->getComponentClass();
       Type* elementTy = componentClass->getType();

      // The size of the element.
      llvm::Constant* elementSize = nullptr;
      if (elementTy->getIntegerBitWidth() <= 32)
        elementSize = ConstantExpr::getZExt(ConstantExpr::getSizeOf(elementTy), Type::getInt32Ty(getGlobalContext()));
      else
        elementSize = ConstantExpr::getTrunc(ConstantExpr::getSizeOf(elementTy), Type::getInt32Ty(getGlobalContext()));

      // The size of the array part of the struct.
      Value* size = BinaryOperator::Create(
        Instruction::Mul, count, elementSize, TMP, ip);
      // The size of the rest of the array object.
      llvm::Constant* arrayObjectSize =
        ConstantExpr::getBitCast(ConstantExpr::getSizeOf(clazz->getLayoutType()),
                              Type::getInt32Ty(getGlobalContext()));

      // Add the array part plus the object part together.
      size = BinaryOperator::Create(
        Instruction::Add, size, arrayObjectSize, TMP, ip);
      // Allocate memory for the object.
      Value* objRef = new AllocaInst(Type::getInt8Ty(getGlobalContext()), size, TMP, ip);
      params[0] = objRef; // dest
      params[1] = ConstantInt::get(Type::getInt8Ty(getGlobalContext()), 0); // value
      params[2] = CastInst::Create(CastInst::ZExt, size, Type::getInt64Ty(getGlobalContext()), TMP, ip); // size
      params[3] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0); // alignment
      CallInst::Create(memset_, params, "", ip);

      // Cast back to array type.
      objRef = CastInst::CreatePointerBitCastOrAddrSpaceCast(objRef, clazz->getType(), TMP, ip);

      // Store the size.
      Value* lengthPtr = getArrayLengthPtr(objRef, ip);
      new StoreInst(count, lengthPtr, ip);

      // Install the class record.
      Value* objBase = CastInst::CreatePointerBitCastOrAddrSpaceCast(objRef, resolver_->getObjectBaseType(), TMP, ip);
       Type* classRecordPtrType = resolver_->getClassRecordPtrType();
      Value* classRecord =
        CastInst::CreatePointerBitCastOrAddrSpaceCast(clazz->getClassRecord(), classRecordPtrType, TMP, ip);

      Value* args[] = {objBase, classRecord};
      CallInst::Create(setClassRecord_, makeArrayRef(args), "", ip);

      return objRef;
    }

    void do_newarray(JType type) {
      Value* count = pop(Type::getInt32Ty(getGlobalContext()));

       VMClass* clazz = resolver_->getClass(type);
       VMClass* arrayClass = resolver_->getArrayClass(clazz);
      emitClassInitializers(arrayClass);

      push(allocateArray(arrayClass, count, currentBB_));
    }

    void do_anewarray(unsigned index) {
      Value* count = pop(Type::getInt32Ty(getGlobalContext()));

       VMClass* clazz = class_->getClass(index);
       VMClass* arrayClass = resolver_->getArrayClass(clazz);
      emitClassInitializers(arrayClass);

      push(allocateArray(arrayClass, count, currentBB_));
    }

    void do_arraylength() {
       VMClass* clazz = resolver_->getClass("[Ljava/lang/Object;");
      Value* arrayRef = pop(clazz->getType());
      Value* lengthPtr = getArrayLengthPtr(arrayRef, currentBB_);
      Value* length = new LoadInst(lengthPtr, TMP, currentBB_);
      push(length);
    }

    void do_athrow() {
      Value* objRef = pop(resolver_->getObjectBaseType());
      CallInst::Create(throw_, objRef, "", currentBB_);
      new UnreachableInst(currentBB_->getContext(), currentBB_);
    }

    void do_checkcast(unsigned index) {
       VMClass* clazz = class_->getClass(index);

      Value* objRef = pop(resolver_->getObjectBaseType());
       Type* classRecordPtrType = resolver_->getClassRecordPtrType();
      Value* classRecord = CastInst::CreatePointerBitCastOrAddrSpaceCast(clazz->getClassRecord(),
                                        classRecordPtrType, TMP, currentBB_);
      Value* args[] = {objRef, classRecord};
      Value* r = CallInst::Create(isInstanceOf_, args, TMP, currentBB_);


      new ICmpInst(*currentBB_, ICmpInst::ICMP_EQ,
                   r, ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 1),
                   TMP);
      // FIXME: if b is false we must throw a ClassCast exception
      push(objRef);
    }

    void do_instanceof(unsigned index) {
       VMClass* clazz = class_->getClass(index);

      Value* objRef = pop(resolver_->getObjectBaseType());
       Type* classRecordPtrType = resolver_->getClassRecordPtrType();
      Value* classRecord = CastInst::CreatePointerBitCastOrAddrSpaceCast(clazz->getClassRecord(),
                                        classRecordPtrType, TMP, currentBB_);
      Value* args[] = {objRef, classRecord};
      Value* r = CallInst::Create(isInstanceOf_, args, TMP, currentBB_);
      push(r);
    }

    void do_monitorenter() {
      // FIXME: This is currently a noop.
      pop(resolver_->getObjectBaseType());
    }

    void do_monitorexit() {
      // FIXME: This is currently a noop.
      pop(resolver_->getObjectBaseType());
    }

    void do_multianewarray(unsigned index, unsigned dims) {
      assert(0 && "not implemented");
    }
  };

} } } // namespace llvm::Java::

Module* llvm::Java::compile(std::string className)
{
  DEBUG(std::cerr << "Compiling class: " << className << '\n');

  LLVMContext &ctx = getGlobalContext();
  Module* m = new Module(className, ctx);
  // Require the Java runtime.
  // m->addLibrary("jrt");

  Compiler c(m);
   VMMethod* main =
    c.compileMethod(className, "main([Ljava/lang/String;)V");

  Function* javaMain = dyn_cast<Function>(m->getOrInsertFunction
    ("llvm_java_main", Type::getVoidTy(getGlobalContext()),
     Type::getInt32Ty(getGlobalContext()), PointerType::get(PointerType::get(Type::getInt8Ty(getGlobalContext()), 0), 0), NULL));

  BasicBlock* bb = BasicBlock::Create(ctx, "entry", javaMain);
   FunctionType* mainTy = main->getFunction()->getFunctionType();
  CallInst::Create(main->getFunction(),
               // FIXME: Forward correct params from llvm_java_main
               llvm::Constant::getNullValue(mainTy->getParamType(0)),
               "",
               bb);
  ReturnInst::Create(ctx, NULL, bb);
  return m;
}
