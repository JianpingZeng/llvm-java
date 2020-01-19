//===-- BasicBlockBuilder.h - Java bytecode BasicBlock builder --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by the LLVM research group and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a BasicBlock builder that creates a LLVM
// BasicBlocks from Java bytecode. It also keeps track of the java
// bytecode indices that bound each basic block.
//
//===----------------------------------------------------------------------===//

#include <llvm/Java/BytecodeParser.h>
#include <llvm/Java/ClassFile.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/IR/Instructions.h>
#include <map>

namespace llvm { namespace Java {

  class BasicBlockBuilder : public BytecodeParser<BasicBlockBuilder> {
    Function* function_;
    typedef std::map<unsigned, BasicBlock*> BC2BBMap;
    BC2BBMap bc2bbMap_;
    typedef std::map<BasicBlock*, std::pair<unsigned,unsigned> > BB2BCMap;
    BB2BCMap bb2bcMap_;
    LLVMContext &ctx;

    BasicBlock* getOrCreateBasicBlockAt(unsigned bcI) {
      BC2BBMap::iterator it = bc2bbMap_.lower_bound(bcI);
      if (it == bc2bbMap_.end() || it->first != bcI) {
        bool inserted;
        BasicBlock* newBB = BasicBlock::Create(ctx, "bc" + utostr(bcI), function_);
        tie(it, inserted) = bc2bbMap_.insert(std::make_pair(bcI, newBB));
        assert(inserted && "LLVM basic block multiply defined!");
      }

      return it->second;
    }

  public:
    BasicBlockBuilder(Function* f, CodeAttribute* c)
      : function_(f), ctx(f->getContext()) {

      // Create the entry block.
      BasicBlock* entry = BasicBlock::Create(ctx, "entry", function_);

      // Create the block for bytecode 0 (bb0).
      BasicBlock* bb = getOrCreateBasicBlockAt(0);

      // Create basic blocks for exception handlers.
      const CodeAttribute::Exceptions& exceptions = c->getExceptions();
      for (unsigned i = 0, e = exceptions.size(); i != e; ++i)
        getOrCreateBasicBlockAt(exceptions[i]->getHandlerPc());

      // Parse the bytecode and create basic blocks for all targets of
      // control flow instructions.
      parse(c->getCode(), 0, c->getCodeSize());

      // Build the bytecode->basic block map.
      for (BC2BBMap::const_iterator i = bc2bbMap_.begin(), e = bc2bbMap_.end();
           i != e; ++i) {
        unsigned end = next(i) != e ? next(i)->first : c->getCodeSize();
        bb2bcMap_.insert(
          std::make_pair(i->second, std::make_pair(i->first, end)));
      }

      assert(function_->getEntryBlock().getName() == "entry");
      assert(bb2bcMap_.find(&function_->getEntryBlock()) == bb2bcMap_.end());
    }

    BasicBlock* getBasicBlock(unsigned bcI) const {
      BC2BBMap::const_iterator i = bc2bbMap_.find(bcI);
      assert(i != bc2bbMap_.end() &&
             "No block is mapped to this bytecode index!");
      return i->second;
    }

    std::pair<unsigned,unsigned> getBytecodeIndices(BasicBlock* bb) const {
      BB2BCMap::const_iterator i = bb2bcMap_.find(bb);
      assert(i != bb2bcMap_.end() &&
             "BasicBlock was not created by this builder!");
      return i->second;
    }

    void do_ifeq(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_ifne(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_iflt(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_ifge(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_ifgt(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_ifle(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_if_icmpeq(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_if_icmpne(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_if_icmplt(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_if_icmpgt(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_if_icmpge(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_if_icmple(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_if_acmpeq(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_if_acmpne(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_jsr(unsigned t, unsigned r) {
      getOrCreateBasicBlockAt(r);
      getOrCreateBasicBlockAt(t);
    }

    void do_switch_common(unsigned defTarget, const SwitchCases& sw) {
      for (unsigned i = 0, e = sw.size(); i != e; ++i) {
        unsigned target = sw[i].second;
        getOrCreateBasicBlockAt(target);
      }
      getOrCreateBasicBlockAt(defTarget);
    }

    void do_tableswitch(unsigned defTarget, const SwitchCases& sw) {
      do_switch_common(defTarget, sw);
    }

    void do_lookupswitch(unsigned defTarget, const SwitchCases& sw) {
      do_switch_common(defTarget, sw);
    }

    void do_goto(unsigned target) {
      getOrCreateBasicBlockAt(target);
    }

    void do_ifnull(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }

    void do_ifnonnull(unsigned t, unsigned f) {
      getOrCreateBasicBlockAt(f);
      getOrCreateBasicBlockAt(t);
    }
  };

} } // namespace llvm::Java
