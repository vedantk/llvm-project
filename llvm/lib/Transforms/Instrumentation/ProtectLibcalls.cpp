//===- ProtectLibcalls.cpp - Trap on invalid library calls ----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Instrumentation/ProtectLibcalls.h"

#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/ValueMap.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Transforms/Utils/Local.h"
#include <utility>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "protect-libcalls"

STATISTIC(NumAllocationsFound, "Number of allocations found");
STATISTIC(NumAllocationsDeemedSafe, "Number of allocation deemed to be safe");
STATISTIC(NumAllocationsChecked, "Number of allocation checks inserted");

static cl::opt<bool> DisableProtectLibcalls(
  "disable-protect-libcalls", cl::Hidden, cl::init(false),
  cl::desc("Disable the ProtectLibcalls pass"));

static Instruction *findTerminatingInstruction(BasicBlock &BB) {
  if (auto *I = BB.getTerminatingMustTailCall())
    return I;
  if (auto *I = BB.getTerminatingDeoptimizeCall())
    return I;
  return BB.getTerminator();
}

static bool promoteToCheckedValue(Value *V,
                                  ValueMap<Value *, Value *> &OverflowMap,
                                  std::vector<Value *> &OverflowBits);

static bool lookupOverflow(Value *V, ValueMap<Value *, Value *> &OverflowMap,
                           std::vector<Value *> &OverflowBits) {
  auto It = OverflowMap.find(V);
  bool Found = It != OverflowMap.end();
  if (Found) {
    Value *DidOverflow = It->second;
    assert(DidOverflow && "Overflow result for V not cached.");
    OverflowBits.push_back(DidOverflow);
  }
  return Found;
}

static Optional<Intrinsic::ID>
getOverflowIntrinsicForBinOp(BinaryOperator *BinOp) {
  if (!isa<OverflowingBinaryOperator>(BinOp))
    return None;

  // Check for unsigned overflow if possible and signed overflow if not. Rely
  // on a `nsw` hint from the frontend to figure this out. This avoids breaking
  // code that computes negative offsets, such as (add %x, -1).
  bool HasNSW = BinOp->hasNoSignedWrap();

  // LLVM canonicalizes "sub %x, const" as "add %x, -const": the latter sets
  // the carry bit and would therefore trip an unsigned overflow check.
  Value *RHS = BinOp->getOperand(1);
  bool IsRHSNegative =
      isa<ConstantInt>(RHS) && cast<ConstantInt>(RHS)->getValue().isNegative();

  switch (BinOp->getOpcode()) {
  case Instruction::Add:
    if (HasNSW || IsRHSNegative)
      return Intrinsic::sadd_with_overflow;
    return Intrinsic::uadd_with_overflow;
  case Instruction::Sub:
    if (HasNSW)
      return Intrinsic::ssub_with_overflow;
    return Intrinsic::usub_with_overflow;
  case Instruction::Mul:
    LLVM_FALLTHROUGH;
  case Instruction::Shl:
    // An unsigned overflow check might not be safe, as the expression might be
    // (mul (add %x, -1), %y), which would trip the check if the result of the
    // add is negative. Checking for signed overflow is sane.
    return Intrinsic::smul_with_overflow;
  default:
    llvm_unreachable("Invalid binop.");
  }
}

static bool isInvalidShiftAmount(Value *Shamt) {
  if (auto *Bits = dyn_cast<ConstantInt>(Shamt))
    return Bits->getValue().uge(Bits->getBitWidth());
  return false;
}

static bool promoteToCheckedBinOp(BinaryOperator *BinOp,
                                  ValueMap<Value *, Value *> &OverflowMap,
                                  std::vector<Value *> &OverflowBits) {
  bool Changed = false;
  LLVMContext &Ctx = BinOp->getContext();
  auto OverflowIID = getOverflowIntrinsicForBinOp(BinOp);
  if (!OverflowIID)
    return Changed;

  // Helper to replace the binop with a checked variant.
  IRBuilder<> B(BinOp);
  auto substituteCheckedOp = [BinOp, &B, &OverflowMap,
                              &OverflowBits](Instruction *CheckedOp) {
    Value *NewLHS = CheckedOp->getOperand(0);
    Value *NewRHS = CheckedOp->getOperand(1);
    Value *Result = B.CreateExtractValue(CheckedOp, 0);
    Value *Overflow = B.CreateExtractValue(CheckedOp, 1);
    BinOp->replaceAllUsesWith(Result);
    BinOp->eraseFromParent();
    OverflowMap[Result] = Overflow;
    OverflowBits.push_back(Overflow);

    // Recurse on the binop operands.
    promoteToCheckedValue(NewLHS, OverflowMap, OverflowBits);
    promoteToCheckedValue(NewRHS, OverflowMap, OverflowBits);
    return true;
  };

  Type *EltTy = BinOp->getType();
  Value *LHS = BinOp->getOperand(0), *RHS = BinOp->getOperand(1);
  bool IsLeftShift = BinOp->getOpcode() == Instruction::Shl;

  if (!IsLeftShift) {
    Instruction *CheckedOp = B.CreateBinaryIntrinsic(*OverflowIID, LHS, RHS);
    return substituteCheckedOp(CheckedOp);
  }

  assert(IsLeftShift && "Invalid binop.");

  if (isInvalidShiftAmount(RHS)) {
    OverflowBits.push_back(ConstantInt::getTrue(Ctx));
    return Changed;
  }

  bool Found = lookupOverflow(BinOp, OverflowMap, OverflowBits);
  if (Found)
    return Changed;

  // Special case: LHS is a power of 2. Check that RHS < (BitW - log(LHS)).
  if (auto *LHSConst = dyn_cast<ConstantInt>(LHS)) {
    APInt LHSVal = LHSConst->getValue();
    if (LHSVal.isPowerOf2()) {
      unsigned BitW = LHSConst->getBitWidth();
      APInt FirstInvalidWidth = APInt(BitW, BitW) - LHSVal.logBase2();
      Value *Overflow = B.CreateICmpUGE(
          RHS, ConstantInt::get(EltTy, FirstInvalidWidth), "shamt.valid");
      if (auto *ICmp = dyn_cast<Instruction>(Overflow))
        ICmp->moveAfter(BinOp);
      Changed = true;
      OverflowMap[BinOp] = Overflow;
      OverflowBits.push_back(Overflow);
      return Changed;
    }
  }

  // General case: multiply LHS by 2**RHS.
  Value *Multiplier =
      B.CreateShl(ConstantInt::get(EltTy, 1), RHS, "shamt.as.multiplier",
                  /*nuw=*/true, /*nsw=*/true);
  Instruction *CheckedOp =
      B.CreateBinaryIntrinsic(*OverflowIID, LHS, Multiplier);
  return substituteCheckedOp(CheckedOp);
}

static bool promoteToCheckedPhi(PHINode *Phi,
                                ValueMap<Value *, Value *> &OverflowMap,
                                std::vector<Value *> &OverflowBits) {
  bool Changed = false;
  LLVMContext &Ctx = Phi->getContext();

  // Check if the phi was visited already.
  {
    // Look up the overflow result. Do this in a fresh scope, as OverflowMap
    // may change before we can compute the overflow result below.
    Value *&DidOverflow = OverflowMap[Phi];
    if (DidOverflow) {
      lookupOverflow(Phi, OverflowMap, OverflowBits);
      return Changed;
    }

    // Conservatively assume there's no overflow if we need to break a loop.
    DidOverflow = ConstantInt::getFalse(Ctx);
  }

  // Check whether any of the incoming values overflow.
  unsigned NumIncomingBBs = Phi->getNumIncomingValues();
  std::vector<std::pair<Value *, BasicBlock *>> PerBlockOverflowBits;
  for (unsigned I = 0; I < NumIncomingBBs; ++I) {
    Value *Incoming = Phi->getIncomingValue(I);
    BasicBlock *IncomingBB = Phi->getIncomingBlock(I);
    std::vector<Value *> IncomingBBOverflowBits;
    Changed |=
        promoteToCheckedValue(Incoming, OverflowMap, IncomingBBOverflowBits);

    Value *IncomingBBOverflow;
    if (IncomingBBOverflowBits.empty()) {
      // If there aren't any incoming overflow results, assume no overflow.
      IncomingBBOverflow = ConstantInt::getFalse(Ctx);
    } else if (IncomingBBOverflowBits.size() == 1) {
      // If there's exactly one incoming overflow result, use it directly.
      IncomingBBOverflow = IncomingBBOverflowBits.front();
    } else {
      // If there's more than one incoming overflow result, merge them.
      IRBuilder<> B(findTerminatingInstruction(*IncomingBB));
      IncomingBBOverflow = B.CreateOr(IncomingBBOverflowBits);
      Changed = true;
    }
    PerBlockOverflowBits.push_back({IncomingBBOverflow, IncomingBB});
  }

  // Create a phi for the incoming overflow results.
  IRBuilder<> B(Phi->getParent()->getFirstNonPHI());
  PHINode *DidOverflowPhi = B.CreatePHI(
      Type::getInt1Ty(Ctx), PerBlockOverflowBits.size(), "did.overflow");
  for (const auto &OverflowBit : PerBlockOverflowBits)
    DidOverflowPhi->addIncoming(OverflowBit.first, OverflowBit.second);
  Value *&DidOverflow = OverflowMap[Phi];
  DidOverflow = DidOverflowPhi;
  OverflowBits.push_back(DidOverflow);
  Changed = true;
  return Changed;
}

static bool promoteToCheckedValue(Value *V,
                                  ValueMap<Value *, Value *> &OverflowMap,
                                  std::vector<Value *> &OverflowBits) {
  bool Changed = false;
  LLVMContext &Ctx = V->getContext();
  assert(V->getType()->isIntegerTy() && "Not an integral value");

  // Peel off integer casts.
  if (auto *Cast = dyn_cast<CastInst>(V)) {
    if (Cast->getSrcTy()->isIntegerTy())
      Changed |=
          promoteToCheckedValue(Cast->getOperand(0), OverflowMap, OverflowBits);
    return Changed;
  }

  // A constant can't overflow unless it's undef.
  if (isa<Constant>(V)) {
    if (isa<UndefValue>(V))
      OverflowBits.push_back(ConstantInt::getTrue(Ctx));
    return Changed;
  }

  // Recursively promote arithmetic to checked arithmetic.
  if (auto *BinOp = dyn_cast<BinaryOperator>(V)) {
    Changed |= promoteToCheckedBinOp(BinOp, OverflowMap, OverflowBits);
    return Changed;
  }

  // Look up the overflow results for previously-checked arithmetic.
  if (auto *Extract = dyn_cast<ExtractValueInst>(V)) {
    bool Found = lookupOverflow(Extract, OverflowMap, OverflowBits);
    if (Found) {
      auto *Call = cast<IntrinsicInst>(Extract->getAggregateOperand());
      Changed |=
          promoteToCheckedValue(Call->getOperand(0), OverflowMap, OverflowBits);
      Changed |=
          promoteToCheckedValue(Call->getOperand(1), OverflowMap, OverflowBits);
    }
    return Changed;
  }

  // Perform checking across basic block boundaries.
  if (auto *Phi = dyn_cast<PHINode>(V)) {
    Changed |= promoteToCheckedPhi(Phi, OverflowMap, OverflowBits);
    return Changed;
  }

  return Changed;
}

static bool
emitOverflowChecksForOperands(Instruction *I, ArrayRef<unsigned> Ops,
                              ValueMap<Value *, Value *> &OverflowMap) {
  ++NumAllocationsFound;

  bool Changed = false;
  std::vector<Value *> OverflowBits;
  for (unsigned OpNo : Ops)
    Changed |=
        promoteToCheckedValue(I->getOperand(OpNo), OverflowMap, OverflowBits);

  if (OverflowBits.empty()) {
    assert(!Changed && "No overflow values found, but function was changed?");
    ++NumAllocationsDeemedSafe;
    return false;
  }

  // Check if any arithmetic overflowed.
  IRBuilder<> B(I);
  Value *DidOverflow = B.CreateOr(OverflowBits);

  // Branch to TrapBB on overflow, to AllocBB otherwise.
  BasicBlock *OrigBB = I->getParent();
  BasicBlock *AllocBB = OrigBB->splitBasicBlock(I, "guarded.alloc");
  BasicBlock *TrapBB = BasicBlock::Create(I->getContext(), "alloc.trap",
                                          I->getFunction(), AllocBB);
  B.SetCurrentDebugLocation(I->getDebugLoc());
  B.SetInsertPoint(OrigBB->getTerminator());
  B.CreateCondBr(DidOverflow, TrapBB, AllocBB);

  // Delete the unconditional branch from OrigBB to AllocBB.
  OrigBB->getTerminator()->eraseFromParent();

  // Set up the trap.
  B.SetInsertPoint(TrapBB);
  B.SetCurrentDebugLocation(I->getDebugLoc());
  B.CreateIntrinsic(Intrinsic::trap, {}, {});
  B.CreateUnreachable();
  Changed = true;

  ++NumAllocationsChecked;
  return Changed;
}

static bool protectLibcalls(Function &F, const TargetLibraryInfo &TLI) {
  bool Changed = false;
  if (DisableProtectLibcalls)
    return Changed;

  // List of unchecked operands in allocation sites.
  std::vector<std::pair<Instruction *, std::vector<unsigned>>> Allocations;

  // Search for allocations.
  for (Instruction &I : instructions(F)) {
    // Visit VLA allocations.
    if (auto *Alloca = dyn_cast<AllocaInst>(&I)) {
      if (!Alloca->isStaticAlloca() && Alloca->isArrayAllocation()) {
        std::vector<unsigned> UncheckedOperandList = {0};
        Allocations.emplace_back(Alloca, std::move(UncheckedOperandList));
      }
      continue;
    }

    // Visit memory allocation callsites marked up with allocsize(n,m) info.
    auto *Call = dyn_cast<CallBase>(&I);
    if (!Call)
      continue;
    auto *Fn = Call->getCalledFunction();
    if (!Fn ||
        !Fn->hasAttribute(AttributeList::FunctionIndex, Attribute::AllocSize))
      continue;
    Attribute SizeAttr = Fn->getFnAttribute(Attribute::AllocSize);
    auto SizeArgs = SizeAttr.getAllocSizeArgs();
    std::vector<unsigned> UncheckedOperandList = {SizeArgs.first};
    if (SizeArgs.second)
      UncheckedOperandList.push_back(*SizeArgs.second);
    Allocations.emplace_back(&I, std::move(UncheckedOperandList));
  }

  // Cache of overflow checks.
  ValueMap<Value *, Value *> OverflowMap;

  // Insert overflow checks for allocations.
  for (const auto &Alloc : Allocations)
    Changed |=
        emitOverflowChecksForOperands(Alloc.first, Alloc.second, OverflowMap);

  // Simplify chains of `or` instructions and constant-fold where possible.
  if (Changed)
    for (BasicBlock &BB : F)
      SimplifyInstructionsInBlock(&BB, &TLI);

  return Changed;
}

class ProtectLibcallsLegacyPass : public FunctionPass {
public:
  static char ID;

  ProtectLibcallsLegacyPass() : FunctionPass(ID) {}

  StringRef getPassName() const override {
    return "ProtectLibcallsFunctionPass";
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<TargetLibraryInfoWrapperPass>();
  }

  bool runOnFunction(Function &F) override {
    const TargetLibraryInfo &TLI =
        getAnalysis<TargetLibraryInfoWrapperPass>().getTLI(F);
    return protectLibcalls(F, TLI);
  }
};

char ProtectLibcallsLegacyPass::ID = 0;

INITIALIZE_PASS_BEGIN(ProtectLibcallsLegacyPass, DEBUG_TYPE, "ProtectLibcalls",
                      false, false)
INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
INITIALIZE_PASS_END(ProtectLibcallsLegacyPass, DEBUG_TYPE, "ProtectLibcalls",
                    false, false)

static RegisterPass<ProtectLibcallsLegacyPass>
    RP("protect-libcalls", "Add traps around unsafe libcall usage");

namespace llvm {
FunctionPass *createProtectLibcallsFunctionPass() {
  return new ProtectLibcallsLegacyPass();
}

PreservedAnalyses NewPMProtectLibcallsPass::run(Function &F,
                                                FunctionAnalysisManager &AM) {
  const TargetLibraryInfo &TLI = AM.getResult<TargetLibraryAnalysis>(F);
  if (protectLibcalls(F, TLI))
    return PreservedAnalyses::none();
  return PreservedAnalyses::all();
}
} // namespace llvm
