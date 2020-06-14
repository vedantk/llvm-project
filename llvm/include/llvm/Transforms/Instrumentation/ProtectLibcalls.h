//===------------ Definition of the ProtectLibcalls pass --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// \file This file defines the ProtectLibcalls pass.
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_TRANSFORMS_INSTRUMENTATION_PROTECTLIBCALLS_H
#define LLVM_TRANSFORMS_INSTRUMENTATION_PROTECTLIBCALLS_H

#include "llvm/IR/PassManager.h"

namespace llvm {

class Function;

FunctionPass *createProtectLibcallsFunctionPass();

struct NewPMProtectLibcallsPass
    : public PassInfoMixin<NewPMProtectLibcallsPass> {
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
};

} // namespace llvm

#endif
