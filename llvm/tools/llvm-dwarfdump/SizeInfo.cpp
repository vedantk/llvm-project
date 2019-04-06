//===-- SizeInfo.cpp - Semantic code size analysis/diffing -----*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "SizeInfo.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include <forward_list>

using namespace dwarf;

//=== Basic code group representation -------------------------------------===//

void CodeGroup::updatePCRange(uint64_t LowPC, uint64_t HighPC) {
  assert(!Finalized && "Cannot add PC range");
  assert(LowPC <= HighPC && "Invalid PC range");
  SizeInBytes = std::max(SizeInBytes, HighPC - LowPC);
}

void CodeGroup::addPCRange(uint64_t LowPC, uint64_t HighPC) {
  assert(!Finalized && "Cannot add PC range");
  assert(LowPC <= HighPC && "Invalid PC range");
  SizeInBytes += HighPC - LowPC;
}

uint64_t CodeGroup::getSize() const {
  assert(Finalized && "Size is not available");
  return SizeInBytes;
}

void CodeGroup::addSubGroup(CodeGroup &CG) {
  assert(!Finalized && !CG.Finalized && "Sub-group cannot be added");
  SubGroups.insert(&CG);
}

void CodeGroup::finalize() {
  if (Finalized)
    return;

  // If this group has a non-zero size, or it has size zero and is an inlining
  // target, do not visit any sub-groups.
  if (SizeInBytes || getKind() == CodeGroupKind::InliningTarget) {
    Finalized = true;
    return;
  }

  for (CodeGroup *SubGroup : SubGroups) {
    SubGroup->finalize();
    SizeInBytes += SubGroup->getSize();
  }
  Finalized = true;
}

//=== Parsing DWARF to form code groups -----------------------------------===//

#ifndef NDEBUG
/// Check whether adding \p Derived as a sub-group of \p Base would induce a
/// cycle in the graph.
static bool detectCycle(CodeGroup &Derived, CodeGroup &Base) {
  if (&Derived == &Base)
    return true;

  if (Derived.getSubGroups().count(&Base))
    return true;

  for (CodeGroup *SubGroup : Derived.getSubGroups())
    if (detectCycle(*SubGroup, Base))
      return true;

  return false;
}
#endif

static bool isClassOrStruct(dwarf::Tag Tag) {
  return Tag == DW_TAG_structure_type || Tag == DW_TAG_class_type;
}

/// Get the fully-qualified name of a definition (ignore AT_specification).
/// Return None if the name cannot be constructed.
Optional<StringRef> SizeInfoStats::getQualifiedName(DWARFDie Die) {
  // Look up the cached fully-qualified name, creating a cache entry if one
  // is not present.
  auto CacheEntry =
      QualifiedNameCache.try_emplace(Die.getDebugInfoEntry(), None);

  // Cache hit.
  if (!CacheEntry.second)
    return CacheEntry.first->second;

  // Walk up to the root of the DIE tree to collect name components.
  std::forward_list<StringRef> Components;
  while (Die.isValid()) {
    StringRef Name = Die.getName(DINameKind::LinkageName);
    if (Name.empty()) {
      if (Die.getTag() == DW_TAG_namespace)
        Name = "(anon)";
      else
        // A name might be empty because, e.g, the DIE describes the class
        // implementing a lambda closure, or the DIE is a forward-declaration.
        break;
    }

    Components.emplace_front(Name);

    // Advance to the parent. Only class/struct/namespace qualifiers.
    Die = Die.getParent();
    auto Tag = Die.getTag();
    if (!isClassOrStruct(Tag) && Tag != DW_TAG_namespace)
      break;
  }

  // Update the cache entry if a fully-qualified name is found.
  if (!Components.empty())
    CacheEntry.first->second = intern(join(Components, "::"));

  return CacheEntry.first->second;
}

/// Add a class as a sub-group of other classes it inherits from.
void SizeInfoStats::recordInheritanceInfo(DWARFDie ClassDie) {
  Optional<StringRef> Classname = getQualifiedName(ClassDie);
  if (!Classname)
    return;

  CodeGroup &ClassCG = getOrCreateCodeGroup(*Classname, CodeGroupKind::Class);
  for (DWARFDie ChildDie : ClassDie.children()) {
    if (ChildDie.getTag() != DW_TAG_inheritance)
      continue;

    DWARFDie InheritedType =
        ChildDie.getAttributeValueAsReferencedDie(DW_AT_type);
    assert(InheritedType && "DW_TAG_inheritance without base type");
    Optional<StringRef> ParentClassname = getQualifiedName(InheritedType);
    if (!ParentClassname)
      continue;
    CodeGroup &ParentClassCG =
        getOrCreateCodeGroup(*ParentClassname, CodeGroupKind::Class);
    assert(!detectCycle(ClassCG, ParentClassCG));
    ParentClassCG.addSubGroup(ClassCG);
  }
}

/// Record an inlined instance of a function.
void SizeInfoStats::recordInlinedInstance(DWARFDie InlinedDie,
                                          DWARFDie ParentDie) {
  DWARFDie Origin =
      InlinedDie.getAttributeValueAsReferencedDie(DW_AT_abstract_origin);
  assert(Origin && "DW_TAG_inlined_subroutine without origin");

  // Create a record for this inlined subroutine.
  auto InlinedFunction = getQualifiedName(Origin);
  if (!InlinedFunction)
    return;
  CodeGroup &CG =
      getOrCreateCodeGroup(*InlinedFunction, CodeGroupKind::InliningTarget);
  if (auto RangesOrErr = InlinedDie.getAddressRanges()) {
    for (DWARFAddressRange Range : RangesOrErr.get())
      CG.addPCRange(Range.LowPC, Range.HighPC);
  } else {
    consumeError(RangesOrErr.takeError());
    return;
  }

  // An inlined function may itself contain inlined code. Add inlining targets
  // as sub-groups of the inlining target they're nested within.
  if (ParentDie.getTag() == DW_TAG_inlined_subroutine) {
    DWARFDie ParentOrigin =
        ParentDie.getAttributeValueAsReferencedDie(DW_AT_abstract_origin);
    auto ParentInlinedFunction = getQualifiedName(ParentOrigin);
    if (!ParentInlinedFunction)
      return;
    CodeGroup &ParentCG = getOrCreateCodeGroup(*ParentInlinedFunction,
                                               CodeGroupKind::InliningTarget);
    ParentCG.addSubGroup(CG);
  }
}

/// Find the unqualified root type of \p Die.
static DWARFDie findRootType(DWARFDie Die) {
  DWARFDie TypeDie = Die.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
  while (true) {
    DWARFDie UnderlyingTypeDie =
        TypeDie.getAttributeValueAsReferencedDie(dwarf::DW_AT_type);
    if (UnderlyingTypeDie)
      TypeDie = UnderlyingTypeDie;
    else
      break;
  }
  return TypeDie;
}

/// Add \p FuncCG to its innermost surrounding class's code group.
void SizeInfoStats::addToClassCodeGroup(CodeGroup &FuncCG, DWARFDie FuncDie) {
  auto AddToClassCG = [&](DWARFDie ClassDie) {
    Optional<StringRef> Classname = getQualifiedName(ClassDie);
    if (Classname) {
      CodeGroup &ClassCG =
          getOrCreateCodeGroup(*Classname, CodeGroupKind::Class);
      ClassCG.addSubGroup(FuncCG);
      return;
    }
  };

  // The function may have an AT_object_pointer field, which in turn has
  // the right type.
  DWARFDie ObjDie =
      FuncDie.getAttributeValueAsReferencedDie(DW_AT_object_pointer);
  if (ObjDie) {
    if (DWARFDie TypeDie = findRootType(ObjDie)) {
      if (isClassOrStruct(TypeDie.getTag())) {
        AddToClassCG(TypeDie);
        return;
      }
    }
  }

  // The function may be nested within a class/struct definition. Find that
  // definition.
  DWARFDie ParentDie = FuncDie.getParent();
  for (; ParentDie.isValid(); ParentDie = ParentDie.getParent()) {
    dwarf::Tag Tag = ParentDie.getTag();
    if (Tag == DW_TAG_subprogram || Tag == DW_TAG_compile_unit)
      return;
    if (isClassOrStruct(Tag)) {
      AddToClassCG(ParentDie);
      return;
    }
  }
}

/// Create a code group for a subprogram definition/declaration and attribute
/// a PC range to this code group if one is present.
void SizeInfoStats::collectSizeInfoInFunction(DWARFDie FuncDie,
                                              CodeGroup &FileCG) {
  StringRef Funcname = FuncDie.getName(DINameKind::LinkageName);
  if (Funcname.empty())
    // No function name. Ignore declarations.
    return;

  uint64_t LowPC, HighPC, SectionIdx;
  bool FoundPCRange = FuncDie.getLowAndHighPC(LowPC, HighPC, SectionIdx);
  CodeGroup &FuncCG = getOrCreateCodeGroup(Funcname, CodeGroupKind::Function);
  if (FoundPCRange) {
    // Attribute the function's size to its defining file.
    FuncCG.updatePCRange(LowPC, HighPC);
    FileCG.addSubGroup(FuncCG);
  }

  // Add the function to its enclosing class if there is one, even if the
  // function is a delcaration. It may not be possible to find the enclosing
  // class from the definition DIE.
  addToClassCodeGroup(FuncCG, FuncDie);
}

/// A recursive helper for \ref collectSizeInfo which drills into subprograms
/// and classes.
void SizeInfoStats::collectSizeInfoInDIE(DWARFDie ParentDie,
                                         CodeGroup &FileCG) {
  for (DWARFDie Die : ParentDie.children()) {
    if (Die.isSubprogramDIE()) {
      collectSizeInfoInFunction(Die, FileCG);
    } else if (Die.getTag() == DW_TAG_inlined_subroutine) {
      recordInlinedInstance(Die, ParentDie);
    } else if (Die.getTag() == DW_TAG_class_type ||
               Die.getTag() == DW_TAG_structure_type) {
      recordInheritanceInfo(Die);
    }
    collectSizeInfoInDIE(Die, FileCG);
  }
}

void SizeInfoStats::collectSizeInfoInCU(DWARFDie CUDie) {
  std::unique_lock<std::mutex> Guard = lock();
  QualifiedNameCache.clear();
  StringRef Filename = CUDie.getName(DINameKind::LinkageName);
  CodeGroup &FileCG = getOrCreateCodeGroup(Filename, CodeGroupKind::File);
  collectSizeInfoInDIE(CUDie, FileCG);
}

void SizeInfoStats::incorporate(SizeInfoStats &Other) {
  assert(!Other.Finalized && "Cannot incorporate");

  std::vector<std::unique_ptr<CodeGroup>> SharedGroups;
  for (auto &Entry : Other.CodeGroups) {
    // The other group's key is new. Map that key to the new group. Any new
    // sub-groups will be fixed up later.
    CodeGroup *OtherGroup = Entry.second.get();
    std::unique_ptr<CodeGroup> &CG = CodeGroups[OtherGroup->getKey()];
    if (!CG) {
      CG = std::move(Entry.second);
      continue;
    }

    // Set the size of shared groups to the maximum known size.
    CG->updatePCRange(0, OtherGroup->SizeInBytes);

    // Add all the sub-groups for shared groups. There may be some duplication
    // here, but it will be fixed up later.
    for (CodeGroup *OtherSubGroup : OtherGroup->getSubGroups())
      CG->addSubGroup(*OtherSubGroup);
  }

  // Fix up all sub-groups. I.e., replace groups owned by \p Other with the
  // newly-created/updated groups owned by \p this.
  for (auto &Entry : CodeGroups) {
    CodeGroup *CG = Entry.second.get();
    SmallDenseSet<CodeGroupKey> Keys;
    for (CodeGroup *SubGroup : CG->getSubGroups())
      Keys.insert(SubGroup->getKey());
    CG->SubGroups.clear();
    for (CodeGroupKey Key : Keys)
      CG->addSubGroup(*CodeGroups[Key].get());
  }
}

void SizeInfoStats::finalize() {
  assert(!Finalized && "Already finalized");
  for (auto &Entry : CodeGroups)
    Entry.second->finalize();
  Finalized = true;
}

StringRef SizeInfoStats::intern(StringRef S) {
  StringRef Result;
  while (StrCtx->Lock.test_and_set(std::memory_order_acquire))
    ;
  Result = StrCtx->Strings.save(S);
  StrCtx->Lock.clear(std::memory_order_release);
  return Result;
}

CodeGroup &SizeInfoStats::getOrCreateCodeGroup(StringRef Name,
                                               CodeGroupKind Kind) {
  assert(!Finalized && "Cannot modify finalized stats");
  Name = intern(Name);
  CodeGroupKey Key{Name, Kind};
  std::unique_ptr<CodeGroup> &CG = CodeGroups[Key];
  if (!CG)
    CG = make_unique<CodeGroup>(Key);
  return *CG.get();
}

bool collectSizeInfo(SizeInfoStats &SizeStats, ObjectFile &,
                     DWARFContext &DICtx, Twine, raw_ostream &) {
  for (const auto &CU : DICtx.compile_units())
    if (DWARFDie CUDie = CU->getUnitDIE(/*ExtractUnitDIEOnly=*/false))
      SizeStats.collectSizeInfoInCU(CUDie);
  outs() << ".";
  return true;
}

//=== Formatting ----------------------------------------------------------===//

static StringRef getKindAsString(CodeGroupKind Kind) {
  switch (Kind) {
  case CodeGroupKind::Function:
    return "function";
  case CodeGroupKind::Class:
    return "class";
  case CodeGroupKind::File:
    return "file";
  case CodeGroupKind::InliningTarget:
    return "inlining-target";
  default:
    llvm_unreachable("Invalid CodeGroupKind");
  }
}

void CodeGroupKey::dump(raw_ostream &OS) const {
  OS << getName() << " [" << getKindAsString(getKind()) << "]";
}

static void sortCodeGroups(std::vector<CodeGroup *> &Groups,
                           CodeGroupWeighter getCodeGroupWeight) {
  std::sort(Groups.begin(), Groups.end(), [&](CodeGroup *LHS, CodeGroup *RHS) {
    int64_t L = getCodeGroupWeight(*LHS);
    int64_t R = getCodeGroupWeight(*RHS);
    if (L != R)
      return L > R;
    return LHS->getKey().getName().compare(RHS->getKey().getName()) == -1;
  });
}

static void emitCodeGroupRecords(CodeGroup *CG,
                                 CodeGroupWeighter getCodeGroupWeight,
                                 std::vector<CodeGroup *> &Ancestors,
                                 SmallPtrSetImpl<CodeGroup *> &Visited,
                                 bool IsDiff, raw_ostream &OS) {
  if (!Visited.insert(CG).second)
    return;

  // For classes and inlining targets, diffing is not meaningful for sub-groups.
  bool HideSubGroups = (CG->getKind() == CodeGroupKind::InliningTarget ||
                        CG->getKind() == CodeGroupKind::Class) &&
                       IsDiff;

  std::vector<CodeGroup *> SortedSubGroups;
  if (!HideSubGroups) {
    // Sort sub-groups by size and filter out empty groups.
    SortedSubGroups.assign(CG->getSubGroups().begin(),
                           CG->getSubGroups().end());
    sortCodeGroups(SortedSubGroups, getCodeGroupWeight);
    while (!SortedSubGroups.empty() &&
           getCodeGroupWeight(*SortedSubGroups.back()) <= 0)
      SortedSubGroups.pop_back();
  }

  if (SortedSubGroups.empty()) {
    // Emit a semicolon-separated path from the root node to this leaf.
    for (CodeGroup *Ancestor : Ancestors) {
      Ancestor->getKey().dump(OS);
      if (Ancestor->getKind() == CodeGroupKind::InliningTarget)
        OS << " " << Ancestor->getSize();
      OS << ";";
    }
    CG->getKey().dump(OS);
    OS << " " << int64_t(getCodeGroupWeight(*CG)) << "\n";
    return;
  }

  // Update the list of ancestor code groups and recurse.
  Ancestors.push_back(CG);
  for (CodeGroup *SubGroup : SortedSubGroups)
    emitCodeGroupRecords(SubGroup, getCodeGroupWeight, Ancestors, Visited,
                         IsDiff, OS);
  Ancestors.pop_back();
}

static void emitFlamegraphFile(std::vector<CodeGroup *> &Groups,
                               CodeGroupWeighter getCodeGroupWeight,
                               StringRef StatsDir, StringRef Basename,
                               bool IsDiff) {
  sortCodeGroups(Groups, getCodeGroupWeight);

  SmallString<256> Filename;
  sys::path::append(Filename, StatsDir,
                    Twine("dwarfdump-size-info.") + Basename +
                        StringRef(IsDiff ? ".diffstats" : ".stats"));

  std::error_code Err;
  auto RawFdStream =
      make_unique<raw_fd_ostream>(Filename, Err, sys::fs::FA_Write);
  if (Err) {
    errs() << "Could not create flamegraph file: " << Filename << " ("
           << Err.message() << ")\n";
    exit(1);
  }
  raw_ostream &OS = *RawFdStream.get();

  std::vector<CodeGroup *> Ancestors;
  SmallPtrSet<CodeGroup *, 8> Visited;
  for (CodeGroup *CG : Groups)
    emitCodeGroupRecords(CG, getCodeGroupWeight, Ancestors, Visited, IsDiff,
                         OS);
}

void SizeInfoStats::emitStats(StringRef StatsDir,
                              CodeGroupWeighter getCodeGroupWeight,
                              bool IsDiff) const {
  // Create file, class, function, and inlining target views into the set of
  // code groups.
  std::vector<CodeGroup *> Files;
  std::vector<CodeGroup *> Classes;
  std::vector<CodeGroup *> Functions;
  std::vector<CodeGroup *> InliningTargets;
  for (auto &Entry : CodeGroups) {
    CodeGroup *CG = Entry.second.get();

    // Filter out groups with non-positive weight. This is used to suppress
    // output related to code groups which did not grow relative to a baseline,
    // or which are just empty.
    if (getCodeGroupWeight(*CG) <= 0)
      continue;

    switch (CG->getKind()) {
    case CodeGroupKind::File:
      Files.push_back(CG);
      continue;
    case CodeGroupKind::Class:
      Classes.push_back(CG);
      continue;
    case CodeGroupKind::Function:
      Functions.push_back(CG);
      continue;
    case CodeGroupKind::InliningTarget:
      InliningTargets.push_back(CG);
      continue;
    default:
      llvm_unreachable("Invalid CodeGroupKind");
    }
  }

  // Ensure that the stats dir exists.
  if (auto Err = sys::fs::create_directories(StatsDir)) {
    errs() << "Could not create stats dir: " << StatsDir << " ("
           << Err.message() << ")\n";
    exit(1);
  }

  // Emit flamegraph-compatible data files for each view.
  emitFlamegraphFile(Files, getCodeGroupWeight, StatsDir, "file-view", IsDiff);
  emitFlamegraphFile(Classes, getCodeGroupWeight, StatsDir, "class-view",
                     IsDiff);
  emitFlamegraphFile(Functions, getCodeGroupWeight, StatsDir, "function-view",
                     IsDiff);
  emitFlamegraphFile(InliningTargets, getCodeGroupWeight, StatsDir,
                     "inlining-view", IsDiff);
}

void SizeInfoStats::emitStats(StringRef StatsDir) const {
  emitStats(StatsDir, [](CodeGroup &CG) { return int64_t(CG.getSize()); },
            /*IsDiff=*/false);
}

void SizeInfoStats::emitDiffstats(SizeInfoStats &Baseline,
                                  StringRef StatsDir) const {
  emitStats(StatsDir, [&](CodeGroup &CG) {
    int64_t Target = int64_t(CG.getSize());
    auto BaselineIt = Baseline.CodeGroups.find(CG.getKey());

    // This code group doesn't exist in the baseline.
    if (BaselineIt == Baseline.CodeGroups.end())
      return 0LL;

    // Report the change in code size. (Non-positive changes are ignored.)
    int64_t Baseline = int64_t(BaselineIt->second.get()->getSize());
    return Target - Baseline;
  }, /*IsDiff=*/true);
}
