//===-- SizeInfo.h - Semantic code size analysis/diffing -------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/DebugInfo/DIContext.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/StringSaver.h"
#include "llvm/Support/raw_ostream.h"
#include <vector>

using namespace llvm;
using namespace object;

/// A kind of code group.
enum class CodeGroupKind : unsigned {
  Function,
  Class,
  File,
  InliningTarget,

  // Invalid kinds. Used internally by DenseMap.
  Empty,
  Tombstone,
};

/// A key which uniquely identifies a code group.
class CodeGroupKey {
  StringRef Name;

  CodeGroupKind Kind;

public:
  CodeGroupKey(StringRef Name, CodeGroupKind Kind) : Name(Name), Kind(Kind) {
    assert(!Name.empty() ||
           (Kind == CodeGroupKind::Empty || Kind == CodeGroupKind::Tombstone) &&
               "Invalid name");
  }

  StringRef getName() const { return Name; }

  CodeGroupKind getKind() const { return Kind; }

  bool equals(const CodeGroupKey &Other) const {
    return std::tie(Name, Kind) == std::tie(Other.Name, Other.Kind);
  }

  void dump(raw_ostream &OS) const;
};

template <> struct DenseMapInfo<CodeGroupKey> {
  static inline CodeGroupKey getEmptyKey() {
    return {{}, CodeGroupKind::Empty};
  }

  static inline CodeGroupKey getTombstoneKey() {
    return {{}, CodeGroupKind::Tombstone};
  }

  static unsigned getHashValue(const CodeGroupKey &Val) {
    return DenseMapInfo<void *>::getHashValue(Val.getName().data());
  }

  static bool isEqual(const CodeGroupKey &LHS, const CodeGroupKey &RHS) {
    return LHS.equals(RHS);
  }
};

/// A group of code: it has a unique key (\ref CodeGroupKey), a size, and a set
/// of sub-groups.
class CodeGroup {
  CodeGroupKey Key;

  uint64_t SizeInBytes = 0;

  SmallPtrSet<CodeGroup *, 1> SubGroups = {};

  bool Finalized = false;

public:
  CodeGroup(CodeGroupKey Key) : Key(Key) {}

  CodeGroupKey getKey() const { return Key; }

  /// Attribute code size between \p LowPC and \p HighPC to this code group.
  void addPCRange(uint64_t LowPC, uint64_t HighPC);

  /// Get the size in bytes of a finalized group (\ref CodeGroup::finalize).
  uint64_t getSize() const;

  /// Register a sub-group of this code group. This must not induce a cycle.
  void addSubGroup(CodeGroup &CG);

  const SmallPtrSetImpl<CodeGroup *> &getSubGroups() const { return SubGroups; }

  /// Finalize a code group. This disallows modifications to the group.
  void finalize();
};

/// A shared context for string data. This is used to intern StringRefs backed
/// by transient DWARF contexts. It also enables cheap comparison of StringRefs.
struct StringContext {
  BumpPtrAllocator Alloc;

  UniqueStringSaver Strings{Alloc};
};

using CodeGroupWeighter = const std::function<int64_t(CodeGroup &)> &;

/// A collection of statistics about code groups within a program.
class SizeInfoStats {
  StringContext &StrCtx;

  DenseMap<CodeGroupKey, std::unique_ptr<CodeGroup>> CodeGroups;

  DenseMap<const DWARFDebugInfoEntry *, Optional<StringRef>> QualifiedNameCache;

  bool Finalized = false;

public:
  SizeInfoStats(StringContext &StrCtx) : StrCtx(StrCtx) {}

  /// Collect size information for code groups represented within \p CUDie.
  void collectSizeInfoInCU(DWARFDie CUDie);

  /// Finalize code groups.
  void finalize();

  void emitStats(StringRef StatsDir) const;

  void emitDiffstats(SizeInfoStats &Baseline, StringRef StatsDir) const;

private:
  StringRef intern(StringRef S);

  /// Get or create a code group keyed by \p Name and \p Kind.
  CodeGroup &getOrCreateCodeGroup(StringRef Name, CodeGroupKind Kind);

  void collectSizeInfoInDIE(DWARFDie ParentDie, CodeGroup &FileCG);

  void collectSizeInfoInFunction(DWARFDie FuncDie, CodeGroup &FileCG);

  void addToClassCodeGroup(CodeGroup &FuncCG, DWARFDie FuncDie);

  void recordInlinedInstance(DWARFDie InlinedDie, DWARFDie ParentDie);

  void recordInheritanceInfo(DWARFDie ClassDie);

  Optional<StringRef> getQualifiedName(DWARFDie Die);

  void emitStats(StringRef StatsDir, CodeGroupWeighter getCodeGroupWeight,
                 bool IsDiff) const;
};

/// Recursively collect code size information from a DIE and organize it within
/// a code group hierarchy.
bool collectSizeInfo(SizeInfoStats &SizeStats, ObjectFile &,
                     DWARFContext &DICtx, Twine, raw_ostream &);
