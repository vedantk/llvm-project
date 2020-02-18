//===- llvm/ADT/CoalescingBitVector.h - A coalescing bitvector --*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file A bitvector that uses an IntervalMap to coalesce adjacent elements
/// into intervals.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_ADT_COALESCINGBITVECTOR_H
#define LLVM_ADT_COALESCINGBITVECTOR_H

#include "llvm/ADT/IntervalMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include <initializer_list>
#include <memory>

namespace llvm {

/// A bitvector that, under the hood, relies on an IntervalMap to coalesce
/// elements into intervals. Good for representing sets which predominantly
/// contain contiguous ranges. Bad for representing sets with lots of gaps
/// between elements. The first N coalesced intervals of set bits are stored
/// in-place (in the initial heap allocation).
///
/// Compared to SparseBitVector, CoalescingBitVector offers more predictable
/// performance for non-sequential find() operations.
template <typename EltT, unsigned N = 16> class CoalescingBitVector {
  using ThisT = CoalescingBitVector<EltT, N>;

  /// An interval map for closed integer ranges. The mapped values are unused.
  using MapT = IntervalMap<EltT, char, N>;

  using UnderlyingIterator = typename MapT::const_iterator;

  using IntervalT = std::pair<EltT, EltT>;

public:
  using Allocator = typename MapT::Allocator;

  /// Construct by passing in a CoalescingBitVector<EltT>::Allocator reference.
  CoalescingBitVector(Allocator &Alloc)
      : Alloc(&Alloc), Intervals(std::make_unique<MapT>(Alloc)) {}

  /// \name Copy/move constructors and assignment operators.
  /// @{

  CoalescingBitVector(const ThisT &RHS)
      : Alloc(RHS.Alloc), Intervals(std::make_unique<MapT>(*RHS.Alloc)) {
    initFromRHS(RHS);
  }

  ThisT &operator=(const ThisT &RHS) {
    clear();
    initFromRHS(RHS);
    return *this;
  }

  CoalescingBitVector(ThisT &&RHS)
      : Alloc(RHS.Alloc), Intervals(std::move(RHS.Intervals)) {}

  ThisT &operator=(ThisT &&RHS) {
    Alloc = RHS.Alloc;
    Intervals = std::move(RHS.Intervals);
    return *this;
  }

  /// @}

  /// Clear all the bits.
  void clear() { Intervals->clear(); }

  /// Check whether no bits are set.
  bool empty() const { return Intervals->empty(); }

  /// Count the number of set bits.
  unsigned count() const {
    unsigned Bits = 0;
    for (auto It = Intervals->begin(), End = Intervals->end(); It != End; ++It)
      Bits += 1 + It.stop() - It.start();
    return Bits;
  }

  /// Set the bit at \p Index.
  ///
  /// This method does /not/ support setting a bit that has already been set,
  /// for efficiency reasons. If possible, restructure your code to not set the
  /// same bit multiple times, or use the test-and-set idiom.
  void set(EltT Index) { insert(Index, Index); }

  /// Set the bits at \p Indices. Used for testing, primarily.
  void set(std::initializer_list<EltT> Indices) {
    for (EltT Index : Indices)
      set(Index);
  }

  /// Check whether the bit at \p Index is set.
  bool test(EltT Index) const {
    const auto It = Intervals->find(Index);
    if (It == Intervals->end())
      return false;
    assert(It.stop() >= Index && "Interval must end after Index");
    return It.start() <= Index;
  }

  /// Reset the bit at \p Index. Does not support resetting a bit that was not
  /// set. See \ref set for the rationale.
  void reset(EltT Index) {
    auto It = Intervals->find(Index);
    if (It == Intervals->end())
      return;

    // Split the interval containing Index into up to two parts: one from
    // [Start, Index-1] and another from [Index+1, Stop]. If Index is equal to
    // either Start or Stop, we create one new interval. If Index is equal to
    // both Start and Stop, we simply erase the existing interval.
    EltT Start = It.start();
    EltT Stop = It.stop();
    assert(Start <= Index && Index <= Stop &&
           "Wrong interval for index, was the bit at Index set?");
    It.erase();
    if (Start < Index)
      insert(Start, Index - 1);
    if (Index < Stop)
      insert(Index + 1, Stop);
  }

  /// Set union.
  void operator|=(const ThisT &RHS) { initFromRHS(RHS); }

  /// Set intersection.
  void operator&=(const ThisT &RHS) {
    // Get the overlaps between the two interval maps (i.e. the intersection).
    SmallVector<IntervalT, 8> Overlaps;
    getOverlaps(RHS, Overlaps);
    // Rebuild the interval map, including only the overlaps.
    clear();
    for (IntervalT Overlap : Overlaps)
      insert(Overlap.first, Overlap.second);
  }

  /// Reset all bits present in \p RHS.
  bool intersectWithComplement(const ThisT &RHS) {
    SmallVector<IntervalT, 8> Overlaps;
    if (!getOverlaps(RHS, Overlaps)) {
      // If there is no overlap with RHS, the intersection is empty.
      return false;
    }

    // Delete the overlapping intervals. Split up intervals that only partially
    // intersect an overlap.
    for (IntervalT Overlap : Overlaps) {
      EltT OlapStart, OlapStop;
      std::tie(OlapStart, OlapStop) = Overlap;

      auto It = Intervals->find(OlapStart);
      EltT CurrStart = It.start();
      EltT CurrStop = It.stop();
      assert(CurrStart <= OlapStart && OlapStop <= CurrStop &&
             "Expected some intersection!");

      // Split the overlap interval into up to two parts: one from [CurrStart,
      // OlapStart-1] and another from [OlapStop+1, CurrStop]. If OlapStart is
      // equal to CurrStart, the first split interval is unnecessary. Ditto for
      // when OlapStop is equal to CurrStop, we omit the second split interval.
      It.erase();
      if (CurrStart < OlapStart)
        insert(CurrStart, OlapStart - 1);
      if (OlapStop < CurrStop)
        insert(OlapStop + 1, CurrStop);
    }
    return true;
  }

  bool operator==(const ThisT &RHS) const {
    // We cannot just use std::equal because it checks the dereferenced values
    // of an iterator pair for equality, not the iterators themselves. In our
    // case that results in comparison of the (unused) IntervalMap values.
    auto ItL = Intervals->begin();
    auto ItR = RHS.Intervals->begin();
    while (ItL != Intervals->end() && ItR != RHS.Intervals->end() &&
           ItL.start() == ItR.start() && ItL.stop() == ItR.stop()) {
      ++ItL;
      ++ItR;
    }
    return ItL == Intervals->end() && ItR == RHS.Intervals->end();
  }

  bool operator!=(const ThisT &RHS) const { return !operator==(RHS); }

  class const_iterator : public std::iterator<std::forward_iterator_tag, EltT> {
    friend class CoalescingBitVector;

    // For performance reasons, make the offset at the end different than the
    // one used in \ref begin, to optimize the common `It == end()` pattern.
    static constexpr unsigned kIteratorAtTheEndOffset = ~0u;

    UnderlyingIterator MapIterator;
    unsigned OffsetIntoMapIterator = 0;

    // Querying the start/stop of an IntervalMap iterator can be very expensive.
    // Cache these values for performance reasons.
    EltT CachedStart = EltT();
    EltT CachedStop = EltT();

    void setToEnd() {
      OffsetIntoMapIterator = kIteratorAtTheEndOffset;
      CachedStart = EltT();
      CachedStop = EltT();
    }

    /// MapIterator has just changed, reset the cached state to point to the
    /// start of the new underlying iterator.
    void resetCache() {
      if (MapIterator.valid()) {
        OffsetIntoMapIterator = 0;
        CachedStart = MapIterator.start();
        CachedStop = MapIterator.stop();
      } else {
        setToEnd();
      }
    }

    /// Advance the iterator to \p Index, if it is contained within the current
    /// interval.
    void advanceTo(EltT Index) {
      assert(OffsetIntoMapIterator == 0 && "Not implemented");
      assert(Index <= CachedStop && "Cannot advance to OOB index");
      if (Index < CachedStart)
        // We're already past this index.
        return;
      OffsetIntoMapIterator = Index - CachedStart;
    }

    const_iterator(UnderlyingIterator MapIt) : MapIterator(MapIt) {
      resetCache();
    }

  public:
    const_iterator() { setToEnd(); }

    bool operator==(const const_iterator &RHS) const {
      // Do /not/ compare MapIterator for equality, as this is very expensive.
      // The cached start/stop values make that check unnecessary.
      return std::tie(OffsetIntoMapIterator, CachedStart, CachedStop) ==
             std::tie(RHS.OffsetIntoMapIterator, RHS.CachedStart,
                      RHS.CachedStop);
    }

    bool operator!=(const const_iterator &RHS) const {
      return !operator==(RHS);
    }

    EltT operator*() const {
      return CachedStart + OffsetIntoMapIterator;
    }

    const_iterator &operator++() { // Pre-increment (++It).
      if (CachedStart + OffsetIntoMapIterator < CachedStop) {
        // Keep going within the current interval.
        ++OffsetIntoMapIterator;
      } else {
        // We reached the end of the current interval: advance.
        ++MapIterator;
        resetCache();
      }
      return *this;
    }

    const_iterator operator++(int) { // Post-increment (It++).
      const_iterator tmp = *this;
      operator++();
      return tmp;
    }
  };

  const_iterator begin() const { return const_iterator(Intervals->begin()); }

  const_iterator end() const { return const_iterator(); }

  /// Return an iterator pointing to the first set bit AT, OR AFTER, \p Index.
  /// If no such set bit exists, return end(). This is like std::lower_bound.
  const_iterator find(EltT Index) const {
    auto UnderlyingIt = Intervals->find(Index);
    if (UnderlyingIt == Intervals->end())
      return end();
    auto It = const_iterator(UnderlyingIt);
    It.advanceTo(Index);
    return It;
  }

  void print(raw_ostream &OS) const {
    // LLDB swallows the first line of output after callling dump(). Add
    // newlines before/after the braces to work around this.
    OS << "\n{";
    for (auto It = Intervals->begin(), End = Intervals->end(); It != End;
         ++It) {
      OS << "[" << It.start();
      if (It.start() != It.stop())
        OS << ", " << It.stop();
      OS << "]";
    }
    OS << "}\n";
  }

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
  LLVM_DUMP_METHOD void dump() const { print(dbgs()); }
#endif

private:
  void initFromRHS(const ThisT &RHS) {
    for (auto It = RHS.Intervals->begin(), End = RHS.Intervals->end();
         It != End; ++It)
      insert(It.start(), It.stop());
  }

  void insert(EltT Start, EltT End) { Intervals->insert(Start, End, 0); }

  /// Record the overlaps between \p this and \p RHS in \p Overlaps. Return
  /// true if there is any overlap.
  bool getOverlaps(const ThisT &RHS,
                   SmallVectorImpl<IntervalT> &Overlaps) const {
    for (IntervalMapOverlaps<MapT, MapT> I(*Intervals, *RHS.Intervals);
         I.valid(); ++I)
      Overlaps.emplace_back(I.start(), I.stop());
    return !Overlaps.empty();
  }

  Allocator *Alloc;
  std::unique_ptr<MapT> Intervals;
};

} // namespace llvm

#endif // LLVM_ADT_COALESCINGBITVECTOR_H
