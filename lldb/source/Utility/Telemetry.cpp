//===-- Telemetry.cpp -----------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "lldb/Utility/Telemetry.h"

#include "lldb/Utility/Stream.h"
#include "lldb/Utility/StructuredData.h"

#include <cassert>
#include <memory>
#include <utility>

namespace lldb_private {

static unsigned GetNumberOfStatistics() {
  return static_cast<unsigned>(Statistic::MaxID);
}

static unsigned GetCounterIndexForStatistic(Statistic S) {
  assert(S != Statistic::MaxID && "invalid stat");
  return static_cast<unsigned>(S);
}

static Statistic GetStatisticForCounterIndex(unsigned index) {
  assert(index < GetNumberOfStatistics() && "invalid stat");
  return static_cast<Statistic>(index);
}

static const char *GetStatisticDescription(Statistic S) {
  switch (S) {
#define LLDB_STATISTIC_DEF(EnumName, Desc)                                     \
  case Statistic::EnumName:                                                    \
    return Desc;
#include "lldb/Utility/Statistics.def"
  case Statistic::MaxID:
    llvm_unreachable("invalid stat");
  }
}

Telemetry::Telemetry() { m_counters.resize(GetNumberOfStatistics()); }

void Telemetry::Record(Statistic stat) {
  if (!m_collecting_stats)
    return;
  ++m_counters[GetCounterIndexForStatistic(stat)];
}

void Telemetry::RecordWithString(Statistic stat, const char *str) {
  if (!m_collecting_stats)
    return;
  Record(stat);
  ++m_strings[str];
}

void Telemetry::Print(Stream &stream) const {
  for (unsigned stat_id = 0; stat_id < GetNumberOfStatistics(); ++stat_id) {
    Statistic s = GetStatisticForCounterIndex(stat_id);
    stream.Printf("%s : %u\n", GetStatisticDescription(s), m_counters[stat_id]);
  }
  for (const auto &string_and_count : m_strings)
    stream.Printf("Assertion (%s) : %u\n", string_and_count.first,
                  string_and_count.second);
}

std::unique_ptr<StructuredData::Dictionary>
Telemetry::GetAsStructuredData() const {
  auto stats_up = std::make_unique<StructuredData::Dictionary>();
  for (unsigned stat_id = 0; stat_id < GetNumberOfStatistics(); ++stat_id) {
    Statistic s = GetStatisticForCounterIndex(stat_id);
    stats_up->AddIntegerItem(GetStatisticDescription(s), m_counters[stat_id]);
  }
  for (const auto &string_and_count : m_strings)
    stats_up->AddIntegerItem(string_and_count.first, string_and_count.second);
  return stats_up;
}

} // namespace lldb_private
