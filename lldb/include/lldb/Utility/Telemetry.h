//===-- Telemetry.h ---------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLDB_UTILITY_TELEMETRY_H
#define LLDB_UTILITY_TELEMETRY_H

#include "lldb/Utility/StructuredData.h"

#include <mutex>
#include <unordered_map>
#include <vector>

namespace lldb_private {

class Stream;

/// A kind of statistic to collect.
enum class Statistic : unsigned {
#define LLDB_STATISTIC_DEF(EnumName, Desc) EnumName,
#include "lldb/Utility/Statistics.def"
  MaxID
};

/// Keep track of statistics. Data collection is off by default.
class Telemetry {
public:
  Telemetry();

  void SetEnabled(bool v) { m_collecting_stats = v; }

  bool IsEnabled() const { return m_collecting_stats; }

  void Record(Statistic stat);

  void RecordWithString(Statistic stat, const char *assertion);

  void Print(Stream &stream) const;

  std::unique_ptr<StructuredData::Dictionary> GetAsStructuredData() const;

  /// Used by \ref SystemInitializerFull to set up telemetry.
  static void Initialize();

  /// Used by \ref SystemInitializerFull to tear down telemetry.
  static void Terminate();

private:
  std::vector<unsigned> m_counters;
  std::unordered_map<const char *, unsigned> m_strings;
  bool m_collecting_stats = false;
};

/// RAII helper which acquires an exlusive lock on the global Telemetry
/// singleton on construction, and releases the lock on destruction.
class ExclusivelyLockedTelemetry {
public:
  ExclusivelyLockedTelemetry();

  ~ExclusivelyLockedTelemetry();

  ExclusivelyLockedTelemetry(ExclusivelyLockedTelemetry &&) = default;

  ExclusivelyLockedTelemetry &
  operator=(ExclusivelyLockedTelemetry &&) = default;

  Telemetry *operator->() const;

private:
  std::unique_lock<std::mutex> m_guard;
};

/// Get a reference to the global Telemetry singleton. This has UB when called
/// without first calling \ref Telemetry::Initialize.
ExclusivelyLockedTelemetry GetTelemetry();

}

#endif // LLDB_UTILITY_TELEMETRY_H
