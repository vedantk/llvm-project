//===----------------- LLDBAssert.h ------------------------------*- C++-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLDB_UTILITY_LLDBASSERT_H
#define LLDB_UTILITY_LLDBASSERT_H

// lldbassert(x):
//
// In Debug mode, this is just a regular assert. However, code should NOT
// assume that the asserted condition is actually true.
//
// In Release mode, this reports a message to the user. If telemetry is
// enabled, a counter associated with the assertion text is incremented each
// time the assertion fails.
#ifdef LLDB_CONFIGURATION_DEBUG
#define lldbassert(x) assert(x)
#else
#define lldbassert(x)                                                          \
  lldb_private::lldb_assert(static_cast<bool>(x), #x, __FUNCTION__, __FILE__,  \
                            __LINE__)
#endif

namespace lldb_private {
void lldb_assert(bool expression, const char *expr_text, const char *func,
                 const char *file, unsigned int line);
}

#endif // LLDB_UTILITY_LLDBASSERT_H
