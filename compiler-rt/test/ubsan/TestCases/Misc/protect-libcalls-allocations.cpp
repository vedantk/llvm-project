// Compile with SIZE_T=int64_t.
// RUN: %clang -DSIZE_T=int64_t -w %s -O3 -o %t.signed
// Compile with SIZE_T=uint64_t.
// RUN: %clang -DSIZE_T=uint64_t -w %s -O3 -o %t.unsigned

// =============== Tests for SIZE_T=int64_t ===============

// malloc(0 + 1) = malloc(1)
// RUN: %t.signed 0 1
// malloc(0 + -1) = malloc(UINT_MAX)
// RUN: %t.signed 0 -1
// malloc(1 + -1) = malloc(0)
// RUN: %t.signed 1 -1
// malloc(-1 + 1) = malloc(0)
// RUN: %t.signed -1 1
// malloc(9223372036854775807 + 9223372036854775807) = trap
// RUN: not --crash %t.signed 9223372036854775807 9223372036854775807
// malloc(1 - 1) = malloc(0)
// RUN: %t.signed 1 0 1
// malloc(1 + 16 - 1) = malloc(16)
// RUN: %t.signed 1 16 1
// malloc(1 * 2) = malloc(2)
// RUN: %t.signed 1 0 0 2
// malloc(-1 * -2) = malloc(2)
// RUN: %t.signed -1 0 0 -2
// malloc(-1 - 7 * -2) = malloc(16)
// RUN: %t.signed -1 0 7 -2
// malloc(9223372036854775807 * 2) = trap
// RUN: not --crash %t.signed 9223372036854775807 0 0 2
// malloc(1 << 63) = malloc(1 << 63)
// RUN: %t.signed 1 0 0 0 63
// malloc(1 << 64) = trap
// RUN: not --crash %t.signed 1 0 0 0 64

// =============== Tests for SIZE_T=int64_t ===============

// malloc(0u + 1) = malloc(1)
// RUN: %t.unsigned 0 1
// malloc(0u + UINT_MAX) = malloc(UINT_MAX)
// RUN: %t.unsigned 0 -1
// malloc(1u + UINT_MAX) = trap
// RUN: not --crash %t.unsigned 1 -1
// malloc(UINT_MAX + 1u) = trap
// RUN: not --crash %t.unsigned -1 1
// malloc(9223372036854775807 + 9223372036854775807) = malloc(18446744073709551614)
// RUN: %t.unsigned 9223372036854775807 9223372036854775807
// malloc(0u - 1u) = trap
// RUN: not --crash %t.unsigned 0 0 -1
// malloc(1u - 1u) = malloc(0)
// RUN: %t.unsigned 1 0 1
// malloc(1u + 16u - 1u) = malloc(16)
// RUN: %t.unsigned 1 16 1

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  unsigned nextArg = 1;
  auto getNextArg = [&](const char *name, int64_t theDefault) {
    bool useDefault = nextArg >= argc;
    int64_t arg = useDefault ? theDefault : atoll(argv[nextArg++]);
    printf("%s\t= %" PRIi64 " (default = %" PRIi64 ")\n", name, arg,
           theDefault);
    return arg;
  };

#define GET_NEXT_ARG(_type, _name, _default) \
  _type _name = getNextArg(#_name, _default);

  // Get add/sub/mul/shl operands from the command line. Use default values if
  // an operand isn't specified.
  GET_NEXT_ARG(SIZE_T, initialSize, 0);     // Default is 0.
  GET_NEXT_ARG(int64_t, toAddSigned, 0);    // Default is 0.
  GET_NEXT_ARG(int64_t, toSubSigned, 0);    // Default is 0.
  GET_NEXT_ARG(int64_t, toMulSigned, 1);    // Default is 1.
  GET_NEXT_ARG(uint64_t, toLeftShift, 0);   // Default is 0.

  // Now, calculate the size based on the given operands. Do the adds first,
  // then the subs, then the muls, then the shl.
  SIZE_T size = initialSize;
  size += toAddSigned;
  size -= toSubSigned;
  size *= toMulSigned;
  size <<= toLeftShift;

  printf("Trying:\t malloc(%" PRIu64 ").\n", uint64_t(size));
  void *p = malloc(size);
  printf("Did not trap: got %p.\n", p);

  return 0;
}
