; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=x86_64-apple-darwin -O0 | FileCheck %s

; Make sure we only use the less significant bit of the value that feeds the
; select. Otherwise, we may account for a non-zero value whereas the
; lsb is zero.
; <rdar://problem/15651765>

define i32 @fastisel_select(i1 %exchSub2211_, i1 %trunc_8766) {
; CHECK-LABEL: fastisel_select:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    movb %sil, %al
; CHECK-NEXT:    movb %dil, %cl
; CHECK-NEXT:    xorl %esi, %esi
; CHECK-NEXT:    subb %al, %cl
; CHECK-NEXT:    testb $1, %cl
; CHECK-NEXT:    movl $1204476887, %edi ## imm = 0x47CADBD7
; CHECK-NEXT:    cmovnel %edi, %esi
; CHECK-NEXT:    movl %esi, %eax
; CHECK-NEXT:    retq
  %shuffleInternal15257_8932 = sub i1 %exchSub2211_, %trunc_8766
  %counter_diff1345 = select i1 %shuffleInternal15257_8932, i32 1204476887, i32 0
  ret i32 %counter_diff1345
}

