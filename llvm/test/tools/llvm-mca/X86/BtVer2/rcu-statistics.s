# RUN: llvm-mca -mtriple=x86_64-unknown-unknown -mcpu=btver2 -resource-pressure=false -retire-stats -iterations=1 < %s | FileCheck %s

  vsqrtps %xmm0, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2
  vaddps  %xmm0, %xmm1, %xmm2

# CHECK:      Iterations:     1
# CHECK-NEXT: Instructions:   16
# CHECK-NEXT: Total Cycles:   31
# CHECK-NEXT: Dispatch Width: 2
# CHECK-NEXT: IPC:            0.52

# CHECK:      Instruction Info:
# CHECK-NEXT: [1]: #uOps
# CHECK-NEXT: [2]: Latency
# CHECK-NEXT: [3]: RThroughput
# CHECK-NEXT: [4]: MayLoad
# CHECK-NEXT: [5]: MayStore
# CHECK-NEXT: [6]: HasSideEffects

# CHECK:      [1]    [2]    [3]    [4]    [5]    [6]	Instructions:
# CHECK-NEXT:  1      21    21.00                   	vsqrtps	%xmm0, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2
# CHECK-NEXT:  1      3     1.00                    	vaddps	%xmm0, %xmm1, %xmm2

# CHECK:      Retire Control Unit - number of cycles where we saw N instructions retired:
# CHECK-NEXT: [# retired], [# cycles]
# CHECK-NEXT:  0,           23  (74.2%)
# CHECK-NEXT:  2,           8  (25.8%)

