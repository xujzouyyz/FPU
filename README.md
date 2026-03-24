Berkeley Hardware Floating-Point Units
======================================

This repository contains hardware floating-point units written in Chisel.
This library contains parameterized floating-point units for fused multiply-add
operations, conversions between integer and floating-point numbers, and
conversions between floating-point conversions with different precision.

**WARNING**:
These units are works in progress.  They may not be yet completely free of
bugs, nor are they fully optimized.


Recoded Format
--------------

The floating-point units in this repository work on an internal recoded format
(exponent has an additional bit) to handle subnormal numbers more efficiently
in a microprocessor.  A more detailed explanation will come soon, but in the
mean time here are some example mappings for single-precision numbers.

    IEEE format                           Recoded format
    ----------------------------------    -----------------------------------
    s 00000000 00000000000000000000000    s 000------ 00000000000000000000000
    s 00000000 00000000000000000000001    s 001101011 00000000000000000000000
    s 00000000 0000000000000000000001f    s 001101100 f0000000000000000000000
    s 00000000 000000000000000000001ff    s 001101101 ff000000000000000000000
        ...              ...                   ...              ... 
    s 00000000 001ffffffffffffffffffff    s 001111111 ffffffffffffffffffff000
    s 00000000 01fffffffffffffffffffff    s 010000000 fffffffffffffffffffff00
    s 00000000 1ffffffffffffffffffffff    s 010000001 ffffffffffffffffffffff0
    s 00000001 fffffffffffffffffffffff    s 010000010 fffffffffffffffffffffff
    s 00000010 fffffffffffffffffffffff    s 010000011 fffffffffffffffffffffff
        ...              ...                   ...              ... 
    s 11111101 fffffffffffffffffffffff    s 101111110 fffffffffffffffffffffff
    s 11111110 fffffffffffffffffffffff    s 101111111 fffffffffffffffffffffff
    s 11111111 00000000000000000000000    s 110------ -----------------------
    s 11111111 fffffffffffffffffffffff    s 111------ fffffffffffffffffffffff


Unit-Testing
------------

To unit-test these floating-point units, you need the berkeley-testfloat-3
package.

To test floating-point units with the C simulator:

    $ make

BF16 NIC-optimized adder
------------------------

For in-network-computing style BF16 accumulation, this repository now provides
specialized adder modules:

- `AddRecBF16_NIC`: single-lane BF16 adder specialization.
- `AddRecBF16_NIC_Array(lanes)`: multi-lane parallel array for throughput
  scaling.
- `AddBF16FTZ`: standalone BF16 adder specialization without recoded format.
- `AddBF16FTZ_Array(lanes)`: multi-lane array of `AddBF16FTZ`.

Design choices for throughput-oriented specialization:

- Fixed op type: add only (no subtract path).
- Fixed rounding mode: round-to-nearest-even.
- Fixed tininess detection: after rounding.
- Optional pruning (`flushSubnormals = true` by default): subnormal inputs are
  flushed to signed zero to simplify datapath behavior in many NIC workloads.
- `AddBF16FTZ` hard-prunes for NIC usage:
  - IEEE BF16 input/output directly (no recoded datapath).
  - Input FTZ (subnormals flushed to zero), output also avoids subnormal
    emission by flushing underflowed results to zero.
  - Fixed add-only datapath and fixed round-to-nearest-even (RNE).
  - Minimal interface (`a`, `b`, `out`) without exception/tininess signals.

Example (Chisel):

    val bf16Adder = Module(new AddRecBF16_NIC(flushSubnormals = true))
    bf16Adder.io.a := inA
    bf16Adder.io.b := inB
    val out = bf16Adder.io.out

    val bf16Add4 = Module(new AddRecBF16_NIC_Array(lanes = 4))
    bf16Add4.io.a := inVecA
    bf16Add4.io.b := inVecB

