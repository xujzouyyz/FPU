package hardfloat

import chisel3._
import chisel3.util._

object BF16Lite {
    val expBits = 8
    val fracBits = 7
    val width = 1 + expBits + fracBits

    val qNaN: UInt = "h7FC0".U(width.W)
    val posZero: UInt = 0.U(width.W)
}

/** BF16 adder specialization for NIC-like workloads.
  *
  * Design goals:
  *   - IEEE BF16 input/output format directly (no recoded format).
  *   - FTZ policy: all subnormal inputs are treated as signed zero.
  *   - Fixed rounding: round-to-nearest-even.
  *   - Minimal interface: add-only and output-only datapath.
  */
class AddBF16FTZ extends RawModule {
    val io = IO(new Bundle {
        val a = Input(UInt(BF16Lite.width.W))
        val b = Input(UInt(BF16Lite.width.W))
        val out = Output(UInt(BF16Lite.width.W))
    })

    private def shiftRightJam12(in: UInt, sh: UInt): UInt = {
        val out = Wire(UInt(12.W))
        out := Cat(0.U(11.W), in.orR)

        switch(sh) {
            is(0.U) {
                out := in
            }
            is(1.U) {
                out := Cat(0.U(1.W), in(11, 2), in(1, 0).orR)
            }
            is(2.U) {
                out := Cat(0.U(2.W), in(11, 3), in(2, 0).orR)
            }
            is(3.U) {
                out := Cat(0.U(3.W), in(11, 4), in(3, 0).orR)
            }
            is(4.U) {
                out := Cat(0.U(4.W), in(11, 5), in(4, 0).orR)
            }
            is(5.U) {
                out := Cat(0.U(5.W), in(11, 6), in(5, 0).orR)
            }
            is(6.U) {
                out := Cat(0.U(6.W), in(11, 7), in(6, 0).orR)
            }
            is(7.U) {
                out := Cat(0.U(7.W), in(11, 8), in(7, 0).orR)
            }
            is(8.U) {
                out := Cat(0.U(8.W), in(11, 9), in(8, 0).orR)
            }
            is(9.U) {
                out := Cat(0.U(9.W), in(11, 10), in(9, 0).orR)
            }
            is(10.U) {
                out := Cat(0.U(10.W), in(11), in(10, 0).orR)
            }
            is(11.U) {
                out := Cat(0.U(11.W), in.orR)
            }
        }

        out
    }

    def sanitizeFTZ(in: UInt): UInt = {
        val sign = in(15)
        val exp = in(14, 7)
        val frac = in(6, 0)
        val isSubnormal = (exp === 0.U) && frac.orR
        Mux(isSubnormal, Cat(sign, 0.U(15.W)), in)
    }

    val aIn = sanitizeFTZ(io.a)
    val bIn = sanitizeFTZ(io.b)

    val aSign = aIn(15)
    val bSign = bIn(15)
    val aExp = aIn(14, 7)
    val bExp = bIn(14, 7)
    val aFrac = aIn(6, 0)
    val bFrac = bIn(6, 0)

    val aIsZero = aExp === 0.U
    val bIsZero = bExp === 0.U
    val aIsInf = (aExp === "hFF".U) && (aFrac === 0.U)
    val bIsInf = (bExp === "hFF".U) && (bFrac === 0.U)
    val aIsNaN = (aExp === "hFF".U) && (aFrac =/= 0.U)
    val bIsNaN = (bExp === "hFF".U) && (bFrac =/= 0.U)

    val invalidInfSum = aIsInf && bIsInf && (aSign =/= bSign)

    val outReg = Wire(UInt(BF16Lite.width.W))
    outReg := BF16Lite.posZero

    when(aIsNaN || bIsNaN || invalidInfSum) {
        outReg := BF16Lite.qNaN
    } .elsewhen(aIsInf || bIsInf) {
        val infSign = Mux(aIsInf, aSign, bSign)
        outReg := Cat(infSign, "hFF".U(8.W), 0.U(7.W))
    } .otherwise {
        val aMag = Cat(aExp, aFrac)
        val bMag = Cat(bExp, bFrac)
        val aGeB = aMag >= bMag

        val largeSign = Mux(aGeB, aSign, bSign)
        val largeExp = Mux(aGeB, aExp, bExp)
        val smallExp = Mux(aGeB, bExp, aExp)
        val largeFrac = Mux(aGeB, aFrac, bFrac)
        val smallFrac = Mux(aGeB, bFrac, aFrac)

        val largeIsZero = Mux(aGeB, aIsZero, bIsZero)
        val smallIsZero = Mux(aGeB, bIsZero, aIsZero)

        val sameSign = largeSign === smallSign

        val largeMant = Mux(largeIsZero, 0.U(8.W), Cat(1.U(1.W), largeFrac))
        val smallMant = Mux(smallIsZero, 0.U(8.W), Cat(1.U(1.W), smallFrac))

        // Keep 3 extra bits (GRS) for RNE rounding.
        val largeExt = Cat(0.U(1.W), largeMant, 0.U(3.W)) // 12 bits
        val smallExt = Cat(0.U(1.W), smallMant, 0.U(3.W)) // 12 bits

        val expDiff = largeExp - smallExp
        val shiftAmt = Mux(expDiff > 11.U, 11.U, expDiff(3, 0))
        val alignedSmall = shiftRightJam12(smallExt, shiftAmt)

        val normExt = Wire(UInt(12.W))
        normExt := 0.U
        val expNormS = Wire(SInt(10.W))
        expNormS := 0.S
        val signNorm = Wire(Bool())
        signNorm := largeSign
        val exactZero = Wire(Bool())
        exactZero := false.B

        when(sameSign) {
            val sumExt = largeExt +& alignedSmall // 13 bits
            val carryIntoMsb = sumExt(11)

            val shiftedForCarry =
                Cat(0.U(1.W), sumExt(11, 2), sumExt(1) | sumExt(0))
            normExt := Mux(carryIntoMsb, shiftedForCarry, sumExt(11, 0))
            expNormS := largeExp.zext + carryIntoMsb.asUInt.zext
            signNorm := largeSign
        } .otherwise {
            val diffExt = largeExt - alignedSmall // non-negative by construction
            when(diffExt === 0.U) {
                exactZero := true.B
                normExt := 0.U
                expNormS := 0.S
                signNorm := false.B
            } .otherwise {
                val diffNoTop = diffExt(10, 0)
                val lz = PriorityEncoder(diffNoTop.asBools.reverse)
                val normalized = (diffNoTop << lz)(10, 0)
                normExt := Cat(0.U(1.W), normalized)
                expNormS := largeExp.zext - lz.zext
                signNorm := largeSign
            }
        }

        when(exactZero || (expNormS <= 0.S)) {
            // FTZ output policy: do not emit subnormals.
            // Exact cancellation uses +0, while underflow keeps sign.
            outReg := Mux(exactZero, BF16Lite.posZero, Cat(signNorm, 0.U(15.W)))
        } .otherwise {
            val mantPre = normExt(10, 3) // hidden + 7 fraction bits
            val guard = normExt(2)
            val round = normExt(1)
            val sticky = normExt(0)
            val roundUp = guard && (round || sticky || mantPre(0))

            val mantRounded = mantPre + roundUp // 9 bits
            val roundCarry = mantRounded(8)
            val mantFinal = Mux(roundCarry, mantRounded(8, 1), mantRounded(7, 0))
            val expAfterRound = expNormS + roundCarry.asUInt.zext

            when(expAfterRound >= 255.S) {
                outReg := Cat(signNorm, "hFF".U(8.W), 0.U(7.W))
            } .otherwise {
                outReg := Cat(signNorm, expAfterRound.asUInt(7, 0), mantFinal(6, 0))
            }
        }
    }

    io.out := outReg
}

class AddBF16FTZ_Array(lanes: Int) extends RawModule {
    require(lanes > 0, "lanes must be greater than 0")

    val io = IO(new Bundle {
        val a = Input(Vec(lanes, UInt(BF16Lite.width.W)))
        val b = Input(Vec(lanes, UInt(BF16Lite.width.W)))
        val out = Output(Vec(lanes, UInt(BF16Lite.width.W)))
    })

    for (lane <- 0 until lanes) {
        val add = Module(new AddBF16FTZ)
        add.io.a := io.a(lane)
        add.io.b := io.b(lane)
        io.out(lane) := add.io.out
    }
}
