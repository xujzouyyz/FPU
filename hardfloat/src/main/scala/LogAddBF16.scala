package hardfloat

import chisel3._
import chisel3.util._

object LogAddConfig {
   
    val useLog2    = true

    val eMin       = -8
    val eMax       =  3

    val numExps    = eMax - eMin + 1                     // 12
    val segBits    = 5                                   // 每个指数 32 段
    val interpBits = BF16Lite.fracBits - segBits         // 2 位段内插值

    val lutEntries = numExps * (1 << segBits)            // 12 * 32 = 384
    val addrBits   = log2Ceil(lutEntries)                // 9
    val eiBits     = log2Ceil(numExps)                   // 4
}

object LogAddTableGen {
    import LogAddConfig._

   
    def floatToBF16(v: Float): Int = {
        if (v.isNaN) return 0x7FC0
        val bits = java.lang.Float.floatToRawIntBits(v)
        val hi   = (bits >>> 16) & 0xFFFF
        val lo   = bits & 0xFFFF
        val up   = (lo > 0x8000) || (lo == 0x8000 && (hi & 1) != 0)
        val out  = if (up) hi + 1 else hi
    
        if ((out & 0x7F80) == 0x7F80 && (out & 0x007F) != 0) {
            (hi & 0x8000) | 0x7F80
        } else out & 0xFFFF
    }

    def dgammaAt(e: Int, seg: Int, interp: Int): Double = {
        val m = ((seg << interpBits) | interp).toDouble
        (1.0 + m / (1 << BF16Lite.fracBits)) * math.pow(2.0, e.toDouble)
    }

    def gOf(dg: Double): Double = {
        val v = math.log(1.0 + math.pow(2.0, -dg))
        if (useLog2) v / math.log(2.0) else v
    }

    def w1Of(dg: Double): Double = 1.0 / (1.0 + math.pow(2.0, dg))

    def buildAll: (Seq[Int], Seq[Int], Seq[Int], Seq[Int]) = {
        val gBase  = Array.fill(lutEntries)(0)
        val gStep  = Array.fill(lutEntries)(0)
        val w1Base = Array.fill(lutEntries)(0)
        val w1Step = Array.fill(lutEntries)(0)

        for (ei <- 0 until numExps; seg <- 0 until (1 << segBits)) {
            val e    = ei + eMin
            val dg0  = dgammaAt(e, seg, 0)
            val dgN  = dgammaAt(e, seg + 1, 0) 
            val gS   = gOf(dg0).toFloat
            val gE   = gOf(dgN).toFloat
            val wS   = w1Of(dg0).toFloat
            val wE   = w1Of(dgN).toFloat

            val idx  = ei * (1 << segBits) + seg
            gBase(idx)  = floatToBF16(gS)
            gStep(idx)  = floatToBF16((gE - gS) / 4.0f)
            w1Base(idx) = floatToBF16(wS)
            w1Step(idx) = floatToBF16((wE - wS) / 4.0f)
        }
        (gBase.toSeq, gStep.toSeq, w1Base.toSeq, w1Step.toSeq)
    }
}


class LogAddBF16 extends Module {
    import LogAddConfig._

    val io = IO(new Bundle {
        val dgamma = Input(UInt(BF16Lite.width.W))
        val g  = Output(UInt(BF16Lite.width.W))
        val w1 = Output(UInt(BF16Lite.width.W))
        val w2 = Output(UInt(BF16Lite.width.W))
    })

    
    val (gBaseTbl, gStepTbl, w1BaseTbl, w1StepTbl) = LogAddTableGen.buildAll
    val gBaseROM  = VecInit(gBaseTbl .map(_.U(BF16Lite.width.W)))
    val gStepROM  = VecInit(gStepTbl .map(_.U(BF16Lite.width.W)))
    val w1BaseROM = VecInit(w1BaseTbl.map(_.U(BF16Lite.width.W)))
    val w1StepROM = VecInit(w1StepTbl.map(_.U(BF16Lite.width.W)))

    val sign = io.dgamma(BF16Lite.width - 1)
    val exp  = io.dgamma(BF16Lite.width - 2, BF16Lite.fracBits)
    val frac = io.dgamma(BF16Lite.fracBits - 1, 0)

    val expS = exp.zext - 127.S                         // 无偏指数 SInt
    val isZeroOrSub = exp === 0.U
    val isInfOrNaN  = exp === "hFF".U
    val isNeg       = sign.asBool

  
    val tooSmall = isNeg || isZeroOrSub || (expS < eMin.S)
    val tooLarge = (!isNeg && (expS > eMax.S)) || (!isNeg && isInfOrNaN)

   
    val eiRaw = (expS - eMin.S)
    val ei    = Mux(tooSmall, 0.U(eiBits.W),
                 Mux(tooLarge, (numExps - 1).U(eiBits.W),
                     eiRaw.asUInt(eiBits - 1, 0)))
    val seg    = frac(BF16Lite.fracBits - 1, interpBits)
    val interp = frac(interpBits - 1, 0)
    val addr   = Cat(ei, seg)

    val gBase  = gBaseROM (addr)
    val gStep  = gStepROM (addr)
    val w1Base = w1BaseROM(addr)
    val w1Step = w1StepROM(addr)

    def bf16Times2(x: UInt): UInt = {
        val s  = x(BF16Lite.width - 1)
        val e  = x(BF16Lite.width - 2, BF16Lite.fracBits)
        val f  = x(BF16Lite.fracBits - 1, 0)
        val isZ  = e === 0.U
        val isMx = e === "hFE".U
        val eN   = Mux(isMx, "hFF".U(BF16Lite.expBits.W), e + 1.U)
        val fN   = Mux(isMx, 0.U(BF16Lite.fracBits.W), f)
        Mux(isZ, 0.U(BF16Lite.width.W), Cat(s, eN, fN))
    }

    val gStep2  = bf16Times2(gStep)
    val w1Step2 = bf16Times2(w1Step)

   
    val gStep3Add = Module(new AddBF16FTZ)
    gStep3Add.io.a := gStep
    gStep3Add.io.b := gStep2
    val gStep3 = gStep3Add.io.out

    val w1Step3Add = Module(new AddBF16FTZ)
    w1Step3Add.io.a := w1Step
    w1Step3Add.io.b := w1Step2
    val w1Step3 = w1Step3Add.io.out

    // ---- k*step mux ----
    val z = BF16Lite.posZero
    val gDelta = Mux(interp === 0.U, z,
                  Mux(interp === 1.U, gStep,
                   Mux(interp === 2.U, gStep2, gStep3)))
    val w1Delta = Mux(interp === 0.U, z,
                   Mux(interp === 1.U, w1Step,
                    Mux(interp === 2.U, w1Step2, w1Step3)))

    // ---- base + delta ----
    val gSumAdd = Module(new AddBF16FTZ)
    gSumAdd.io.a := gBase
    gSumAdd.io.b := gDelta
    val gRaw = gSumAdd.io.out

    val w1SumAdd = Module(new AddBF16FTZ)
    w1SumAdd.io.a := w1Base
    w1SumAdd.io.b := w1Delta
    val w1Raw = w1SumAdd.io.out

    // ---- 饱和 ----
    val bf16_one  = "h3F80".U(BF16Lite.width.W)   // 1.0
    val bf16_half = "h3F00".U(BF16Lite.width.W)   // 0.5
    val bf16_ln2  = "h3F31".U(BF16Lite.width.W)   // ln(2) ≈ 0.6931 (RNE 到 bf16)

    val gSmall  = if (useLog2) bf16_one else bf16_ln2  // g(Δγ→0+)
    val w1Small = bf16_half                            // w1(Δγ→0+)
    val zero    = BF16Lite.posZero                     // g(Δγ→∞) , w1(Δγ→∞)

    val gOut  = Mux(tooSmall, gSmall,  Mux(tooLarge, zero, gRaw))
    val w1Out = Mux(tooSmall, w1Small, Mux(tooLarge, zero, w1Raw))

    // ---- w2 = 1 - w1 : 翻 w1 的符号位再加 1.0 ----
    val negW1 = Cat(~w1Out(BF16Lite.width - 1), w1Out(BF16Lite.width - 2, 0))
    val w2Add = Module(new AddBF16FTZ)
    w2Add.io.a := bf16_one
    w2Add.io.b := negW1
    val w2Out = w2Add.io.out

    io.g  := gOut
    io.w1 := w1Out
    io.w2 := w2Out
}
