package hardfloat

import chisel3._
import chisel3.util._
import consts._


class SoftmaxWeightUpdater(val lanes: Int) extends Module {
    require(lanes > 0, "lanes must be positive")

    val io = IO(new Bundle {
        val update   = Input(Bool())
        val aIn      = Input(Vec(lanes, UInt(BF16Lite.width.W)))
        val gammaIn  = Input(UInt(BF16Lite.width.W))
        val aOut     = Output(Vec(lanes, UInt(BF16Lite.width.W)))
        val gammaOut = Output(UInt(BF16Lite.width.W))
    })

    private val bf16NegInf = "hFF80".U(BF16Lite.width.W)

    val aReg     = RegInit(VecInit(Seq.fill(lanes)(BF16Lite.posZero)))
    val gammaReg = RegInit(bf16NegInf)

    
    private val negGammaIn = Cat(
        ~io.gammaIn(BF16Lite.width - 1),
        io.gammaIn(BF16Lite.width - 2, 0)
    )
    private val diffAdd = Module(new AddBF16FTZ)
    diffAdd.io.a := gammaReg
    diffAdd.io.b := negGammaIn
    private val diffBf16 = diffAdd.io.out
    private val dgamma   = Cat(0.U(1.W), diffBf16(BF16Lite.width - 2, 0))

   
    private val aSign = gammaReg(BF16Lite.width - 1)
    private val bSign = io.gammaIn(BF16Lite.width - 1)
    private val aMag  = gammaReg(BF16Lite.width - 2, 0)
    private val bMag  = io.gammaIn(BF16Lite.width - 2, 0)
    private val bothZero        = (aMag === 0.U) && (bMag === 0.U)
    private val sameSign        = aSign === bSign
    // 同号: 正数按模式直接比大小, 负数反向。
    private val gammaGeSameSign = Mux(aSign.asBool, aMag <= bMag, aMag >= bMag)
    // 异号: a 正 b 负 ⇒ a ≥ b。
    private val gammaGeDiffSign = !aSign.asBool
    val gammaGe: Bool = bothZero || Mux(sameSign, gammaGeSameSign, gammaGeDiffSign)

    // ---- 3. 查表得到 g / w_small / w_large ----------------------------------------
    private val logAdd = Module(new LogAddBF16)
    logAdd.io.dgamma := dgamma
    private val g      = logAdd.io.g          // log₂(1 + 2^{−Δγ})
    private val wSmall = logAdd.io.w1         // 1/(1 + 2^{+Δγ})  ≤ 1/2
    private val wLarge = logAdd.io.w2         // 1/(1 + 2^{−Δγ})  ≥ 1/2

    private val wOld     = Mux(gammaGe, wLarge, wSmall)
    private val wNew     = Mux(gammaGe, wSmall, wLarge)
    private val gammaMax = Mux(gammaGe, gammaReg, io.gammaIn)

    // ---- 4. γ_new = γ_max + g -----------------------------------------------------
    private val gammaAdd = Module(new AddBF16FTZ)
    gammaAdd.io.a := gammaMax
    gammaAdd.io.b := g
    private val gammaNew = gammaAdd.io.out

    // ---- 5. 向量融合: a_new[i] = wOld · a_local[i] + wNew · a'[i] -----------------
    private val aNewVec = Wire(Vec(lanes, UInt(BF16Lite.width.W)))
    for (i <- 0 until lanes) {
        val prodOld = SoftmaxWeightUpdater.bf16Mul(wOld, aReg(i))
        val prodNew = SoftmaxWeightUpdater.bf16Mul(wNew, io.aIn(i))
        val sumAdd  = Module(new AddBF16FTZ)
        sumAdd.io.a := prodOld
        sumAdd.io.b := prodNew
        aNewVec(i)  := sumAdd.io.out
    }

    when(io.update) {
        gammaReg := gammaNew
        for (i <- 0 until lanes) {
            aReg(i) := aNewVec(i)
        }
    }

    io.aOut     := aReg
    io.gammaOut := gammaReg
}

object SoftmaxWeightUpdater {
    def bf16Mul(a: UInt, b: UInt): UInt = {
        val aS = sanitizeBF16(a, flushSubnormals = true)
        val bS = sanitizeBF16(b, flushSubnormals = true)

        val mul = Module(new MulRecFN(BF16.expWidth, BF16.sigWidth))
        mul.io.a := recFNFromFN(BF16.expWidth, BF16.sigWidth, aS)
        mul.io.b := recFNFromFN(BF16.expWidth, BF16.sigWidth, bS)
        mul.io.roundingMode   := round_near_even
        mul.io.detectTininess := tininess_afterRounding

        val fn = fNFromRecFN(BF16.expWidth, BF16.sigWidth, mul.io.out)
        sanitizeBF16(fn, flushSubnormals = true)
    }
}
