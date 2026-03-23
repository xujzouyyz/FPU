package hardfloat

import chisel3._
import consts._

object BF16 {
    val expWidth = 8
    val sigWidth = 8
    val width = expWidth + sigWidth
    val recWidth = width + 1
}

object sanitizeBF16 {
    def apply(in: UInt, flushSubnormals: Boolean): UInt = {
        if (flushSubnormals) {
            val exp = in(14, 7)
            val fract = in(6, 0)
            val isSubnormal = (exp === 0.U) && fract.orR
            Mux(isSubnormal, Cat(in(15), 0.U(15.W)), in)
        } else {
            in
        }
    }
}



class AddRecBF16_NIC(flushSubnormals: Boolean = true) extends RawModule
{
    val io = IO(new Bundle {
        val a = Input(UInt(BF16.width.W))
        val b = Input(UInt(BF16.width.W))
        val out = Output(UInt(BF16.width.W))
        val recOut = Output(UInt(BF16.recWidth.W))
        val exceptionFlags = Output(UInt(5.W))
    })

    val aSanitized = sanitizeBF16(io.a, flushSubnormals)
    val bSanitized = sanitizeBF16(io.b, flushSubnormals)

    val addRecFN = Module(new AddRecFN(BF16.expWidth, BF16.sigWidth))
    addRecFN.io.subOp := false.B
    addRecFN.io.a := recFNFromFN(BF16.expWidth, BF16.sigWidth, aSanitized)
    addRecFN.io.b := recFNFromFN(BF16.expWidth, BF16.sigWidth, bSanitized)
    addRecFN.io.roundingMode := round_near_even
    addRecFN.io.detectTininess := tininess_afterRounding

    io.recOut := addRecFN.io.out
    io.out := fNFromRecFN(BF16.expWidth, BF16.sigWidth, addRecFN.io.out)
    io.exceptionFlags := addRecFN.io.exceptionFlags
}

/** Multi-lane BF16 NIC add array for throughput scaling.
  *
  * Per lane semantics are identical to [[AddRecBF16_NIC]].
  */
class AddRecBF16_NIC_Array(lanes: Int, flushSubnormals: Boolean = true)
    extends RawModule
{
    require(lanes > 0, "lanes must be greater than 0")

    val io = IO(new Bundle {
        val a = Input(Vec(lanes, UInt(BF16.width.W)))
        val b = Input(Vec(lanes, UInt(BF16.width.W)))
        val out = Output(Vec(lanes, UInt(BF16.width.W)))
        val recOut = Output(Vec(lanes, UInt(BF16.recWidth.W)))
        val exceptionFlags = Output(Vec(lanes, UInt(5.W)))
    })

    for (lane <- 0 until lanes) {
        val add = Module(new AddRecBF16_NIC(flushSubnormals))
        add.io.a := io.a(lane)
        add.io.b := io.b(lane)
        io.out(lane) := add.io.out
        io.recOut(lane) := add.io.recOut
        io.exceptionFlags(lane) := add.io.exceptionFlags
    }
}
