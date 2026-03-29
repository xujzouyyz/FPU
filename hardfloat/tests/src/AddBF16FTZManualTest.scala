package hardfloat

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AddBF16FTZ_TestWrapper extends Module {
  val io = IO(new Bundle {
    val a   = Input(UInt(BF16Lite.width.W))
    val b   = Input(UInt(BF16Lite.width.W))
    val out = Output(UInt(BF16Lite.width.W))
  })
  val adder = Module(new AddBF16FTZ)
  adder.io.a := io.a
  adder.io.b := io.b
  io.out := adder.io.out
}

object BF16Helper {
  def bf16ToFloat(bf16: Int): Float =
    java.lang.Float.intBitsToFloat((bf16 & 0xFFFF) << 16)

  def floatToBF16(f: Float): Int = {
    val bits = java.lang.Float.floatToRawIntBits(f)
    (bits >>> 16) & 0xFFFF
  }

  def printBF16(label: String, v: Int): Unit = {
    val f = bf16ToFloat(v)
    val sign = (v >> 15) & 1
    val exp  = (v >> 7) & 0xFF
    val frac = v & 0x7F
    println(f"  $label%-10s: 0x${v}%04X  sign=$sign  exp=$exp%3d  frac=0x${frac}%02X  float=$f")
  }
}

class AddBF16FTZManualTest extends AnyFlatSpec with ChiselScalatestTester {
  import BF16Helper._

  private def runAdd(dut: AddBF16FTZ_TestWrapper, aFloat: Float, bFloat: Float): Unit = {
    val aBF16 = floatToBF16(aFloat)
    val bBF16 = floatToBF16(bFloat)

    dut.io.a.poke((aBF16 & 0xFFFF).U)
    dut.io.b.poke((bBF16 & 0xFFFF).U)
    dut.clock.step()

    val outBF16 = dut.io.out.peek().litValue.toInt
    val outFloat = bf16ToFloat(outBF16)

    println("=" * 60)
    println(f"  AddBF16FTZ:  $aFloat  +  $bFloat")
    println("-" * 60)
    printBF16("a", aBF16)
    printBF16("b", bBF16)
    printBF16("out", outBF16)
    println("-" * 60)
    println(f"  结果: $aFloat + $bFloat = $outFloat")
    println(f"  (精确值: ${aFloat + bFloat})")
    println("=" * 60)
  }

  behavior of "AddBF16FTZ Manual Test"

  it should "compute 1.5 + 0.25" in {
    test(new AddBF16FTZ_TestWrapper) { dut =>
      runAdd(dut, -5.0f, 0.25f)
    }
  }
}
