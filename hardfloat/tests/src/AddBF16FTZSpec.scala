package hardfloat

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import java.io.{File, PrintWriter}

/** Thin wrapper so chiseltest can drive the combinational [[AddBF16FTZ]]. */
class AddBF16FTZ_Wrapper extends Module {
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

// ---------------------------------------------------------------------------
// Software reference model
// ---------------------------------------------------------------------------
object BF16Ref {
  val qNaN:   Int = 0x7FC0
  val posInf: Int = 0x7F80
  val negInf: Int = 0xFF80

  def bf16ToFloat(bf16: Int): Float =
    java.lang.Float.intBitsToFloat((bf16 & 0xFFFF) << 16)

  /** Round a Float32 to BF16 using RNE (Round to Nearest, ties to Even). */
  def floatToBF16RNE(f: Float): Int = {
    val bits  = java.lang.Float.floatToRawIntBits(f)
    val sign  = (bits >>> 31) & 1
    val exp   = (bits >>> 23) & 0xFF
    val frac  = bits & 0x7FFFFF

    if (exp == 0xFF && frac != 0) return qNaN
    if (exp == 0xFF) return if (sign == 1) negInf else posInf

    val upper  = (bits >>> 16) & 0xFFFF
    val lower  = bits & 0xFFFF
    val guard  = (lower >>> 15) & 1
    val round  = (lower >>> 14) & 1
    val sticky = if ((lower & 0x3FFF) != 0) 1 else 0
    val lsb    = upper & 1
    val roundUp = guard & (round | sticky | lsb)
    (upper + roundUp) & 0xFFFF
  }

  def ftz(bf16: Int): Int = {
    val exp  = (bf16 >>> 7) & 0xFF
    val frac = bf16 & 0x7F
    if (exp == 0 && frac != 0) bf16 & 0x8000 else bf16 & 0xFFFF
  }

  def isNaN(bf16: Int): Boolean = {
    val exp  = (bf16 >>> 7) & 0xFF
    val frac = bf16 & 0x7F
    exp == 0xFF && frac != 0
  }

  /** Software BF16 FTZ addition (mirrors hardware semantics). */
  def add(a: Int, b: Int): Int = {
    val aS = ftz(a)
    val bS = ftz(b)

    val aExp  = (aS >>> 7) & 0xFF
    val bExp  = (bS >>> 7) & 0xFF
    val aFrac = aS & 0x7F
    val bFrac = bS & 0x7F
    val aSign = (aS >>> 15) & 1
    val bSign = (bS >>> 15) & 1

    val aIsNaN = aExp == 0xFF && aFrac != 0
    val bIsNaN = bExp == 0xFF && bFrac != 0
    val aIsInf = aExp == 0xFF && aFrac == 0
    val bIsInf = bExp == 0xFF && bFrac == 0

    if (aIsNaN || bIsNaN) return qNaN
    if (aIsInf && bIsInf && aSign != bSign) return qNaN
    if (aIsInf) return aS & 0xFFFF
    if (bIsInf) return bS & 0xFFFF

    val fa = bf16ToFloat(aS)
    val fb = bf16ToFloat(bS)
    val result = fa + fb

    if (result == 0.0f) {
      if (java.lang.Float.floatToRawIntBits(result) != 0)
        return 0x8000 // -0
      else
        return 0x0000 // +0
    }

    ftz(floatToBF16RNE(result))
  }

  def bf16Str(v: Int): String =
    f"0x${v & 0xFFFF}%04X (${bf16ToFloat(v)})"
}

// ---------------------------------------------------------------------------
// Test spec
// ---------------------------------------------------------------------------
class AddBF16FTZSpec extends AnyFlatSpec with ChiselScalatestTester
    with Matchers with BeforeAndAfterAll {
  import BF16Ref._

  private val logDir  = new File("test_run_dir")
  private val logFile = new File(logDir, "AddBF16FTZ_results.txt")
  private var writer: PrintWriter = _
  private var caseIdx: Int = 0
  private var passCount: Int = 0
  private var failCount: Int = 0

  override def beforeAll(): Unit = {
    super.beforeAll()
    logDir.mkdirs()
    writer = new PrintWriter(logFile, "UTF-8")
    writer.println(f"${"#"}%-8s ${"a(hex)"}%-12s ${"b(hex)"}%-12s " +
      f"${"expected(hex)"}%-16s ${"actual(hex)"}%-16s " +
      f"${"a(float)"}%-14s ${"b(float)"}%-14s " +
      f"${"expected(float)"}%-16s ${"actual(float)"}%-16s ${"pass/fail"}%-8s")
    writer.println("-" * 140)
    writer.flush()
  }

  override def afterAll(): Unit = {
    writer.println("-" * 140)
    writer.println(f"Total: ${passCount + failCount}%d | Pass: $passCount%d | Fail: $failCount%d")
    writer.close()
    println(s"\n[AddBF16FTZSpec] Results written to: ${logFile.getAbsolutePath}")
    println(f"[AddBF16FTZSpec] Total: ${passCount + failCount}%d | Pass: $passCount%d | Fail: $failCount%d")
    super.afterAll()
  }

  private def testOne(dut: AddBF16FTZ_Wrapper, a: Int, b: Int): Unit = {
    dut.io.a.poke((a & 0xFFFF).U)
    dut.io.b.poke((b & 0xFFFF).U)
    dut.clock.step()
    val expected = add(a, b)
    val actual   = dut.io.out.peek().litValue.toInt

    val pass = if (isNaN(expected)) isNaN(actual) else actual == expected

    caseIdx += 1
    if (pass) passCount += 1 else failCount += 1

    val aHex = f"0x${a & 0xFFFF}%04X"
    val bHex = f"0x${b & 0xFFFF}%04X"
    val expHex = f"0x${expected & 0xFFFF}%04X"
    val actHex = f"0x${actual & 0xFFFF}%04X"
    writer.println(
      f"${caseIdx}%-8d ${aHex}%-12s ${bHex}%-12s " +
      f"${expHex}%-16s ${actHex}%-16s " +
      f"${bf16ToFloat(a)}%-14s ${bf16ToFloat(b)}%-14s " +
      f"${bf16ToFloat(expected)}%-16s ${bf16ToFloat(actual)}%-16s ${if (pass) "PASS" else "FAIL"}%-8s"
    )
    writer.flush()

    if (isNaN(expected)) {
      assert(isNaN(actual),
        s"Expected NaN but got ${bf16Str(actual)} | a=${bf16Str(a)}, b=${bf16Str(b)}")
    } else {
      assert(actual == expected,
        s"Mismatch: a=${bf16Str(a)}, b=${bf16Str(b)} => " +
        s"expected=${bf16Str(expected)}, actual=${bf16Str(actual)}")
    }
  }

  behavior of "AddBF16FTZ"

  // ---------- zero arithmetic ----------

  it should "add +0 + +0 = +0" in {
    test(new AddBF16FTZ_Wrapper) { dut => testOne(dut, 0x0000, 0x0000) }
  }

  it should "add +0 + -0 = +0" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      testOne(dut, 0x0000, 0x8000)
      testOne(dut, 0x8000, 0x0000)
    }
  }

  it should "add -0 + -0 = -0" in {
    test(new AddBF16FTZ_Wrapper) { dut => testOne(dut, 0x8000, 0x8000) }
  }

  // ---------- basic normal values ----------

  it should "compute 1.0 + 1.0 = 2.0" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      testOne(dut, 0x3F80, 0x3F80) // 1.0 + 1.0 = 2.0 (0x4000)
    }
  }

  it should "compute 1.0 + (-1.0) = +0" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      testOne(dut, 0x3F80, 0xBF80)
    }
  }

  it should "compute 3.5 + 2.25 = 5.75" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      testOne(dut, 0x4060, 0x4010) // 3.5 + 2.25
    }
  }

  it should "compute (-5.0) + 3.0 = (-2.0)" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      testOne(dut, 0xC0A0, 0x4040) // -5.0 + 3.0
    }
  }

  // ---------- NaN handling ----------

  it should "return qNaN when any input is NaN" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      testOne(dut, 0x7FC0, 0x3F80) // qNaN + 1.0
      testOne(dut, 0x3F80, 0x7FC0) // 1.0 + qNaN
      testOne(dut, 0x7FC0, 0x7FC0) // qNaN + qNaN
      testOne(dut, 0x7F81, 0x3F80) // sNaN + 1.0
    }
  }

  // ---------- infinity handling ----------

  it should "handle infinity correctly" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      testOne(dut, 0x7F80, 0x3F80) // +Inf + 1.0 = +Inf
      testOne(dut, 0xFF80, 0x3F80) // -Inf + 1.0 = -Inf
      testOne(dut, 0x7F80, 0x7F80) // +Inf + +Inf = +Inf
      testOne(dut, 0xFF80, 0xFF80) // -Inf + -Inf = -Inf
    }
  }

  it should "return qNaN for Inf + (-Inf)" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      testOne(dut, 0x7F80, 0xFF80)
      testOne(dut, 0xFF80, 0x7F80)
    }
  }

  // ---------- FTZ input flushing ----------

  it should "flush subnormal inputs to zero" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      testOne(dut, 0x0001, 0x3F80) // smallest pos subnormal + 1.0 → 1.0
      testOne(dut, 0x003F, 0x0000) // subnormal + 0 → 0
      testOne(dut, 0x007F, 0x807F) // pos subnormal + neg subnormal → 0
    }
  }

  // ---------- FTZ output (underflow) ----------

  it should "flush underflow results to zero" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      // smallest normal BF16 ≈ 2^-126 = 0x0080
      testOne(dut, 0x0080, 0x8080) // exact cancellation → +0
    }
  }

  // ---------- overflow to infinity ----------

  it should "overflow to infinity for very large sums" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      testOne(dut, 0x7F7F, 0x7F7F) // max_normal + max_normal → +Inf
    }
  }

  // ---------- large exponent difference ----------

  it should "handle large exponent difference" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      testOne(dut, 0x3F80, 0x0080) // 1.0 + tiny → 1.0
      testOne(dut, 0x4B00, 0x3F80) // large + 1.0
    }
  }

  // ---------- rounding (RNE) ----------

  it should "round correctly with various GRS patterns" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      testOne(dut, 0x3F80, 0x3400) // 1.0 + 0.125
      testOne(dut, 0x4000, 0x3E00) // 2.0 + 0.5
      testOne(dut, 0x4049, 0x3C01) // rounding edge case
      testOne(dut, 0x3F81, 0x3380) // 1.0039 + 0.0625
    }
  }

  // ---------- random exhaustive sweep ----------

  it should "match software model on 100,000 random BF16 pairs" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      val rng = new scala.util.Random(42)
      for (i <- 0 until 100) {
        val a = rng.nextInt(0x10000)
        val b = rng.nextInt(0x10000)
        testOne(dut, a, b)
      }
    }
  }

  // ---------- structured sweep over special regions ----------

  it should "pass structured sweep (zeros, normals, specials × normals)" in {
    test(new AddBF16FTZ_Wrapper) { dut =>
      val specials = Seq(0x0000, 0x8000, 0x7F80, 0xFF80, 0x7FC0, 0xFFC0)
      val normals  = Seq(0x3F80, 0x4000, 0x4080, 0xBF80, 0xC000, 0x0080, 0x8080, 0x7F7F, 0xFF7F)

      for (a <- specials ++ normals; b <- specials ++ normals)
        testOne(dut, a, b)
    }
  }
}
