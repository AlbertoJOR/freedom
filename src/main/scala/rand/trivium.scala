package rand

import chisel3._
import chisel3.internal.firrtl.MemPortDirection.READ
import chisel3.util._

trait HasCommonInter extends Module{
 val io = IO(new Bundle() {
    val iv = Input(UInt(64.W))
    val K = Input(UInt(64.W))
    val reset = Input(Bool())
    val valid = Output(Bool())
    val Z = Output(UInt(64.W))
  })
}
class notTrivium extends Module with HasCommonInter{
  io.valid := true.B
  io.Z := "hff".U
}
class trivium extends Module with HasCommonInter {
   def toBools(A_UInt: UInt, width: Int): Vec[Bool] = {
    val A_Bools = VecInit(Seq.fill(width)(false.B))
    for (j <- 0 until width) {
      A_Bools(j) := A_UInt(j)
    }
    A_Bools
  }
  /*def toUInt(A_vecBools :Vec[Bool], width :Int): UInt ={
    val A_Uint = UInt(width.W)

    A_Uint
  }

*/
  val S1 = RegInit(VecInit(Seq.fill(93)(false.B)))
  val S2 = RegInit(VecInit(Seq.fill(84)(false.B)))
  val S3 = RegInit(VecInit(Seq.fill(108)(false.B) ++ Seq.fill(3)(true.B)))

  val S1_Uint = S1.asUInt()
  val S2_Uint = S2.asUInt()
  val S3_Uint = S3.asUInt()

  val counter = RegInit(0.U(11.W))
  val ready_reg = RegInit(false.B)
  val iR1, iR2, iR3, ZZ = RegInit(0.U(64.W))
  val t1, t2, t3, t1r, t2r, t3r = Wire(Vec(64, Bool()))
  for (i <- 0 until 64) {

    t1r(63 - i) := S1(65 - i) ^ S1(92 - i)
    t2r(63 - i) := S2(161 - 93 - i) ^ S2(176 - 93 - i)
    t3r(63 - i) := S3(242 - 177 - i) ^ S3(287 - 177 - i)
    t1(63 - i) := S1(65 - i) ^ S1(92 - i) ^ (S1(90 - i) & S1(91 - i)) ^ S2(170 - 93 - i)
    t2(63 - i) := S2(161 - 93 - i) ^ S2(176 - 93 - i) ^ (S2(174 - 93 - i) & S2(175 - 93 - i)) ^ S3(263 - 177 - i)
    t3(63 - i) := S3(242 - 177 - i) ^ S3(287 - 177 - i) ^ (S3(285 - 177 - i) & S3(286 - 177 - i)) ^ S1(68 - i)
  }

  ZZ := t1r.asUInt ^ t2r.asUInt ^ t3r.asUInt
  when(counter === 0.U) {
    iR3 := io.K
    iR2 := 0.U
    iR1 := io.iv
    S1 := toBools(Cat(S1_Uint(28,0), io.K), 29 + 64)
    S2 := toBools(Cat(S2_Uint(19, 0), io.iv), 20 + 64)
  }.elsewhen(counter === 1.U) {
    iR3 := io.K
    iR1 := io.iv
    S1 := toBools(Cat(S1_Uint(76, 0), io.K(15, 0)), 77 + 16)
    S2 := toBools(Cat(S2_Uint(67, 0), io.iv(15, 0)), 68 + 16)
  }.elsewhen(counter < 20.U) {
    iR1 := t1.asUInt
    iR2 := t2.asUInt
    iR3 := t3.asUInt
    S1 := toBools(Cat(S1_Uint(28, 0), t3.asUInt), 29 + 64)
    S2 := toBools(Cat(S2_Uint(19, 0), t1.asUInt), 20 + 64)
    S3 := toBools(Cat(S3_Uint(46, 0), t2.asUInt), 47 + 64)
  }.otherwise {
    ready_reg := 1.U
    iR1 := t1.asUInt
    iR2 := t2.asUInt
    iR3 := t3.asUInt
    S1 := toBools(Cat(S1_Uint(28, 0), t3.asUInt), 29 + 64)
    S2 := toBools(Cat(S2_Uint(19, 0), t1.asUInt), 20 + 64)
    S3 := toBools(Cat(S3_Uint(46, 0), t2.asUInt), 47 + 64)
  }
  io.Z := ZZ
  when(!ready_reg) {
    counter := counter + 1.U
  }
  io.valid := ready_reg
}
