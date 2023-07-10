package trivium

import chisel3._
import chisel3.util._

class trivium extends Module {

  val io = IO(new Bundle() {
    val iv = Input(UInt(64.W))
    val K = Input(UInt(64.W))
    val valid = Output(Bool())
    val Z = Output(UInt(64.W))
    val S1 = Output(UInt(93.W))
    val S2 = Output(UInt(84.W))
    val S3 = Output(UInt(111.W))
  })
  val S1 = RegInit(VecInit(Seq.fill(93)(false.B)))
  val S2 = RegInit(VecInit(Seq.fill(84)(false.B)))
  val S3 = RegInit(VecInit(Seq.fill(108)(false.B) ++ Seq.fill(3)(true.B)))
  /// NO existe el asBITs
  S3.asUInt()
  val counter = RegInit(0.U(11.W))
  val exam = 3.U(4.W)
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
    S1 := Cat(S1.asUInt()(28, 0), io.K)
    S2 := Cat(S2.asUInt()(19, 0), io.iv)
  }.elsewhen(counter === 1.U) {
    iR3 := io.K
    iR1 := io.iv
    S1 := Cat(S1.asUInt()(76, 0), io.K(15, 0))
    S2 := Cat(S2.asUInt()(67, 0), io.iv(15, 0))
  }.elsewhen(counter < 20.U) {
    iR1 := t1.asUInt
    iR2 := t2.asUInt
    iR3 := t3.asUInt
    S1 := Cat(S1.asUInt()(28, 0), t3.asUInt)
    S2 := Cat(S2.asUInt()(19, 0), t1.asUInt)
    S3 := Cat(S3.asUInt()(46, 0), t2.asUInt)
  }.otherwise{
   ready_reg := 1.U
    iR1 := t1.asUInt
			iR2 := t2.asUInt
			iR3 := t3.asUInt
			S1 :=Cat(  S1.asUInt()(28 , 0) , t3.asUInt)
			S2 := Cat(  S2.asUInt()(19 , 0) , t1.asUInt)
			S3 := Cat(  S3.asUInt()(46 , 0) , t2.asUInt)
  }
  io.Z := ZZ
  when(!ready_reg){
    counter := counter +1.U
  }
  io.valid := ready_reg
  io.S1 := S1.asUInt
  io.S2 := S2.asUInt
  io.S3 :=S3.asUInt




  //for (i <- 0 until 64) {
  //regs(i) := regs(i+1)
  //}
}
