package ascon.permutation.threshold

import chisel3._
import chisel3.util._

class permutationP1_th extends Module {
  val io = IO(new Bundle() {
    val round = Input(UInt(4.W))
    val A = Input(Vec(5, UInt(64.W)))
    val S = Output(Vec(5, UInt(64.W)))
    val r0, r1, r2, r3, r4, r5, r6 = Input(UInt(64.W))
  })
  val Const0 = Module(new constant_th_0)
  val Const1 = Module(new constant_th_1)
  val Const2 = Module(new constant_th_2)

  val Sust0 = Module(new SBox_th_0)
  val Sust1 = Module(new SBox_th_1)
  val Sust2 = Module(new SBox_th_2)

  val Lin0 = Module(new linear_th_0)
  val Lin1 = Module(new linear_th_1)
  val Lin2 = Module(new linear_th_2)
  // Round constant addition
  Const0.io.A := io.A
  Const0.io.r0 := io.r5
  Const1.io.A := io.A
  Const1.io.r1 := io.r6
  Const2.io.A := io.A
  Const2.io.r0 := io.r5
  Const2.io.r1 := io.r6
  Const2.io.round := io.round
  //println(io.round.litValue.toString(16))


  val x0_0 = Const0.io.A(0)
  val x1_0 = Const0.io.A(1)
  val x2_0 = Const0.io.A(2)
  val x3_0 = Const0.io.A(3)
  val x4_0 = Const0.io.A(4)

  val x0_1 = Const1.io.A(0)
  val x1_1 = Const1.io.A(1)
  val x2_1 = Const1.io.A(2)
  val x3_1 = Const1.io.A(3)
  val x4_1 = Const1.io.A(4)

  val x0_2 = Const2.io.A(0)
  val x1_2 = Const2.io.A(1)
  val x2_2 = Const2.io.A(2)
  val x3_2 = Const2.io.A(3)
  val x4_2 = Const2.io.A(4)

  // Substitution layer with 5 bit sbox
  Sust0.io.x0_0 := x0_0
  Sust0.io.x1_0 := x1_0
  Sust0.io.x2_0 := x2_0
  Sust0.io.x3_0 := x3_0
  Sust0.io.x4_0 := x4_0
  Sust0.io.x0_1 := x0_1
  Sust0.io.x1_1 := x1_1
  Sust0.io.x2_1 := x2_1
  Sust0.io.x3_1 := x3_1
  Sust0.io.x4_1 := x4_1
  Sust0.io.x0_2 := x0_2
  Sust0.io.x1_2 := x1_2
  Sust0.io.x2_2 := x2_2
  Sust0.io.x3_2 := x3_2
  Sust0.io.x4_2 := x4_2

  Sust1.io.x0_0 := x0_0
  Sust1.io.x1_0 := x1_0
  Sust1.io.x2_0 := x2_0
  Sust1.io.x3_0 := x3_0
  Sust1.io.x4_0 := x4_0
  Sust1.io.x0_1 := x0_1
  Sust1.io.x1_1 := x1_1
  Sust1.io.x2_1 := x2_1
  Sust1.io.x3_1 := x3_1
  Sust1.io.x4_1 := x4_1
  Sust1.io.x0_2 := x0_2
  Sust1.io.x1_2 := x1_2
  Sust1.io.x2_2 := x2_2
  Sust1.io.x3_2 := x3_2
  Sust1.io.x4_2 := x4_2

  Sust2.io.x0_0 := x0_0
  Sust2.io.x1_0 := x1_0
  Sust2.io.x2_0 := x2_0
  Sust2.io.x3_0 := x3_0
  Sust2.io.x4_0 := x4_0
  Sust2.io.x0_1 := x0_1
  Sust2.io.x1_1 := x1_1
  Sust2.io.x2_1 := x2_1
  Sust2.io.x3_1 := x3_1
  Sust2.io.x4_1 := x4_1
  Sust2.io.x0_2 := x0_2
  Sust2.io.x1_2 := x1_2
  Sust2.io.x2_2 := x2_2
  Sust2.io.x3_2 := x3_2
  Sust2.io.x4_2 := x4_2

  // Linear layer 64 bit diffusion function
  Lin0.io.A(0) := Sust0.io.y0_0
  Lin0.io.A(1) := Sust0.io.y1_0
  Lin0.io.A(2) := Sust0.io.y2_0
  Lin0.io.A(3) := Sust0.io.y3_0
  Lin0.io.A(4) := Sust0.io.y4_0
  Lin0.io.r0 := io.r0
  Lin0.io.r1 := io.r1
  Lin0.io.r2 := io.r2
  Lin0.io.r3 := io.r3
  Lin0.io.r4 := io.r4

  Lin1.io.A(0) := Sust1.io.y0_1
  Lin1.io.A(1) := Sust1.io.y1_1
  Lin1.io.A(2) := Sust1.io.y2_1
  Lin1.io.A(3) := Sust1.io.y3_1
  Lin1.io.A(4) := Sust1.io.y4_1
  Lin1.io.r0 := io.r0
  Lin1.io.r1 := io.r1
  Lin1.io.r2 := io.r2
  Lin1.io.r3 := io.r3
  Lin1.io.r4 := io.r4

  Lin2.io.A(0) := Sust2.io.y0_2
  Lin2.io.A(1) := Sust2.io.y1_2
  Lin2.io.A(2) := Sust2.io.y2_2
  Lin2.io.A(3) := Sust2.io.y3_2
  Lin2.io.A(4) := Sust2.io.y4_2
  Lin2.io.r0 := io.r0
  Lin2.io.r1 := io.r1
  Lin2.io.r2 := io.r2
  Lin2.io.r3 := io.r3
  Lin2.io.r4 := io.r4

  val s0 = Lin0.io.S(0) ^ Lin1.io.S(0) ^ Lin2.io.S(0)
  val s1 = Lin0.io.S(1) ^ Lin1.io.S(1) ^ Lin2.io.S(1)
  val s2 = Lin0.io.S(2) ^ Lin1.io.S(2) ^ Lin2.io.S(2)
  val s3 = Lin0.io.S(3) ^ Lin1.io.S(3) ^ Lin2.io.S(3)
  val s4 = Lin0.io.S(4) ^ Lin1.io.S(4) ^ Lin2.io.S(4)
  io.S(0) := s0
  io.S(1) := s1
  io.S(2) := s2
  io.S(3) := s3
  io.S(4) := s4

}