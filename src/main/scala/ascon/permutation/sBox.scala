package ascon.permutation

import chisel3._
class sBox( ) extends  Module{
  val io = IO(new Bundle{
    val A = Input(Vec(5, UInt(64.W)))
    val S = Output(Vec(5, UInt(64.W)))
  })

  val x0, x1, x2, x3, x4 = Wire(UInt(64.W))
  val t0, t1, t2, t3, t4 = Wire(UInt(64.W))
  val x00, x22, x44 = Wire(UInt(64.W))
  // 1
  x0 := io.A(0) ^ io.A(4);      x4 := io.A(4) ^ io.A(3);      x2 := io.A(2) ^ io.A(1)
  // 2
  t0 := ~x4 & x0
  // 3
  t1 := ~io.A(1) & x2;      x00 := x0 ^ t1
  // 4
  t2 := ~io.A(3) & x4;      x22 := x2 ^ t2
  // 5
  t3 := ~x00 & io.A(1);     x44 := x4 ^ t3
  // 6
  t4 := ~x22 & io.A(3);     x1 := io.A(1) ^ t4
  // 7
  x3 := io.A(3) ^ t0
  // 8
  io.S(1) := x1 ^ x00;      io.S(3) := x3 ^ x22;      io.S(0) := x00 ^ x44;     io.S(2) := ~x22
  //
  io.S(4) := x44

  /**
   * página 33 figura 6 bitsliced 5-bit S-box S(x)
   * x0 += x4;     x4 +=  x3;     x2 += x1;
   * t0 = (~x4) & x0;
   * t1 = (~x1) & x2;     x0 += t1;
   * t1 = (~x3) & x4;     x2 += t1;
   * t1 = (~x0) & x1;     x4 += t1;
   * t1 = (~x2) & x3;     x1 += t1;
   * x3 += t0;
   * x1 += x0;     x3 +=  x2;     x0 += x4;     x2 += ~x2;
   */
}