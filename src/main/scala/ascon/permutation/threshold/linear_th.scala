package ascon.permutation.threshold

import chisel3._
import chisel3.util._

class linear_th_0 extends Module {
  val io = IO(new Bundle() {
    val r0, r1, r2, r3, r4 = Input(UInt(64.W))
    val A = Input(Vec(5, UInt(64.W)))
    val S = Output(Vec(5, UInt(64.W)))
  })
  val r0 = Cat(io.r0(63, 45), io.r0(63, 55), io.r0(63, 28))
  val r1 = Cat(io.r0(63, 25), io.r0(63, 42), io.r0(63, 61))
  val r2 = Cat(io.r0(63), io.r0(63, 59), io.r0(63, 6))
  val r3 = Cat(io.r0(63, 54), io.r0(63, 57), io.r0(63, 17))
  val r4 = Cat(io.r0(63, 57), io.r0(63, 30), io.r0(63, 41))
  
  io.S(0) := io.A(0) ^ Cat(io.A(0)(18, 0), io.A(0)(63, 19)) ^ Cat(io.A(0)(27, 0), io.A(0)(63, 28)) ^ r0
  io.S(1) := io.A(1) ^ Cat(io.A(1)(60, 0), io.A(1)(63, 61)) ^ Cat(io.A(1)(38, 0), io.A(1)(63, 39)) ^ r1
  io.S(2) := io.A(2) ^ Cat(io.A(2)(0), io.A(2)(63, 1)) ^ Cat(io.A(2)(5, 0), io.A(2)(63, 6)) ^ r2
  io.S(3) := io.A(3) ^ Cat(io.A(3)(9, 0), io.A(3)(63, 10)) ^ Cat(io.A(3)(16, 0), io.A(3)(63, 17)) ^ r3
  io.S(4) := io.A(4) ^ Cat(io.A(4)(6, 0), io.A(4)(63, 7)) ^ Cat(io.A(4)(40, 0), io.A(4)(63, 41)) ^ r4
}

class linear_th_1 extends Module {
  val io = IO(new Bundle() {
    val r0, r1, r2, r3, r4 = Input(UInt(64.W))
    val A = Input(Vec(5, UInt(64.W)))
    val S = Output(Vec(5, UInt(64.W)))
  })
  val r0 = Cat(io.r0(27, 9), io.r0(44, 36), io.r0(54, 19))
  val r1 = Cat(io.r0(60, 22), io.r0(24, 3), io.r0(41, 39))
  val r2 = Cat(io.r0(5), io.r0(62, 58), io.r0(58, 1))
  val r3 = Cat(io.r0(16, 7), io.r0(53, 47), io.r0(56, 10))
  val r4 = Cat(io.r0(40, 34), io.r0(56, 23), io.r0(29, 7))

  io.S(0) := io.A(0) ^ Cat(io.A(0)(18, 0), io.A(0)(63, 19)) ^ Cat(io.A(0)(27, 0), io.A(0)(63, 28)) ^ r0
  io.S(1) := io.A(1) ^ Cat(io.A(1)(60, 0), io.A(1)(63, 61)) ^ Cat(io.A(1)(38, 0), io.A(1)(63, 39)) ^ r1
  io.S(2) := io.A(2) ^ Cat(io.A(2)(0), io.A(2)(63, 1)) ^ Cat(io.A(2)(5, 0), io.A(2)(63, 6)) ^ r2
  io.S(3) := io.A(3) ^ Cat(io.A(3)(9, 0), io.A(3)(63, 10)) ^ Cat(io.A(3)(16, 0), io.A(3)(63, 17)) ^ r3
  io.S(4) := io.A(4) ^ Cat(io.A(4)(6, 0), io.A(4)(63, 7)) ^ Cat(io.A(4)(40, 0), io.A(4)(63, 41)) ^ r4
}

class linear_th_2 extends Module {
  val io = IO(new Bundle() {
    val r0, r1, r2, r3, r4 = Input(UInt(64.W))
    val A = Input(Vec(5, UInt(64.W)))
    val S = Output(Vec(5, UInt(64.W)))
  })
  val r0 = Cat(io.r0(63, 45), io.r0(63, 55), io.r0(63, 28)) ^ Cat(io.r0(27, 9), io.r0(44, 36), io.r0(54, 19))
  val r1 = Cat(io.r0(63, 25), io.r0(63, 42), io.r0(63, 61)) ^ Cat(io.r0(60, 22), io.r0(24, 3), io.r0(41, 39))
  val r2 = Cat(io.r0(63), io.r0(63, 59), io.r0(63, 6)) ^ Cat(io.r0(5), io.r0(62, 58), io.r0(58, 1))
  val r3 = Cat(io.r0(63, 54), io.r0(63, 57), io.r0(63, 17)) ^ Cat(io.r0(16, 7), io.r0(53, 47), io.r0(56, 10))
  val r4 = Cat(io.r0(63, 57), io.r0(63, 30), io.r0(63, 41)) ^ Cat(io.r0(40, 34), io.r0(56, 23), io.r0(29, 7))

  io.S(0) := io.A(0) ^ Cat(io.A(0)(18, 0), io.A(0)(63, 19)) ^ Cat(io.A(0)(27, 0), io.A(0)(63, 28)) ^ r0
  io.S(1) := io.A(1) ^ Cat(io.A(1)(60, 0), io.A(1)(63, 61)) ^ Cat(io.A(1)(38, 0), io.A(1)(63, 39)) ^ r1
  io.S(2) := io.A(2) ^ Cat(io.A(2)(0), io.A(2)(63, 1)) ^ Cat(io.A(2)(5, 0), io.A(2)(63, 6)) ^ r2
  io.S(3) := io.A(3) ^ Cat(io.A(3)(9, 0), io.A(3)(63, 10)) ^ Cat(io.A(3)(16, 0), io.A(3)(63, 17)) ^ r3
  io.S(4) := io.A(4) ^ Cat(io.A(4)(6, 0), io.A(4)(63, 7)) ^ Cat(io.A(4)(40, 0), io.A(4)(63, 41)) ^ r4
}
