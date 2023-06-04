package ascon.permutation

import chisel3._
import chisel3.util._

class DiffusionL() extends Module {
  val io = IO(new Bundle{

    val A = Input(Vec(5, UInt(64.W)))
    val S = Output(Vec(5, UInt(64.W)))
  })
    io.S(0) := io.A(0) ^ Cat(io.A(0)(18,0) , io.A(0)(63, 19)) ^Cat(io.A(0)(27, 0), io.A(0)(63,28))
    io.S(1) := io.A(1) ^ Cat(io.A(1)(60,0) , io.A(1)(63, 61)) ^Cat(io.A(1)(38, 0) , io.A(1)(63,39))
    io.S(2) := io.A(2) ^ Cat(io.A(2)(0)    , io.A(2)(63, 1 )) ^Cat(io.A(2)( 5, 0) , io.A(2)(63, 6))
    io.S(3) := io.A(3) ^ Cat(io.A(3)( 9,0) , io.A(3)(63, 10)) ^Cat(io.A(3)(16, 0) , io.A(3)(63,17))
    io.S(4) := io.A(4) ^ Cat(io.A(4)( 6,0) , io.A(4)(63, 7)) ^Cat(io.A(4)(40, 0) , io.A(4)(63,41))
  }