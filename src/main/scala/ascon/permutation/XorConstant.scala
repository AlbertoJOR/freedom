package ascon.permutation

import chisel3._
class XorConstant extends Module{
  val io = IO(new Bundle() {
    val A = Input(Vec(5, UInt(64.W)))
    val round = Input(UInt(4.W))
    val S = Output(Vec(5, UInt(64.W)))
  })
  val constants = Module(new ConstantL)
  val constant = Wire(UInt(8.W))
  val A2 = Wire(UInt(64.W))
  val aux_zero = "h0000_0000_0000_0000_0000_0000_0000_0000".U
  val aux2 = Wire(UInt(64.W))
  // Signal connexion to the lookUpTable round constants
  constants.io.sel := io.round
  constant := constants.io.A
  aux2 := aux_zero ^ constant

  // Xor with the constant in the second row in the lsb
  A2 := io.A(2) ^ aux2
  // Output assignment
  io.S(0) := io.A(0)
  io.S(1) := io.A(1)
  io.S(2) := A2
  io.S(3) := io.A(3)
  io.S(4) := io.A(4)

}