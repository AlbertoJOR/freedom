package ascon.permutation.threshold
import ascon.permutation.ConstantL
import chisel3._
class constant_th_0 extends Module{
  val io = IO(new Bundle() {
    val A = Input(Vec(5, UInt(64.W)))
    val r0  = Input(UInt(64.W))
    val S = Output(Vec(5, UInt(64.W)))
  })

  // Xor with the constant in the second row in the lsb
 val   A2 = io.A(2) ^ io.r0
  // Output assignment
  io.S(0) := io.A(0)
  io.S(1) := io.A(1)
  io.S(2) := A2
  io.S(3) := io.A(3)
  io.S(4) := io.A(4)

}

class constant_th_1 extends Module{
  val io = IO(new Bundle() {
    val A = Input(Vec(5, UInt(64.W)))
    val r1  = Input(UInt(64.W))
    val S = Output(Vec(5, UInt(64.W)))
  })

  // Xor with the constant in the second row in the lsb
 val   A2 = io.A(2) ^ io.r1
  // Output assignment
  io.S(0) := io.A(0)
  io.S(1) := io.A(1)
  io.S(2) := A2
  io.S(3) := io.A(3)
  io.S(4) := io.A(4)

}

class constant_th_2 extends Module{
  val io = IO(new Bundle() {
    val A = Input(Vec(5, UInt(64.W)))
    val round = Input(UInt(4.W))
    val r0, r1 = Input(UInt(64.W))
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
  A2 := io.A(2) ^ aux2 ^io.r0 ^io.r1
  // Output assignment
  io.S(0) := io.A(0)
  io.S(1) := io.A(1)
  io.S(2) := A2
  io.S(3) := io.A(3)
  io.S(4) := io.A(4)

}