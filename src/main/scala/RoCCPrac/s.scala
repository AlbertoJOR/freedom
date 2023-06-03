package RoCCPrac

import chisel3._

class s extends Module{
  val io = IO(new Bundle{
    val in_A = Input(UInt(32.W))
    val in_B = Input(UInt(32.W))
    val out_C = Output(UInt(32.W))
  })
  io.out_C := io.in_B ^ io.in_A

}
