package RoCCPrac

import chisel3._

class SimpleOperation2 extends Module{
  val io = IO(new Bundle{
    val in_A = Input(UInt(64.W))
    val out_C = Output(UInt(64.W))
  })
  io.out_C := io.in_A ^( "hf0f0_f0f0_f0f0_f0f0".U)

}
