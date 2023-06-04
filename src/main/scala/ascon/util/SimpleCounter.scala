package ascon.util

import chisel3._

class SimpleCounter(n: Int) extends Module {
  val io = IO(new Bundle {
    val inc = Input(Bool())
    val extReset = Input(Bool())
    val max = Input(UInt((n + 1).W))
    val wrap = Output(Bool())
    val value = Output(UInt((n+1).W))
  })

  val value = RegInit(0.U(n.W))
  io.value := value
  when(io.inc) {
    value := value + 1.U
  }
  io.wrap := value >= (io.max)
  when(io.extReset || io.wrap) {
    value := 0.U
  }
}