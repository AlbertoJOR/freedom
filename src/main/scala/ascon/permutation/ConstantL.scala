package ascon.permutation

import chisel3._
import chisel3.util._

class ConstantL() extends Module {
  val io = IO(new Bundle {
    val sel = Input(UInt(4.W))
    val A = Output(UInt(8.W))
  })

  io.A := MuxLookup(io.sel, "h00".U,
    Array(0.U -> "hf0".U,
          1.U -> "he1".U,
          2.U -> "hd2".U,
          3.U -> "hc3".U,
          4.U -> "hb4".U,
          5.U -> "ha5".U,
          6.U -> "h96".U,
          7.U -> "h87".U,
          8.U -> "h78".U,
          9.U -> "h69".U,
          10.U -> "h5a".U,
          11.U -> "h4b".U
  ))}