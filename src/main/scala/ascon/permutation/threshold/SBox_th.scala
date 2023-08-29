package ascon.permutation.threshold

import chisel3._
import chisel3.util._

class SBox_th_0 extends Module {
  val io = IO(new Bundle {
    val x0_0, x1_0, x2_0, x3_0, x4_0 = Input(UInt(64.W))
    val x0_1, x1_1, x2_1, x3_1, x4_1 = Input(UInt(64.W))
    val x0_2, x1_2, x2_2, x3_2, x4_2 = Input(UInt(64.W))
    val y0_0, y1_0, y2_0, y3_0, y4_0 = Output(UInt(64.W))
  })

  io.y0_0 := io.x0_0 ^ io.x0_1 & io.x1_1 ^ io.x0_1 & io.x1_2 ^ io.x0_1 ^ io.x1_1 & io.x2_1 ^ io.x1_1 & io.x4_1 ^ io.x1_1 & io.x0_2 ^ io.x1_1 & io.x2_2 ^ io.x1_1 & io.x4_2 ^ io.x1_1 ^ io.x2_1 & io.x1_2 ^ io.x2_1 ^ io.x3_1 ^ io.x4_1 & io.x1_2 ^ io.x0_2 & io.x1_2 ^ io.x1_2 & io.x2_2 ^ io.x1_2 & io.x4_2 ^ io.x1_2 ^ io.x2_2 ^ io.x3_2
  io.y1_0 := io.x1_0 ^ io.x0_1 ^ io.x1_1 & io.x2_1 ^ io.x1_1 & io.x3_1 ^ io.x1_1 & io.x2_2 ^ io.x1_1 & io.x3_2 ^ io.x1_1 ^ io.x2_1 & io.x3_1 ^ io.x2_1 & io.x1_2 ^ io.x2_1 & io.x3_2 ^ io.x2_1 ^ io.x3_1 & io.x1_2 ^ io.x3_1 & io.x2_2 ^ io.x3_1 ^ io.x4_1 ^ io.x0_2 ^ io.x1_2 & io.x2_2 ^ io.x1_2 & io.x3_2 ^ io.x2_2 & io.x3_2 ^ io.x2_2 ^ io.x3_2 ^ io.x4_2
  io.y2_0 := io.x2_0 ^ io.x1_1 ^ io.x2_1 ^ io.x3_1 & io.x4_1 ^ io.x3_1 & io.x4_2 ^ io.x4_1 & io.x3_2 ^ io.x4_1 ^ io.x1_2 ^ io.x3_2 & io.x4_2 ^ io.x4_2 ^ "hffffffffffffffff".U
  io.y3_0 := io.x3_0 ^ io.x0_1 & io.x3_1 ^ io.x0_1 & io.x4_1 ^ io.x0_1 & io.x3_2 ^ io.x0_1 & io.x4_2 ^ io.x0_1 ^ io.x1_1 ^ io.x2_1 ^ io.x3_1 & io.x0_2 ^ io.x3_1 ^ io.x4_1 & io.x0_2 ^ io.x4_1 ^ io.x0_2 & io.x3_2 ^ io.x0_2 & io.x4_2 ^ io.x0_2 ^ io.x1_2 ^ io.x2_2 ^ io.x4_2
  io.y4_0 := io.x4_0 ^ io.x0_1 & io.x1_1 ^ io.x0_1 & io.x1_2 ^ io.x1_1 & io.x4_1 ^ io.x1_1 & io.x0_2 ^ io.x1_1 & io.x4_2 ^ io.x1_1 ^ io.x3_1 ^ io.x4_1 & io.x1_2 ^ io.x4_1 ^ io.x0_2 & io.x1_2 ^ io.x1_2 & io.x4_2 ^ io.x1_2 ^ io.x3_2


}

class SBox_th_1 extends Module {
  val io = IO(new Bundle {
    val x0_0, x1_0, x2_0, x3_0, x4_0 = Input(UInt(64.W))
    val x0_1, x1_1, x2_1, x3_1, x4_1 = Input(UInt(64.W))
    val x0_2, x1_2, x2_2, x3_2, x4_2 = Input(UInt(64.W))
    val y0_1, y1_1, y2_1, y3_1, y4_1 = Output(UInt(64.W))
  })
  io.y0_1 := io.x0_0 & io.x1_0 ^ io.x0_0 & io.x1_1 ^ io.x0_0 & io.x1_2 ^ io.x1_0 & io.x2_0 ^ io.x1_0 & io.x4_0 ^ io.x1_0 & io.x0_1 ^ io.x1_0 & io.x2_1 ^ io.x1_0 & io.x4_1 ^ io.x1_0 & io.x0_2 ^ io.x1_0 & io.x2_2 ^ io.x1_0 & io.x4_2 ^ io.x1_0 ^ io.x2_0 & io.x1_1 ^ io.x2_0 & io.x1_2 ^ io.x2_0 ^ io.x3_0 ^ io.x4_0 & io.x1_1 ^ io.x4_0 & io.x1_2
  io.y1_1 := io.x0_0 ^ io.x1_0 & io.x2_0 ^ io.x1_0 & io.x3_0 ^ io.x1_0 & io.x2_1 ^ io.x1_0 & io.x3_1 ^ io.x1_0 & io.x2_2 ^ io.x1_0 & io.x3_2 ^ io.x2_0 & io.x3_0 ^ io.x2_0 & io.x1_1 ^ io.x2_0 & io.x3_1 ^ io.x2_0 & io.x1_2 ^ io.x2_0 & io.x3_2 ^ io.x2_0 ^ io.x3_0 & io.x1_1 ^ io.x3_0 & io.x2_1 ^ io.x3_0 & io.x1_2 ^ io.x3_0 & io.x2_2 ^ io.x3_0 ^ io.x4_0
  io.y2_1 := io.x1_0 ^ io.x3_0 & io.x4_0 ^ io.x3_0 & io.x4_1 ^ io.x3_0 & io.x4_2 ^ io.x4_0 & io.x3_1 ^ io.x4_0 & io.x3_2 ^ io.x4_0
  io.y3_1 := io.x0_0 & io.x3_0 ^ io.x0_0 & io.x4_0 ^ io.x0_0 & io.x3_1 ^ io.x0_0 & io.x4_1 ^ io.x0_0 & io.x3_2 ^ io.x0_0 & io.x4_2 ^ io.x0_0 ^ io.x1_0 ^ io.x2_0 ^ io.x3_0 & io.x0_1 ^ io.x3_0 & io.x0_2 ^ io.x4_0 & io.x0_1 ^ io.x4_0 & io.x0_2 ^ io.x4_0
  io.y4_1 := io.x0_0 & io.x1_0 ^ io.x0_0 & io.x1_1 ^ io.x0_0 & io.x1_2 ^ io.x1_0 & io.x4_0 ^ io.x1_0 & io.x0_1 ^ io.x1_0 & io.x4_1 ^ io.x1_0 & io.x0_2 ^ io.x1_0 & io.x4_2 ^ io.x1_0 ^ io.x3_0 ^ io.x4_0 & io.x1_1 ^ io.x4_0 & io.x1_2

}

class SBox_th_2 extends Module {
  val io = IO(new Bundle {
    val x0_0, x1_0, x2_0, x3_0, x4_0 = Input(UInt(64.W))
    val x0_1, x1_1, x2_1, x3_1, x4_1 = Input(UInt(64.W))
    val x0_2, x1_2, x2_2, x3_2, x4_2 = Input(UInt(64.W))
    val y0_2, y1_2, y2_2, y3_2, y4_2 = Output(UInt(64.W))
  })
  io.y0_2 := io.x0_2;
  io.y1_2 := io.x1_2;
  io.y2_2 := io.x2_2;
  io.y3_2 := io.x3_2;
  io.y4_2 := io.x4_2;
}
