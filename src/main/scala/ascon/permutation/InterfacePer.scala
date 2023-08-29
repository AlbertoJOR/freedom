package ascon.permutation

import chisel3._

trait CtrlPermutationInter extends Module{
  val io = IO(new Bundle() {
    val start = Input(Bool())
    val typePer = Input(UInt(2.W))
    val ready = Output(Bool())
    val valid = Output(Bool())
    val selIn = Output(Bool())
    val busy = Output(Bool())
    val round = Output(UInt(4.W))
    val rst_per = Input(Bool())

  })
}
trait PermutationInter extends Module{
  val io = IO(new Bundle() {
    val A = Input(Vec(5, UInt(64.W)))
    val S = Output(Vec(5, UInt(64.W)))
    val typePer = Input(UInt(2.W))
    val start = Input(Bool())
    val ready = Output(Bool())
    val busy = Output(Bool())
    val valid = Output(Bool())
    val rst_per = Input(Bool())
  })
}