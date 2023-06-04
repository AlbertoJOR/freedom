package ascon.permutation

import chisel3._
import chisel3.util._

import javax.print.attribute.standard.JobStateReason

class PermutationPa extends Module {
  val io = IO(new Bundle() {
    val A = Input(Vec(5, UInt(64.W)))
    val S = Output(Vec(5, UInt(64.W)))
    val typePer = Input(UInt(2.W))
    val start = Input(Bool())
    val ready = Output(Bool())
    val busy = Output(Bool())
    val valid = Output(Bool())
  })
  // This implementation of the permutation is able to perform
  // 2 rounds per cycle
  val per1 = Module(new permutationP1)
  val per2 = Module(new permutationP1)
  val ctrlPer = Module(new CtrlPermutation)
  val round = Wire(UInt(4.W))
  val inputReg = RegInit(VecInit(Seq.fill(5)(0.U(64.W))))
  val stateWire = Wire(Vec(5, UInt(64.W)))
  //val statePrev = RegInit(VecInit(Seq.fill(5)(0.U(64.W))))
  inputReg := io.A


  ctrlPer.io.start := io.start
  ctrlPer.io.typePer := io.typePer
  io.ready := ctrlPer.io.ready
  round := ctrlPer.io.round
  io.busy := ctrlPer.io.busy
  io.valid := ctrlPer.io.valid


  val stateReg = RegInit(VecInit(Seq.fill(5)(0.U(64.W))))

  val dataInPer = Mux(ctrlPer.io.selIn, inputReg, stateReg)

  per1.io.A := dataInPer
  per1.io.round := round
  per2.io.A := per1.io.S
  per2.io.round := round + 1.U
  stateWire := per2.io.S
  when(ctrlPer.io.busy) {

    stateReg := stateWire
  }
  io.S := stateReg
}