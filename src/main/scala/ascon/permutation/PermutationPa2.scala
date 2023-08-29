package ascon.permutation

import chisel3._

class PermutationPa2 extends Module with PermutationInter {

  // This implementation of the permutation is able to perform
  // 2 rounds per cycle
  val per1 = Module(new permutationP1)
  val per2 = Module(new permutationP1)
  val ctrlPer = Module(new CtrlPermutation2)
  val round = Wire(UInt(4.W))
  val inputReg = RegInit(VecInit(Seq.fill(5)(0.U(64.W))))
  val stateWire = Wire(Vec(5, UInt(64.W)))
  //val statePrev = RegInit(VecInit(Seq.fill(5)(0.U(64.W))))
  inputReg := io.A


  ctrlPer.io.start := io.start
  ctrlPer.io.typePer := io.typePer
  ctrlPer.io.rst_per := io.rst_per
  io.ready := ctrlPer.io.ready
  round := ctrlPer.io.round
  io.busy := ctrlPer.io.busy
  io.valid := ctrlPer.io.valid


  val stateReg = RegInit(VecInit(Seq.fill(5)(0.U(64.W))))

  val dataInPer = Mux(ctrlPer.io.selIn, io.A, stateReg)

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