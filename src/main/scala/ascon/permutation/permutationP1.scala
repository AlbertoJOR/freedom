package ascon.permutation

import chisel3._
import chisel3.util._
class permutationP1 extends Module {
  val io = IO(new Bundle() {
    val round = Input(UInt(4.W))
    val A = Input(Vec(5, UInt(64.W)))
    val S = Output(Vec(5, UInt(64.W)))
  })
  val constLayer = Module(new XorConstant)
  val substLayer = Module(new sBox)
  val diffLayer = Module(new DiffusionL)
  // Round constant addition
  constLayer.io.A := io.A
  constLayer.io.round := io.round
  //println(io.round.litValue.toString(16))

  // Substitution layer with 5 bit sbox
  substLayer.io.A := constLayer.io.S

  // Linear layer 64 bit diffusion function

  diffLayer.io.A := substLayer.io.S

  io.S := diffLayer.io.S

}