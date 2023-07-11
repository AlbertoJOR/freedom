package ascon

import chisel3._
import chisel3.util._
class RandFSM extends Module {
  val io = IO(new Bundle() {
    val rdy = Output(Bool())
    val valid = Output(Bool())
    val seed = Input(Bool())
    val seed_mux = Output(Bool())
    val rand = Input(Bool())
    val rand_mux = Output(Bool())
    val init_per = Output(Bool())
    val busy_per = Input(Bool())
  })
  val s_rst :: s_idle :: s_absorb :: s_squeeze :: s_absorb_ack :: s_squeeze_ack :: Nil = Enum(6)
  val stateReg = RegInit(s_rst)
  val valid_reg = RegInit(false.B)
  io.rdy := stateReg === s_idle
  io.valid := valid_reg
  io.rand_mux := stateReg === s_squeeze || stateReg === s_squeeze_ack
  io.seed_mux := stateReg === s_absorb || stateReg === s_absorb_ack
  io.init_per := stateReg === s_absorb || stateReg === s_squeeze

  switch(stateReg) {
    is(s_idle) {
      when(io.seed) {
        stateReg := s_absorb
        valid_reg := false.B
      //}.elsewhen(io.key) {
        //stateReg := s_absorb
        //valid_reg := false.B
      }.elsewhen(io.rand) {
        stateReg := s_squeeze
        valid_reg := false.B
      }
    }
    is(s_absorb) {
      stateReg := s_absorb_ack
    }
    is(s_squeeze) {
      stateReg := s_squeeze_ack
    }
    is(s_absorb_ack) {
      when(!io.busy_per) {
        stateReg := s_idle
        valid_reg := true.B
      }

    }
    is(s_squeeze_ack) {
      when(!io.busy_per) {
        stateReg := s_idle
        valid_reg := true.B

      }
    }
  }
}
