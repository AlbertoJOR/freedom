package ascon.permutation

import chisel3.util.{is, switch, _}
import chisel3.{when, _}


class CtrlPermutation2 extends Module  with CtrlPermutationInter {


  val sRst :: sWait :: sEnc :: Nil = Enum(3)


  // Initial State when reset+
  val stateReg = RegInit(sRst)
  val roundReg = RegInit(0.U(4.W))
  val readyReg = RegInit(false.B)
  val selInReg = RegInit(false.B)
  val encWire = Wire(UInt(1.W))
  val validReg = RegInit(false.B)
  val startReg = RegInit(false.B)

  when(io.start) {
    startReg := true.B
  }
  when(io.rst_per){
    validReg := false.B
  }

  io.valid := validReg
  encWire := false.B
  io.round := roundReg
  io.selIn := selInReg
  io.ready := readyReg
  io.busy := encWire
  switch(stateReg) {
    is(sRst) {
      stateReg := sWait
      encWire := false.B
    }
    is(sWait) {
      readyReg := true.B
      encWire := false.B
      selInReg := true.B
      when(io.typePer === "b00".U) {
        // When the a = 12 rounds
        roundReg := "b0000".U
      }.elsewhen(io.typePer === "b01".U) {
        // When the a = 8 rounds
        roundReg := "b0100".U
      }.otherwise {
        // when the a = 6 rounds
        roundReg := "b0110".U
      }

      when(io.start) {
        stateReg := sEnc
        readyReg := false.B
        validReg := false.B
      }

    }
    is(sEnc) {
      startReg := false.B
      encWire := true.B
      selInReg := false.B
      roundReg := roundReg + 2.U // 2 rolled permutation
      when(roundReg >= 10.U) {
        stateReg := sWait
        validReg := true.B

        when(io.typePer === "b00".U) {
          // When the a = 12 rounds
          roundReg := "b0000".U
        }.elsewhen(io.typePer === "b01".U) {
          // When the a = 8 rounds
          roundReg := "b0100".U
        }.otherwise {
          // when the a = 6 rounds
          roundReg := "b0110".U
        }
      }

    }
  }

}