package ascon

import chisel3._
import chisel3.util._
class IOAsconHandler extends Module {
  val io = IO(new Bundle{
    val m_block = Input(UInt(64.W))
    val a_block = Input(UInt(64.W))
    val c_block = Output(UInt(64.W))
    val key = Input(UInt(128.W))
    val iv = Input(UInt(68.W))
    val nonce = Input(UInt(128.W))

    val tag = Output(UInt(64.W))
    val cr_in =Input(Vec(5, UInt(64.W))) // capacity and rate input from permutation
    val cr_out =Output(Vec(5, UInt(64.W))) // capacity and rate feed to the permutation
    // Control signals
    val stage_ascon = Input(UInt(4.W)) // stage of the algorithm (Init, Associated, Message, Tag)
    val last_block = Input(Bool())
    val first_block = Input(Bool())
    val padding = Input(Bool())
    val valid_c = Output(Bool())
    val valid_tag = Output(Bool())


  })
  object Stages {
  val s_rst     = (0x0.U)
  val s_idle    = (0x1.U)
  val s_init    = (0x2.U)
  val s_init_key= (0x3.U)
  val s_asso    = (0x4.U)
  val s_asso_l  = (0x5.U)
  val s_ciph    = (0x6.U)
  val s_ciph_l  = (0x7.U)
  val s_tag_k   = (0x8.U)
  val s_tag     = (0x9.U)
}
  val cr_wire = Wire(Vec(5, UInt(64.W)))
  val valid_c_wire = Wire(Bool())
  val valid_tag_wire = Wire(Bool())
  val tag_wire = Wire(Bool())
  val c_block_wire = UInt(64.W)


  io.cr_out := cr_wire
  io.valid_c := valid_c_wire
  io.valid_tag := valid_tag_wire
  io.tag := tag_wire
  io.c_block := c_block_wire
  // val cr_reg := RegNext(cr_wire, VecInit.fill(5)(0.U(64.W)))

  when(io.stage_ascon === Stages.s_rst){
    cr_wire.foreach(u => u:= 0x0.U)
    valid_c_wire := false.B
    valid_tag_wire :=  false.B
    tag_wire := 0x0.U
    c_block_wire := 0x0.U
  }.elsewhen(io.stage_ascon === Stages.s_idle){
    cr_wire.foreach(u => u:= 0x0.U)
    valid_c_wire := false.B
    valid_tag_wire :=  false.B
    tag_wire := 0x0.U
    c_block_wire := 0x0.U
  }.elsewhen(io.stage_ascon === Stages.s_init){

  }

  

}