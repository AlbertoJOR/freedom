package ascon

import ascon.permutation.PermutationPa
import chisel3._
import chisel3.util._
import ascon.util._

class asconRoCC2(r_c: Int, w_c:Int) extends Module {
  val io = IO(new Bundle() {
    val m_len = Input(UInt(32.W))
    val ad_len = Input(UInt(32.W))
    val M = Input(UInt(64.W))
    val AD = Input(UInt(64.W))
    val Key = Input(Vec(2, UInt(64.W)))
    val Npub = Input(Vec(2, UInt(64.W)))
    val init = Input(Bool())
    val decrypt = Input(Bool())
    val hash_mode = Input(Bool())

    // RoCC Control
    val cipher_stage = Output(Bool())
    val load_to_write = Output(Bool())


    val busy = Output(Bool())

    val C = Output(UInt(64.W))
    // Module Tag
    val valid_tag = Output(Bool())
    val Tag  = Output(Vec(2, UInt(64.W)))
    val Hash = Output(Vec(4, UInt(64.W)))
    val valid_hash = Output(Bool())
  })

  val Ascon = Module(new ascon128RoCC2)
  val MemFSM = Module(new memFSM(r_c, w_c))

  Ascon.io.m_len := io.m_len
  Ascon.io.ad_len := io.ad_len
  Ascon.io.M := io.M
  Ascon.io.AD := io.AD
  Ascon.io.Key := io.Key
  Ascon.io.Npub := io.Npub
  Ascon.io.init := io.init
  Ascon.io.decrypt := io.decrypt
  Ascon.io.hash_mode :=io.hash_mode
  io.cipher_stage := Ascon.io.cipher_stage
  io.load_to_write := MemFSM.io.load_to_write
  io.busy := Ascon.io.busy
  io.C := Ascon.io.C
  io.valid_tag := Ascon.io.valid_tag
  io.Tag := Ascon.io.Tag
  io.Hash := Ascon.io.Hash
  io.valid_hash := Ascon.io.valid_hash


  MemFSM.io.load_block := Ascon.io.load_block
  MemFSM.io.write_block := Ascon.io.C_valid

  Ascon.io.read_busy := MemFSM.io.busy_load
  Ascon.io.write_busy := MemFSM.io.busy_write


}
