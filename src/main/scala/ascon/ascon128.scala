package ascon

import ascon.permutation.PermutationPa
import chisel3._
import chisel3.util._

class ascon128 extends Module {
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


    val busy = Output(Bool())

    val C = Output(UInt(64.W))
    val C_valid = Output(Bool())
    // Module Tag
    val valid_tag = Output(Bool())
    val Tag  = Output(Vec(2, UInt(64.W)))
    val Hash = Output(Vec(4, UInt(64.W)))
    val valid_hash = Output(Bool())
  })

  val MuxRate = Module(new MuxInRate)
  val MuxCap = Module(new MuxInCapacity)
  val MuxMC = Module(new CipheredMux)
  val ToState = Module(new Separator)
  val TagGen = Module(new RateCapacityTag)
  val Ctrl = Module(new asconCtrl64)
  val Asconp = Module(new PermutationPa)
  val HashReg = RegInit(VecInit(Seq.fill(4)(0.U(64.W))))


  MuxRate.io.as_data := io.AD
  MuxRate.io.iv := Mux(io.hash_mode, "h00400c0000000100".U ,"h80400c0600000000".U)
  MuxRate.io.rate_in := TagGen.io.rate_o
  MuxRate.io.c_init  := Ctrl.io.c_rate_mux(2)
  MuxRate.io.c_as_dt := Ctrl.io.c_rate_mux(1)
  MuxRate.io.c_a_last := Ctrl.io.c_rate_mux(0)
  MuxRate.io.a_bytes := Ctrl.io.bytes_pad

  MuxCap.io.key := Cat(io.Key(0), io.Key(1))
  MuxCap.io.nonce := Cat(io.Npub(0), io.Npub(1))
  MuxCap.io.capacity_in := TagGen.io.capacity_o
  MuxCap.io.c_init := Ctrl.io.c_capacity_mux(4)
  MuxCap.io.c_a_init   := Ctrl.io.c_capacity_mux(3)
  MuxCap.io.c_a_domain   := Ctrl.io.c_capacity_mux(2)
  MuxCap.io.c_no_a   := Ctrl.io.c_capacity_mux(1)
  MuxCap.io.c_fin   := Ctrl.io.c_capacity_mux(0)

  MuxMC.io.plain := io.M
  MuxMC.io.rate := TagGen.io.rate_o
  MuxMC.io.c_cipher := Ctrl.io.c_cipher_mux(1)
  MuxMC.io.c_c_last := Ctrl.io.c_cipher_mux(0) || Ctrl.io.last_cipher_block
  MuxMC.io.p_bytes := Ctrl.io.bytes_pad
  MuxMC.io.valid_per := Asconp.io.valid
  MuxMC.io.decrypt_mode := io.decrypt
  io.C := MuxMC.io.cipher_text
  io.C_valid:= MuxMC.io.valid

  // Control
  Ctrl.io.asso_len := io.ad_len
  Ctrl.io.plain_len := io.m_len
  Ctrl.io.c_init := io.init
  Ctrl.io.valid_per:= Asconp.io.valid
  Ctrl.io.busy_per := Asconp.io.busy


  val rate_in = Mux(Ctrl.io.ciphering, MuxMC.io.S_out, MuxRate.io.S0_xor)

  ToState.io.rate := rate_in
  ToState.io.capacity := MuxCap.io.capacity_out

  Asconp.io.A := ToState.io.S
  Asconp.io.typePer := Ctrl.io.type_per
  Asconp.io.start := Ctrl.io.init_perm

  TagGen.io.S_i := Asconp.io.S
  TagGen.io.key := Cat(io.Key(0), io.Key(1))
  TagGen.io.c_tag := Ctrl.io.c_tag
  io.Tag(0) := TagGen.io.tag(127, 64)
  io.Tag(1) := TagGen.io.tag(63, 0)
  io.valid_tag := TagGen.io.valid

  io.busy := Ctrl.io.busy


  // Hash Register
  MuxMC.io.hash_mode := io.hash_mode
  io.valid_hash := Ctrl.io.valid_hash
  Ctrl.io.hash_mode := io.hash_mode
  MuxMC.io.hash_stage := Ctrl.io.hash_stage
  io.Hash := HashReg
  when(Ctrl.io.hash_stage && MuxMC.io.valid ){
    HashReg(Ctrl.io.hash_index(1,0)) := MuxMC.io.cipher_text
  }




}
