package ascon

import ascon.permutation._
import chisel3._
import chisel3.util._
import rand._

class ascon128RoCC2(withTrivium: Boolean, unrolled: Int = 1) extends Module {
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
    val write_busy = Input(Bool())
    val read_busy = Input(Bool())
    val load_block = Output(Bool())
    val cipher_stage = Output(Bool())


    val busy = Output(Bool())

    val C = Output(UInt(64.W))
    val C_valid = Output(Bool()) // Load block
    // Module Tag
    val valid_tag = Output(Bool())
    val Tag = Output(Vec(2, UInt(64.W)))
    val Hash = Output(Vec(4, UInt(64.W)))
    val valid_ad = Input(Bool())
    val valid_hash = Output(Bool())

    val tag_written = Input(Bool())
    val hash_written = Input(Bool())

    // RAMDOM//
    val random = Input(Bool())
    val seed = Input(Bool())
    val finish_rand= Output(Bool())
  })

  val MuxRate = Module(new MuxInRate)
  val MuxCap = Module(new MuxInCapacity)
  val MuxMC = Module(new CipheredMux)
  val ToState = Module(new Separator)
  val TagGen = Module(new RateCapacityTag)
  val Ctrl = Module(new asconCtrlRocc2)
  //val Asconp = Module(new PermutationPa)
  val HashReg = RegInit(VecInit(Seq.fill(4)(0.U(64.W))))
  val write_block_reg = RegInit(false.B)
  // Unrolled //
  val Asconp = if(unrolled == 3) {
    Module(new PermutationPa3)
  } else if(unrolled == 2){
    Module(new PermutationPa2)
  } else{
    Module(new PermutationPa)
  }
  // RANDOM ///
  val trivium = if (withTrivium) {
    Module(new trivium)
  } else {
    Module(new notTrivium)
  }
  // val trivium = Module(new trivium)
  val randFSM = Module(new randFSM2)
  val AState = Module(new randAsconState)

  val random_mode = randFSM.io.rand_mux || randFSM.io.seed_mux
  //val kmu = Module(new KMU(8))

  MuxRate.io.as_data := io.AD
  MuxRate.io.iv := Mux(io.hash_mode, "h00400c0000000100".U, "h80400c0600000000".U)
  MuxRate.io.rate_in := TagGen.io.rate_o
  MuxRate.io.c_init := Ctrl.io.c_rate_mux(2)
  MuxRate.io.c_as_dt := Ctrl.io.c_rate_mux(1)
  MuxRate.io.c_a_last := Ctrl.io.c_rate_mux(0)
  MuxRate.io.a_bytes := Ctrl.io.bytes_pad
  MuxRate.io.valid_ad := io.valid_ad

  MuxCap.io.key := Cat(io.Key(0), io.Key(1))
  MuxCap.io.nonce := Cat(io.Npub(0), io.Npub(1))
  MuxCap.io.capacity_in := TagGen.io.capacity_o
  MuxCap.io.c_init := Ctrl.io.c_capacity_mux(4)
  MuxCap.io.c_a_init := Ctrl.io.c_capacity_mux(3)
  MuxCap.io.c_a_domain := Ctrl.io.c_capacity_mux(2)
  MuxCap.io.c_no_a := Ctrl.io.c_capacity_mux(1)
  MuxCap.io.c_fin := Ctrl.io.c_capacity_mux(0)

  MuxMC.io.plain := io.M
  MuxMC.io.rate := TagGen.io.rate_o
  MuxMC.io.c_cipher := Ctrl.io.c_cipher_mux(1)
  MuxMC.io.c_c_last := Ctrl.io.c_cipher_mux(0) || Ctrl.io.last_cipher_block
  MuxMC.io.p_bytes := Ctrl.io.bytes_pad
  MuxMC.io.valid_per := Asconp.io.valid
  MuxMC.io.decrypt_mode := io.decrypt
  MuxMC.io.block_zero := Ctrl.io.block_zero
  MuxMC.io.has_inc_block := Ctrl.io.has_inc_plain_block
  io.C := Mux(randFSM.io.rand_mux, AState.io.state(0), MuxMC.io.cipher_text)

  when(io.write_busy) {
    write_block_reg := false.B
  }.elsewhen(MuxMC.io.valid && Ctrl.io.busy && Ctrl.io.can_write) {
    write_block_reg := true.B
  }

  io.C_valid := Mux(randFSM.io.rand_mux, randFSM.io.valid_rand, write_block_reg)

  // Control
  Ctrl.io.asso_len := io.ad_len
  Ctrl.io.plain_len := io.m_len
  Ctrl.io.c_init := io.init
  Ctrl.io.valid_per := Asconp.io.valid
  Ctrl.io.busy_per := Asconp.io.busy
  Ctrl.io.tag_written := io.tag_written


  val rate_in = Mux(Ctrl.io.ciphering, MuxMC.io.S_out, MuxRate.io.S0_xor)

  ToState.io.rate := rate_in
  ToState.io.capacity := MuxCap.io.capacity_out

  Asconp.io.A := Mux(random_mode, AState.io.state, ToState.io.S)
  Asconp.io.typePer := Mux(random_mode, randFSM.io.type_per, Ctrl.io.type_per)
  Asconp.io.start := Mux(random_mode, randFSM.io.init_per, Ctrl.io.init_perm)
  Asconp.io.rst_per := Ctrl.io.rst_per
  // Asconp.io.rst_per := Mux(random_mode, Ctrl.io.rst_per, false.B)

  TagGen.io.S_i := Asconp.io.S
  TagGen.io.key := Cat(io.Key(0), io.Key(1))
  TagGen.io.c_tag := Ctrl.io.c_tag
  io.Tag(0) := TagGen.io.tag(127, 64)
  io.Tag(1) := TagGen.io.tag(63, 0)
  io.valid_tag := TagGen.io.valid

  io.busy := Ctrl.io.busy || randFSM.io.busy




  // Hash Register
  MuxMC.io.hash_mode := io.hash_mode
  io.valid_hash := Ctrl.io.valid_hash
  Ctrl.io.hash_mode := io.hash_mode
  MuxMC.io.hash_stage := Ctrl.io.hash_stage
  io.Hash := HashReg
  when(Ctrl.io.hash_stage && write_block_reg) {
    HashReg(Ctrl.io.hash_index(1, 0)) := MuxMC.io.cipher_text
  }

  // RoCC Ctrl
  Ctrl.io.write_busy := io.write_busy
  Ctrl.io.read_busy := io.read_busy
  io.load_block := Ctrl.io.load_block
  io.cipher_stage := Ctrl.io.ciphering

  // Random
  /// trivium
  trivium.io.reset := false.B
  trivium.io.K := "h8".U
  trivium.io.iv := "h7".U
  // Rando FSM
  randFSM.io.rand := io.random
  randFSM.io.seed := io.seed
  randFSM.io.busy_per := Asconp.io.busy

  randFSM.io.valid_per := Asconp.io.valid
  randFSM.io.num_rand := Mux(io.random, io.m_len, 0.U)
  randFSM.io.busy_write := io.write_busy
  randFSM.io.valid_seed := trivium.io.valid
  io.finish_rand := randFSM.io.finish
  // randFSM.io.new_init := io.init

  AState.io.seed := randFSM.io.seed_mux
  AState.io.seed_valid := randFSM.io.seed_valid
  AState.io.seed_value := trivium.io.Z
  AState.io.new_state := Asconp.io.S
  AState.io.valid_state := Asconp.io.valid && random_mode // random




  //KMU
  /*kmu.io.key_in := 9.U
  kmu.io.key_id := 0.U
  kmu.io.store_key := false.B
  kmu.io.get_key := false.B
  kmu.io.delete_key := false.B
  kmu.io.get_master := false.B*/

  Ctrl.io.hash_written := io.hash_written


}
