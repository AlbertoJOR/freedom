package RoCCPrac

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import ascon._

class WriteReadAcc(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new RWImp(this)
}

class RWImp(outer: WriteReadAcc)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
  with HasCoreParameters {
  // The parts of the command are as follows
  // inst  - the parts of the instruction itself
  //   opcode
  //   rd  - destination register number
  //   rs1 - first source register number
  //   rs2 - second source register number
  //   funct
  //   xd  - is the destination register being used?
  //   xs1 - is the first source register being used?
  //   xs2 - is the second source register being used?
  //   rs1 - the value of source register 1
  //   rs2 - the value of source register 2

  val resp_rd = RegInit(0.U(5.W)) //Reg(io.resp.bits.rd)

  //val processed_words = RegInit(0.U(32.W))


  val s_idle :: s_mem_load_req :: s_load :: s_mem_write_req :: s_write :: s_wait :: s_end :: Nil = Enum(7)
  // Current state of the state machine
  val state = RegInit(s_idle)

  // Ready to receive new command when in idle state
  io.cmd.ready := (state === s_idle)
  // Cmd response is valid when in response state
  val resp_valid = RegInit(Bool(false))
  resp_valid := (state === s_end)
  // When finished just write 1.U to the rd destination
  val resp_rd_data = RegInit(0.U(32.W))
  val block_counter = RegInit(0.U(32.W))

  // ASCON Crtl
  val vec_blocks_write = RegInit(Vec(Seq.fill(4)(7.U(32.W))))
  val vec_blocks_load = RegInit(Vec(Seq.fill(4)(7.U(32.W))))
  val wire_load_busy = Wire(Bool())
  val wire_write_busy = Wire(Bool())


  val block_size = RegInit(2.U(32.W))


  val module_ID = io.cmd.bits.inst.funct(6, 4)
  val function_ID = io.cmd.bits.inst.funct(3, 0)

  val AEAD_Enc_ID = module_ID === 1.U
  val AEAD_Dec_ID = module_ID === 2.U
  val Hash_ID = module_ID === 3.U
  val Rand_ID = module_ID === 4.U
  val KMU_ID = module_ID === 5.U

  /// AEAD
  val plain_text_addr = RegInit(0.U(32.W))
  val plain_text_len = RegInit(0.U(32.W))
  val associated_data_addr = RegInit(0.U(32.W))
  val associated_data_len = RegInit(0.U(32.W))
  val cipher_text_addr = RegInit(0.U(32.W))
  val Tag_addr = RegInit(0.U(32.W))
  val Key_value = RegInit(0.U(128.W))
  val Nonce_value = RegInit(0.U(128.W))
  val init_ascon = RegInit(false.B)


  when(io.cmd.fire()) {
    when(AEAD_Enc_ID) {
      resp_rd := io.cmd.bits.inst.rd
      resp_rd_data := "h3454".U
      when(function_ID === 1.U) { // Set P
        plain_text_addr := io.cmd.bits.inst.rs1
        plain_text_len := io.cmd.bits.inst.rs2
        resp_valid := true.B
        resp_rd_data := 1.U
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }

      }.elsewhen(function_ID === 2.U) { // Set AD
        associated_data_addr := io.cmd.bits.inst.rs1
        associated_data_len := io.cmd.bits.inst.rs2
        resp_valid := true.B
        resp_rd_data := 2.U
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }
      }.elsewhen(function_ID === 3.U) { // Set C Tag
        cipher_text_addr := io.cmd.bits.inst.rs1
        Tag_addr := io.cmd.bits.inst.rs2
        resp_valid := true.B
        resp_rd_data := 3.U
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }
      }.elsewhen(function_ID === 4.U) { // Set Nonce
        Nonce_value := 0.U
        resp_valid := true.B
        resp_rd_data := 4.U
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }
      }.elsewhen(function_ID === 5.U) { // Use Key
        Key_value := 518.U
        resp_valid := true.B
        resp_rd_data := 5.U
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }
      }.elsewhen(function_ID === 6.U) { // Init Enc
        init_ascon := true.B
        resp_rd_data := 14.U
        state := s_wait
        resp_valid := false.B
      }
    }.elsewhen(AEAD_Dec_ID) {
    }.elsewhen(Hash_ID) {
    }.elsewhen(Rand_ID) {
    }.elsewhen(KMU_ID) {

    }.otherwise {
      state := s_end
      resp_rd_data := "h1234".U
    }
  }


  ///////////ASCON Declaration ////////////////
  val ASCON = Module(new ascon128RoCC)
  //// Inputs
  ASCON.io.m_len := plain_text_len
  ASCON.io.ad_len := associated_data_len
  ASCON.io.M := Cat(vec_blocks_load(0), vec_blocks_load(1))
  ASCON.io.AD := Cat(vec_blocks_load(0), vec_blocks_load(1))
  ASCON.io.Key(0) := Key_value(127, 64)
  ASCON.io.Key(1) := Key_value(63, 0)
  ASCON.io.Npub(0) := Nonce_value(127, 64)
  ASCON.io.Npub(1) := Nonce_value(63, 0)
  ASCON.io.init := init_ascon
  ASCON.io.decrypt := false.B
  ASCON.io.hash_mode := false.B
  ASCON.io.write_busy := wire_write_busy
  ASCON.io.read_busy := wire_load_busy
  /// ASCON Outputs
  val wire_load_block = RegInit(false.B)
  val wire_cipher_stage = Wire(Bool())
  val wire_busy = Wire(Bool())
  val wire_cipher_text = Wire(UInt(64.W))
  val wire_write_block = RegInit(false.B)
  val wire_Tag = Wire(Vec(2, UInt(64.W)))
  val wire_Tag_valid = RegInit(false.B)

  wire_load_block := ASCON.io.load_block
  wire_cipher_stage := ASCON.io.cipher_stage
  wire_busy := ASCON.io.busy
  wire_cipher_text := ASCON.io.C
  wire_write_block := ASCON.io.C_valid
  wire_Tag := ASCON.io.Tag
  wire_Tag_valid := ASCON.io.valid_tag


  // Response sent back to CPU
  when(io.resp.fire()) {
    state := s_idle
  }


  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := (state === s_mem_load_req || state === s_mem_write_req)
  io.mem.req.bits.addr := Mux(wire_cipher_stage, plain_text_addr, associated_data_addr)
  io.mem.req.bits.tag := Mux(wire_cipher_stage, plain_text_addr, associated_data_addr)
  io.mem.req.bits.cmd := M_XRD // M_XRD = load, M_XWR = write
  io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := 0.U // Data to be stored
  io.mem.req.bits.phys := false.B // 0 since we dont need translation

  wire_load_busy := state === s_mem_load_req || state === s_load
  wire_write_busy := false.B //state === s_mem_write_req || state === s_write

  when(ASCON.io.C_valid) {
    wire_write_block := true.B
    vec_blocks_write(0) := wire_cipher_text(63, 32)
    vec_blocks_write(1) := wire_cipher_text(31, 0)

  }
  when(ASCON.io.valid_tag) {
    wire_Tag_valid := true.B
    //tag_valid_reg := true.B
    vec_blocks_write(0) := wire_Tag(0)(63, 32)
    vec_blocks_write(1) := wire_Tag(0)(31, 0)
    vec_blocks_write(2) := wire_Tag(1)(63, 32)
    vec_blocks_write(3) := wire_Tag(1)(31, 0)

  }
  when(ASCON.io.load_block) {
    wire_load_block := true.B
  }

  val counter = RegInit(0.U(32.W))

  switch(state) {
    is(s_idle) {
      io.busy := false.B
    }

    is(s_mem_load_req) {
      wire_load_block := false.B
      wire_Tag_valid := false.B
      io.mem.req.bits.addr := Mux(wire_cipher_stage, plain_text_addr, associated_data_addr)
      io.mem.req.bits.tag := Mux(wire_cipher_stage, plain_text_addr, associated_data_addr)
      io.mem.req.bits.cmd := M_XRD // M_XRD = load, M_XWR = write
      io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
      io.mem.req.bits.data := 0.U
      // Memory reques sent
      when(io.mem.req.fire()) {
        state := s_load
        when(wire_cipher_stage) {
          plain_text_addr := plain_text_addr + 4.U
        }.otherwise {
          associated_data_addr := associated_data_len + 4.U
        }
      }
    }
    is(s_load) {
      when(io.mem.resp.valid) {
        vec_blocks_load(block_counter) := io.mem.resp.bits.data
        block_counter := block_counter + 1.U
        when(block_counter < block_size - 1.U) {
          state := s_mem_load_req
        }.otherwise {
          state := s_wait
          block_counter := 0.U
          init_ascon := false.B
          // load_block_reg := false.B
          // tag_valid_reg := false.B
        }
      }
    }
    is(s_wait) {
      counter := Mux(counter < 20000.U, counter + 1.U, 0.U)
      when(counter > 10000.U) {
        state := s_end
        resp_rd_data := "hc0".U
      }
      when(!wire_busy) {
        state := s_end
        resp_rd_data := "ha1".U
      }
      //when(wire_load_block) {
      // state := s_mem_load_req
      // }.elsewhen(wire_write_block) {
      //  state := s_mem_write_req
      when(wire_Tag_valid) {
        block_size := 4.U
        resp_rd_data := 21.U
        state := s_end


      }
    }
    is(s_mem_write_req) {
      wire_write_block := false.B

      io.mem.req.bits.addr := cipher_text_addr
      io.mem.req.bits.tag := cipher_text_addr(5, 0) // differentiate between responses
      io.mem.req.bits.cmd := M_XWR // M_XRD = load, M_XWR = write
      io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
      io.mem.req.bits.data := vec_blocks_write(block_counter)
      // Memory request sent
      when(io.mem.req.fire()) {
        cipher_text_addr := cipher_text_addr + 4.U
        block_counter := block_counter + 1.U
        state := s_write
      }
    }

    is(s_write) {
      when(io.mem.resp.valid) {
        //when(block_counter < 2.U) {
        when(block_counter < block_size) {
          state := s_mem_write_req
        }.otherwise {
          state := s_wait
          block_counter := 0.U
          when(wire_Tag_valid) {
            state := s_end
          }
        }
      }
    }


    is(s_end) {
      resp_valid := true.B
      // resp_rd_data := 8.U // Identify the end of the program
      block_counter := 0.U
      block_size := 2.U
      state := s_idle
      io.busy := false.B
    }
  }

  // Response sent back to CPU
  io.resp.valid := resp_valid
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := resp_rd_data
  when(io.resp.fire()) {
    state := s_idle
    io.busy := false.B
  }


}

class WithRWRoCC extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p: Parameters) => {
      val Accel = LazyModule(new WriteReadAcc(OpcodeSet.custom0)(p))
      Accel
    }
  )
})