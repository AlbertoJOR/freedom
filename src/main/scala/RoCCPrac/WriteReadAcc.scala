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
  val vec_blocks_write = RegInit(Vec(Seq.fill(4)(127.U(32.W))))
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

  val counter6 = RegInit(0.U(32.W))


  when(io.cmd.fire()) {
    when(AEAD_Enc_ID) {
      resp_rd := io.cmd.bits.inst.rd
      resp_rd_data := "h0".U
      when(function_ID === 1.U) { // Set P
        plain_text_addr := io.cmd.bits.rs1
        plain_text_len := io.cmd.bits.rs2
        // plain_text_len := io.cmd.bits.inst.rs2
        resp_valid := true.B
        resp_rd_data := io.cmd.bits.rs2
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }

      }.elsewhen(function_ID === 2.U) { // Set AD
        associated_data_addr := io.cmd.bits.rs1
        associated_data_len := io.cmd.bits.rs2
        resp_valid := true.B
        resp_rd_data := io.cmd.bits.rs2
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }
      }.elsewhen(function_ID === 3.U) { // Set C Tag
        cipher_text_addr := io.cmd.bits.rs1
        Tag_addr := io.cmd.bits.rs2
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
        resp_rd_data := 0.U
        state := s_wait
        resp_valid := false.B
        counter6 := Mux(counter6 < 2000000.U, counter6 + 1.U, 0.U)
        when(counter6 > 1000000.U) {
          state := s_end
          resp_rd_data := "hffffffffffffffff".U
        }

      }
    }.elsewhen(AEAD_Dec_ID) {
    }.elsewhen(Hash_ID) {
    }.elsewhen(Rand_ID) {
    }.elsewhen(KMU_ID) {

    }.otherwise {
      state := s_end
      resp_rd_data := "haaaaaaaaaaaaa".U
    }
  }


  ///////////ASCON Declaration ////////////////
  val ASCON = Module(new ascon128RoCC2)
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
  val wire_load_block = Wire(Bool())
  val wire_cipher_stage = Wire(Bool())
  val load_block_delay = Reg(Bool())
  val write_block_delay = Reg(Bool())
  val wire_busy = Wire(Bool())
  val wire_cipher_text = Wire(UInt(64.W))
  val wire_write_block = Wire(Bool())
  val wire_Tag = Wire(Vec(2, UInt(64.W)))
  val wire_Tag_valid = RegInit(false.B)

  wire_load_block := ASCON.io.load_block
  wire_cipher_stage := ASCON.io.cipher_stage
  wire_busy := ASCON.io.busy
  wire_cipher_text := ASCON.io.C
  wire_write_block := ASCON.io.C_valid
  wire_Tag := ASCON.io.Tag
  wire_Tag_valid := ASCON.io.valid_tag

  load_block_delay := wire_load_block
  write_block_delay := wire_write_block


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
  // wire_load_busy := false.B
  wire_write_busy := state === s_mem_write_req || state === s_write
  // wire_write_busy := false.B

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

  val counter1 = RegInit(0.U(32.W))
  val counter2 = RegInit(0.U(32.W))
  val counter3 = RegInit(0.U(32.W))
  val counter5 = RegInit(0.U(32.W))
  val counter4 = RegInit(0.U(32.W))

  when(wire_load_busy && ASCON.io.load_block) {
    resp_rd_data := resp_rd_data | "h400".U
  }
  when(wire_write_busy && ASCON.io.C_valid) {
    resp_rd_data := resp_rd_data | "h800".U
  }

  switch(state) {
    is(s_idle) {
      io.busy := false.B
    }

    is(s_mem_load_req) {
      counter1 := Mux(counter1 < 2000000.U, counter1 + 1.U, 0.U)
      when(counter1 > 1000000.U) {
        resp_rd_data := resp_rd_data | "h1".U
        state := s_end
      }
      resp_rd_data := resp_rd_data | "h2000".U
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
          associated_data_addr := associated_data_addr + 4.U
        }
      }
    }
    is(s_load) {
      counter2 := Mux(counter2 < 2000000.U, counter2 + 1.U, 0.U)
      when(counter2 > 1000000.U) {
        resp_rd_data := resp_rd_data | "h2".U
        state := s_end
      }
      resp_rd_data := resp_rd_data | "h4000".U
      when(io.mem.resp.valid) {
        vec_blocks_load(block_counter) := io.mem.resp.bits.data
        block_counter := block_counter + 1.U
        when(block_counter < block_size - 1.U) {
          state := s_mem_load_req
        /*}.elsewhen(wire_write_block && !write_block_delay) {
          state := s_mem_write_req
          block_counter := 0.U
          resp_rd_data := resp_rd_data | "h200".U*/
        }.otherwise {
          state := s_wait
          block_counter := 0.U
          // load_block_reg := false.B
          // tag_valid_reg := false.B
        }
      }
    }
    is(s_wait) {
      init_ascon := false.B
      counter3 := Mux(counter3 < 2000000.U, counter3 + 1.U, 0.U)
      resp_rd_data := resp_rd_data | "h1000".U
      when(counter3 > 10000.U) {
        resp_rd_data := resp_rd_data | "h4".U
        state := s_end
      }
      // LOAD
      when(wire_load_block ) {
        state := s_mem_load_req
        block_counter := 0.U
        resp_rd_data := resp_rd_data | "h100".U
      }
      when(wire_write_block ) {
        state := s_mem_write_req
        block_counter := 0.U
        resp_rd_data := resp_rd_data | "h200".U
      }

      when(wire_Tag_valid) {
        block_size := 4.U
        resp_rd_data := resp_rd_data | "h40000".U
        state := s_end
      }
    }
    is(s_mem_write_req) {
      counter4 := Mux(counter4 < 2000000.U, counter4 + 1.U, 0.U)
      resp_rd_data := resp_rd_data | "h8000".U
      when(counter4 > 1000000.U) {
        resp_rd_data := resp_rd_data | "h8".U
        state := s_end
      }

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
      counter5 := Mux(counter5 < 2000000.U, counter5 + 1.U, 0.U)
      resp_rd_data := resp_rd_data | "h10000".U
      when(counter5 > 1000000.U) {
        resp_rd_data := resp_rd_data | "h10".U
        state := s_end
      }
      when(io.mem.resp.valid) {
        //when(block_counter < 2.U) {
        when(block_counter < block_size) {
          state := s_mem_write_req
        }.otherwise {
          state := s_wait
          block_counter := 0.U
        }
      }
    }


    is(s_end) {
      resp_rd_data := resp_rd_data | "h20000".U
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