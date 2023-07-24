package RoCCPrac

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import ascon._
import chisel3.core

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
  val block_size = RegInit(0.U(32.W))


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
  val key_value = RegInit(0.U(128.W))
  val nonce_value = RegInit(0.U(128.W))
  val init_ascon = RegInit(false.B)


  // val first_load = RegInit(false.B)
  val valid_ad_reg = RegInit(false.B)
  val tag_written = RegInit(false.B)
  /// ASCON Outputs
  val wire_cipher_stage = Wire(Bool())
  val wire_busy = Wire(Bool())
  val valid_tag_reg = RegInit(false.B)

  ///////////ASCON Declaration ////////////////
  val ASCON = Module(new ascon128RoCC2)

  // Set nonce
  val nonce_addr = RegInit(0.U(32.W))
  val load_nonce = RegInit(false.B)
  val valid_nonce = RegInit(false.B)
  val valid_key = RegInit(false.B)
  val key_addr = RegInit(0.U(32.W))
  val load_key = RegInit(false.B)
  // val load_init_nonce = RegInit(false.B)
  val wire_load_init = Wire(Bool())
  wire_load_init := function_ID === 4.U && state === s_idle

  when(io.cmd.fire()) {
    // load_init_nonce := false.B
    when(AEAD_Enc_ID) {
      resp_rd := io.cmd.bits.inst.rd
      resp_rd_data := "h0".U
      when(function_ID === 1.U) { // Set P
        plain_text_addr := io.cmd.bits.rs1
        plain_text_len := io.cmd.bits.rs2
        // plain_text_len := io.cmd.bits.inst.rs2
        resp_valid := true.B
        resp_rd_data := io.cmd.bits.rs1
        //resp_rd_data := Cat(block_size(2,0),init_ascon,wire_busy,ASCON.io.valid_tag,wire_cipher_stage,ASCON.io.C_valid, valid_tag_reg,state === s_idle,state(2,0))
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }

      }.elsewhen(function_ID === 2.U) { // Set AD
        associated_data_addr := io.cmd.bits.rs1
        associated_data_len := io.cmd.bits.rs2
        resp_valid := true.B
        //resp_rd_data := io.cmd.bits.rs1
        resp_rd_data := Cat(load_nonce, valid_nonce, block_size(2, 0), init_ascon, wire_busy, ASCON.io.valid_tag, wire_cipher_stage, ASCON.io.C_valid, valid_tag_reg, state === s_idle, state(2, 0))
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }
      }.elsewhen(function_ID === 3.U) { // Set C Tag
        cipher_text_addr := io.cmd.bits.rs1
        Tag_addr := io.cmd.bits.rs2
        resp_valid := true.B
        //resp_rd_data := io.cmd.bits.rs1
        resp_rd_data := Cat(load_nonce, valid_nonce, block_size(2, 0), init_ascon, wire_busy, ASCON.io.valid_tag, wire_cipher_stage, ASCON.io.C_valid, valid_tag_reg, state === s_idle, state(2, 0))
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }
      }.elsewhen(function_ID === 4.U) { // Set Nonce

        resp_rd_data := 0.U
        nonce_addr := io.cmd.bits.rs1
        state := s_mem_load_req
        resp_valid := false.B
        load_nonce := true.B
        block_size := 4.U
        valid_nonce := false.B
        // load_init_nonce := false.B
        block_counter := 0.U
        resp_rd_data := resp_rd_data | "h100000".U
        // load_init_nonce := true.B
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }

      }.elsewhen(function_ID === 5.U) { // Use Key
        key_value := 0.U
        resp_valid := true.B
        //resp_rd_data := 123.U
        resp_rd_data := Cat(load_nonce, valid_nonce, block_size(2, 0), init_ascon, wire_busy, ASCON.io.valid_tag, wire_cipher_stage, ASCON.io.C_valid, valid_tag_reg, state === s_idle, state(2, 0))
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }
      }.elsewhen(function_ID === 6.U) { // Init Enc
        init_ascon := true.B
        resp_rd_data := 0.U
        state := s_wait
        resp_valid := false.B
        tag_written := false.B
      }
    }.elsewhen(AEAD_Dec_ID) {
    }.elsewhen(Hash_ID) {
    }.elsewhen(Rand_ID) {
    }.elsewhen(KMU_ID) {

    }.otherwise {
      state := s_end
      resp_rd_data := "ha1a1a1a1".U
    }
  }



  //// Inputs
  ASCON.io.m_len := plain_text_len
  ASCON.io.ad_len := associated_data_len
  ASCON.io.M := Cat(vec_blocks_load(0), vec_blocks_load(1))
  ASCON.io.AD := Cat(vec_blocks_load(0), vec_blocks_load(1))
  ASCON.io.Key(0) := key_value(127, 64)
  ASCON.io.Key(1) := key_value(63, 0)
  ASCON.io.Npub(0) := nonce_value(127, 64)
  ASCON.io.Npub(1) := nonce_value(63, 0)
  ASCON.io.init := init_ascon
  ASCON.io.decrypt := false.B
  ASCON.io.hash_mode := false.B
  ASCON.io.tag_written := tag_written
  ASCON.io.write_busy := state === s_mem_write_req || state === s_write
  ASCON.io.read_busy := state === s_mem_load_req || state === s_load


  wire_cipher_stage := ASCON.io.cipher_stage
  wire_busy := ASCON.io.busy
  val load_addr_sel = Cat(wire_cipher_stage, load_key, load_nonce)
  val load_addr = MuxLookup(load_addr_sel, associated_data_addr, Array(
    "b000".U -> associated_data_addr,
    "b100".U -> plain_text_addr,
    "b010".U -> key_addr,
    "b001".U -> nonce_addr))

  io.busy := (state =/= s_idle || state =/= s_end)
  io.interrupt := false.B
  io.mem.req.valid := (state === s_mem_load_req || state === s_mem_write_req)
  io.mem.req.bits.addr := load_addr
  io.mem.req.bits.tag := load_addr
  //io.mem.req.bits.addr := Mux(wire_cipher_stage, plain_text_addr, associated_data_addr)
  //io.mem.req.bits.tag := Mux(wire_cipher_stage, plain_text_addr, associated_data_addr)
  io.mem.req.bits.cmd := M_XRD // M_XRD = load, M_XWR = write
  io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := 0.U // Data to be stored
  io.mem.req.bits.phys := false.B // 0 since we dont need translation


  when(ASCON.io.C_valid) {
    vec_blocks_write(0) := ASCON.io.C(63, 32)
    vec_blocks_write(1) := ASCON.io.C(31, 0)
  }

  when(valid_nonce) {
    nonce_value := Cat(vec_blocks_load(0), vec_blocks_load(1), vec_blocks_load(2), vec_blocks_load(3))
    resp_rd_data := Cat(vec_blocks_load(0)(7, 0), vec_blocks_load(1)(7, 0), vec_blocks_load(2)(7, 0), vec_blocks_load(3)(7, 0))
    valid_nonce := false.B
    load_nonce := false.B
  }
  when(ASCON.io.valid_tag && (state =/= s_idle || state =/= s_end)) {
    valid_tag_reg := true.B
    vec_blocks_write(0) := ASCON.io.Tag(0)(63, 32)
    vec_blocks_write(1) := ASCON.io.Tag(0)(31, 0)
    vec_blocks_write(2) := ASCON.io.Tag(1)(63, 32)
    vec_blocks_write(3) := ASCON.io.Tag(1)(31, 0)

  }

  val counter1 = RegInit(0.U(8.W))
  val counter2 = RegInit(0.U(8.W))
  val counter3 = RegInit(0.U(8.W))
  val counter5 = RegInit(0.U(8.W))
  val counter4 = RegInit(0.U(8.W))
  ASCON.io.valid_ad := valid_ad_reg

  valid_nonce := load_nonce && state === s_end

  switch(state) {
    is(s_idle) {
      io.busy := false.B
      // first_load := false.B
      valid_ad_reg := false.B
      block_size := 2.U
      tag_written := false.B
      valid_tag_reg := false.B
      //load_nonce := false.B
      // valid_nonce := false.B


    }

    is(s_mem_load_req) {
      // counter1 := counter1 +& 1.U
      /*when(counter1 >= "hff".U) {
        resp_rd_data := resp_rd_data | "h1".U
        state := s_end
      }*/
      resp_rd_data := resp_rd_data | "h2000".U
      io.mem.req.bits.addr := Mux(wire_cipher_stage, plain_text_addr, associated_data_addr)
      io.mem.req.bits.tag := Mux(wire_cipher_stage, plain_text_addr, associated_data_addr)
      // io.mem.req.bits.addr := load_addr
      // io.mem.req.bits.tag := load_addr
      io.mem.req.bits.cmd := M_XRD // M_XRD = load, M_XWR = write
      io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
      io.mem.req.bits.data := 0.U
      // Memory reques sent
      when(io.mem.req.fire()) {
        state := s_load
        counter1 := counter1 +& 1.U
        when(wire_cipher_stage) {
          plain_text_addr := plain_text_addr + 4.U
        }.elsewhen(load_nonce) {
          nonce_addr := nonce_addr + 4.U
        }.otherwise {
          associated_data_addr := associated_data_addr + 4.U
        }
      }
    }
    is(s_load) {
      //      counter2 := counter2 +& 1.U
      //      when(counter2 >= "hff".U) {
      //        resp_rd_data := resp_rd_data | "h2".U
      //        state := s_end
      //      }
      resp_rd_data := resp_rd_data | "h4000".U
      when(io.mem.resp.valid) {
        vec_blocks_load(block_counter) := io.mem.resp.bits.data
        block_counter := block_counter + 1.U
        when(block_counter < block_size - 1.U) {
          state := s_mem_load_req
        }.otherwise {
          valid_ad_reg := !load_nonce
          block_counter := 0.U
          when(load_nonce) {
            state := s_end

          }.otherwise {
            state := s_wait
          }
        }
      }
    }
    is(s_wait) {
      init_ascon := false.B
      counter3 := counter3 +& 1.U
      // resp_rd_data := resp_rd_data | "h1000".U
      /*when(counter3 >= "hff".U) {
        resp_rd_data := resp_rd_data | "h4".U
        state := s_end
      }*/
      // LOAD
      block_counter := 0.U
      when(ASCON.io.load_block) {
        state := s_mem_load_req
        valid_ad_reg := true.B
        //block_counter := 0.U
        block_size := 2.U
        resp_rd_data := resp_rd_data | "h100".U
      }.elsewhen(ASCON.io.C_valid) {
        state := s_mem_write_req
        block_size := 2.U
        //block_counter := 0.U
        resp_rd_data := resp_rd_data | "h200".U
      }.elsewhen(valid_tag_reg) {
        block_size := 4.U
        resp_rd_data := resp_rd_data | "h40000".U
        state := s_mem_write_req
      }
    }
    is(s_mem_write_req) {
      // counter4 := counter4 +& 1.U
      resp_rd_data := resp_rd_data | "h8000".U
      /*when(counter4 >= "hff".U) {
        resp_rd_data := resp_rd_data | "h8".U
        state := s_end
      }*/

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
        counter4 := counter4 +& 1.U
      }
    }

    is(s_write) {
      counter5 := counter5 +& 1.U
      resp_rd_data := resp_rd_data | "h10000".U

      /*when(counter5 >= "hff".U) {
        resp_rd_data := resp_rd_data | "h10".U
        state := s_end
      }*/
      when(io.mem.resp.valid) {
        //when(block_counter < 2.U) {
        when(block_counter < block_size) {
          state := s_mem_write_req
        }.otherwise {
          when(valid_tag_reg) {
            state := s_end
            tag_written := true.B
            resp_rd_data := Cat(counter1, counter2, counter4, counter5)
            valid_tag_reg := false.B
          }.otherwise {
            state := s_wait
          }
          block_counter := 0.U
        }
      }
    }


    is(s_end) {
      resp_rd_data := resp_rd_data | "h20000".U
      // resp_rd_data := half_load_reg(47, 16)
      resp_valid := true.B
      // resp_rd_data := 8.U // Identify the end of the program
      block_counter := 0.U
      block_size := 2.U
      state := s_idle
      io.busy := false.B
    }
  }

  // Response sent back to CPU
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := resp_rd_data
  when(io.resp.fire()) {
    state := s_idle
    io.busy := false.B
    resp_valid := false.B
  }
  io.resp.valid := state === s_end || resp_valid

}

class WithRWRoCC extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p: Parameters) => {
      val Accel = LazyModule(new WriteReadAcc(OpcodeSet.custom0)(p))
      Accel
    }
  )
})