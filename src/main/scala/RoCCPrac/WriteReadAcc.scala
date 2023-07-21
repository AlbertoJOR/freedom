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
  val Key_value = RegInit(0.U(128.W))
  val Nonce_value = RegInit(0.U(128.W))
  val init_ascon = RegInit(false.B)

  val counter6 = RegInit(0.U(32.W))

  val need_load = RegInit(false.B)
  val need_write = RegInit(false.B)
  val first_load = RegInit(false.B)
  val half_load_reg = RegInit(0.U(64.W))
  val valid_ad_reg = RegInit(false.B)


  when(io.cmd.fire()) {
    when(AEAD_Enc_ID) {
      resp_rd := io.cmd.bits.inst.rd
      resp_rd_data := "h0".U
      when(function_ID === 1.U) { // Set P
        plain_text_addr := io.cmd.bits.rs1
        plain_text_len := io.cmd.bits.rs2
        // plain_text_len := io.cmd.bits.inst.rs2
        resp_valid := true.B
        resp_rd_data := io.cmd.bits.rs1
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }

      }.elsewhen(function_ID === 2.U) { // Set AD
        associated_data_addr := io.cmd.bits.rs1
        associated_data_len := io.cmd.bits.rs2
        resp_valid := true.B
        resp_rd_data := io.cmd.bits.rs1
        when(io.resp.fire()) {
          io.busy := false.B
          resp_valid := false.B
        }
      }.elsewhen(function_ID === 3.U) { // Set C Tag
        cipher_text_addr := io.cmd.bits.rs1
        Tag_addr := io.cmd.bits.rs2
        resp_valid := true.B
        resp_rd_data := io.cmd.bits.rs1
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
        Key_value := 0.U
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
          resp_rd_data := "hc8c8c8c8".U
        }

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
  wire_load_busy := state === s_mem_load_req || state === s_load
  // wire_load_busy := false.B
  wire_write_busy := state === s_mem_write_req || state === s_write
  // wire_write_busy := false.B



  ASCON.io.write_busy := wire_write_busy
  ASCON.io.read_busy := wire_load_busy
  /// ASCON Outputs
  val wire_cipher_stage = Wire(Bool())
  val wire_busy = Wire(Bool())
  val wire_cipher_text = Wire(UInt(64.W))
  val wire_Tag = Wire(Vec(2, UInt(64.W)))
  val valid_tag_reg = RegInit(false.B)

  wire_cipher_stage := ASCON.io.cipher_stage
  wire_busy := ASCON.io.busy
  wire_cipher_text := ASCON.io.C
  wire_Tag := ASCON.io.Tag



  // Response sent back to CPU
  /* when(io.resp.fire()) {
     state := s_idle
   }*/


  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := (state === s_mem_load_req || state === s_mem_write_req)
  io.mem.req.bits.addr := Mux(wire_cipher_stage, plain_text_addr, associated_data_addr)
  io.mem.req.bits.tag := Mux(wire_cipher_stage, plain_text_addr, associated_data_addr)
  io.mem.req.bits.cmd := M_XRD // M_XRD = load, M_XWR = write
  io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := 0.U // Data to be stored
  io.mem.req.bits.phys := false.B // 0 since we dont need translation

  when(ASCON.io.load_block && !wire_load_busy && !wire_write_busy) {
    need_load := true.B
  }
  /*when(ASCON.io.C_valid && !wire_load_busy && !wire_write_busy) {
    vec_blocks_write(0) := wire_cipher_text(63, 32)
    vec_blocks_write(1) := wire_cipher_text(31, 0)
    need_write := true.B
  }*/
  when(ASCON.io.C_valid) {
    vec_blocks_write(0) := ASCON.io.C(63, 32)
    vec_blocks_write(1) := ASCON.io.C(31, 0)
  }
  when(ASCON.io.valid_tag) {
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


  switch(state) {
    is(s_idle) {
      io.busy := false.B
      need_load := false.B
      need_write := false.B
      first_load := false.B
      half_load_reg := 666.U
      valid_ad_reg := false.B
      block_size := 2.U
      valid_tag_reg := false.B
    }

    is(s_mem_load_req) {
      // counter1 := counter1 +& 1.U
      when(counter1 >= "hff".U) {
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
        counter1 := counter1 +& 1.U
        when(wire_cipher_stage) {
          plain_text_addr := plain_text_addr + 4.U
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
          state := s_wait
          valid_ad_reg := true.B
          block_counter := 0.U
          when(!first_load) {
            first_load := true.B
            half_load_reg := Cat(vec_blocks_load(0), vec_blocks_load(1))
          }

        }
      }
    }
    is(s_wait) {
      init_ascon := false.B
      counter3 := counter3 +& 1.U
      resp_rd_data := resp_rd_data | "h1000".U
      when(counter3 >= "hff".U) {
        resp_rd_data := resp_rd_data | "h4".U
        state := s_end
      }
      // LOAD
      when(ASCON.io.load_block) {
        state := s_mem_load_req
        need_load := false.B
        valid_ad_reg := true.B
        block_counter := 0.U
        resp_rd_data := resp_rd_data | "h100".U
      }
      when(ASCON.io.C_valid) {
        state := s_mem_write_req
        need_write := false.B
        block_counter := 0.U
        resp_rd_data := resp_rd_data | "h200".U
      }

      when(valid_tag_reg) {
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

      when(counter5 >= "hff".U) {
        resp_rd_data := resp_rd_data | "h10".U
        state := s_end
      }
      when(io.mem.resp.valid) {
        //when(block_counter < 2.U) {
        when(block_counter < block_size) {
          state := s_mem_write_req
        }.otherwise {
          when(valid_tag_reg) {
            state := s_end
          }.otherwise {
            state := s_wait
          }
          block_counter := 0.U
        }
      }
    }


    is(s_end) {
      // resp_rd_data := resp_rd_data | "h20000".U
      resp_rd_data := half_load_reg(47, 16)
      // resp_rd_data := Cat(counter1, counter2, counter4, counter5)
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