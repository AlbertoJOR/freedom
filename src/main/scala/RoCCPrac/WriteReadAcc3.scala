package RoCCPrac

import Chisel._
import ascon._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

class WriteReadAcc3(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new RWImp3(this)
}

class RWImp3(outer: WriteReadAcc3)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
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


  val s_idle :: s_mem_load_req :: s_load :: s_mem_write_req :: s_write :: s_wait :: s_comp_tag :: s_end :: Nil = Enum(8)
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
  val tag_counter = RegInit(0.U(6.W))

  // ASCON Crtl
  // val vec_blocks_write = RegInit(Vec(Seq.fill(4)(127.U(32.W))))
  val vec_blocks = RegInit(Vec(Seq.fill(8)(7.U(32.W)))) // for the HASH 256


  val module_ID = io.cmd.bits.inst.funct(6, 4)
  val operation_ID = io.cmd.bits.inst.funct(3, 0)
  val Function = io.cmd.bits.inst.funct

  val E_Set_P = Function === "h11".U
  val E_Set_A = Function === "h12".U
  val E_Set_C = Function === "h13".U
  val E_LD_N = Function === "h14".U
  val E_lD_K = Function === "h15".U
  val E_Init = Function === "h16".U
  val D_LD_T = Function === "h17".U
  val D_Init = Function === "h18".U
  val H_Set_M = Function === "h31".U
  val H_Init = Function === "h32".U
  val R_Seed = Function === "h41".U
  val R_Rand = Function === "h42".U



  //  val AEAD_Enc_ID = module_ID === 1.U
  //  val AEAD_Dec_ID = module_ID === 2.U
  //  val Hash_ID = module_ID === 3.U
  //  val Rand_ID = module_ID === 4.U
  //  val KMU_ID = module_ID === 5.U

  /// AEAD
  val plain_text_addr = RegInit(0.U(32.W))
  val plain_text_len = RegInit(0.U(32.W))
  val associated_data_addr = RegInit(0.U(32.W))
  val associated_data_len = RegInit(0.U(32.W))
  val cipher_text_addr = RegInit(0.U(32.W))
  val init_ascon = RegInit(false.B)


  // val first_load = RegInit(false.B)
  val valid_ad_reg = RegInit(false.B)
  val tag_written = RegInit(false.B)
  /// ASCON Outputs
  val wire_cipher_stage = Wire(Bool())
  val wire_busy = Wire(Bool())
  val valid_tag_reg = RegInit(false.B)

  ///////////ASCON Declaration ////////////////
  val ASCON = Module(new ascon128RoCC2(true))

  /// TAG Counter //
  val nonce_value = RegInit("h11111111222222223333333344444444".U(128.W))
  val nonce_addr = RegInit(0.U(32.W))
  val load_nonce = RegInit(false.B)
  val valid_nonce = RegInit(false.B)

  val key_value = RegInit("h55555555666666667777777788888888".U(128.W))
  val key_addr = RegInit(0.U(32.W))
  val load_key = RegInit(false.B)
  val valid_key = RegInit(false.B)

  /// Load Tag to compare //
  val tag_addr = RegInit(0.U(32.W))
  val load_tag = RegInit(false.B)
  val load_tag_valid = RegInit(false.B)
  val tag_value = RegInit("h99999999aaaaaaaabbbbbbbbcccccccc".U(128.W))

  // Decrypt mode ///
  val decrypt_mode = RegInit(false.B)
  val tags_equal = RegInit(false.B)

  // Hash Mode ///
  val hash_mode = RegInit(false.B)
  val hash_written = RegInit(false.B)
  val random = RegInit(false.B)
  val seed = RegInit(false.B)
  //val random_mode = RegInit(false.B)
  /*  val counter1 = RegInit(0.U(8.W))
    val counter2 = RegInit(0.U(8.W))
    val counter3 = RegInit(0.U(8.W))
    val counter4 = RegInit(0.U(8.W))
    val counter5 = RegInit(0.U(8.W))*/

  when(io.cmd.fire()) {
    hash_written := false.B
    resp_rd := io.cmd.bits.inst.rd
    resp_rd_data := "h0".U
    hash_mode := false.B
    block_counter := 0.U
    when(E_Set_P) {
      plain_text_addr := io.cmd.bits.rs1
      plain_text_len := io.cmd.bits.rs2
      resp_valid := true.B
      resp_rd_data := io.cmd.bits.rs1
    }.elsewhen(E_Set_A) {
      associated_data_addr := io.cmd.bits.rs1
      associated_data_len := io.cmd.bits.rs2
      resp_valid := true.B
      resp_rd_data := io.cmd.bits.rs1
    }.elsewhen(E_Set_C) {
      cipher_text_addr := io.cmd.bits.rs1
      resp_valid := true.B
      resp_rd_data := io.cmd.bits.rs1
    }.elsewhen(E_LD_N) {
      nonce_addr := io.cmd.bits.rs1
      state := s_mem_load_req
      resp_valid := false.B
      load_nonce := true.B
      valid_nonce := false.B
    }.elsewhen(E_lD_K) {
      key_addr := io.cmd.bits.rs1
      state := s_mem_load_req
      resp_valid := false.B
      load_key := true.B
      valid_key := false.B
    }.elsewhen(E_Init) {
      init_ascon := true.B
      state := s_wait
      resp_valid := false.B
      tag_written := false.B
      decrypt_mode := false.B
    }.elsewhen(D_LD_T) {
      tag_addr := io.cmd.bits.rs1
      state := s_mem_load_req
      resp_valid := false.B
      load_tag := true.B
      load_tag_valid := false.B
    }.elsewhen(D_Init) {
      init_ascon := true.B
      state := s_wait
      resp_valid := false.B
      tag_written := false.B
      decrypt_mode := true.B
      tags_equal := false.B
    }.elsewhen(H_Set_M) {
      plain_text_addr := io.cmd.bits.rs1
      plain_text_len := io.cmd.bits.rs2
      resp_valid := true.B
      resp_rd_data := io.cmd.bits.rs1
      decrypt_mode := false.B
      tag_written := false.B
      tags_equal := false.B
    }.elsewhen(H_Init) {
      decrypt_mode := false.B
      tag_written := false.B
      tags_equal := false.B
      state := s_wait
      resp_valid := false.B
      cipher_text_addr := io.cmd.bits.rs1
      hash_mode := true.B
      init_ascon := true.B
    }.elsewhen(R_Seed) {
      decrypt_mode := false.B
      tag_written := false.B
      tags_equal := false.B
      hash_mode := false.B
      //random_mode := true.B
      seed:= true.B
      state := s_wait
      resp_valid := false.B
    }.elsewhen(R_Rand) {
      decrypt_mode := false.B
      tag_written := false.B
      tags_equal := false.B
      hash_mode := false.B
      //random_mode := true.B
      random := true.B
      cipher_text_addr := io.cmd.bits.rs1
      plain_text_len := io.cmd.bits.rs2
      resp_valid := false.B
      state := s_wait
    }.otherwise {
      state := s_end
      resp_valid := true.B
      resp_rd_data := "ha1a1a1a1".U
    }
  }


  //// Inputs
  ASCON.io.m_len := plain_text_len
  ASCON.io.ad_len := associated_data_len
  ASCON.io.M := Cat(vec_blocks(0), vec_blocks(1))
  ASCON.io.AD := Cat(vec_blocks(0), vec_blocks(1))
  ASCON.io.Key(0) := key_value(127, 64)
  ASCON.io.Key(1) := key_value(63, 0)
  ASCON.io.Npub(0) := nonce_value(127, 64)
  ASCON.io.Npub(1) := nonce_value(63, 0)
  ASCON.io.init := init_ascon
  ASCON.io.decrypt := decrypt_mode
  ASCON.io.hash_mode := hash_mode
  ASCON.io.hash_written := hash_written
  ASCON.io.tag_written := tag_written
  ASCON.io.write_busy := state === s_mem_write_req || state === s_write
  ASCON.io.read_busy := state === s_mem_load_req || state === s_load
  ASCON.io.seed := seed
  ASCON.io.random := random



  wire_cipher_stage := ASCON.io.cipher_stage
  wire_busy := ASCON.io.busy

  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  // Default values of the memory interface
  io.mem.req.valid := (state === s_mem_load_req || state === s_mem_write_req)
  io.mem.req.bits.addr := Mux(wire_cipher_stage, plain_text_addr, associated_data_addr)
  io.mem.req.bits.tag := tag_counter
  //io.mem.req.bits.tag := Mux(wire_cipher_stage, plain_text_addr, associated_data_addr)(5,0)
  io.mem.req.bits.cmd := M_XRD // M_XRD = load, M_XWR = write
  io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := 0.U // Data to be stored
  io.mem.req.bits.phys := false.B // 0 since we dont need translation


  when(ASCON.io.C_valid) {
    resp_rd_data := resp_rd_data | "h20000000".U
    vec_blocks(2) := ASCON.io.C(63, 32)
    vec_blocks(3) := ASCON.io.C(31, 0)
  }
  when(ASCON.io.valid_tag && (state =/= s_idle || state =/= s_end)) {
    valid_tag_reg := true.B
    vec_blocks(0) := ASCON.io.Tag(0)(63, 32)
    vec_blocks(1) := ASCON.io.Tag(0)(31, 0)
    vec_blocks(2) := ASCON.io.Tag(1)(63, 32)
    vec_blocks(3) := ASCON.io.Tag(1)(31, 0)
  }
  when(valid_nonce) {
    nonce_value := Cat(vec_blocks(0), vec_blocks(1), vec_blocks(2), vec_blocks(3))
    resp_rd_data := Cat(vec_blocks(0)(7, 0), vec_blocks(1)(7, 0), vec_blocks(2)(7, 0), vec_blocks(3)(7, 0))
    valid_nonce := false.B
  }

  when(valid_key) {
    key_value := Cat(vec_blocks(0), vec_blocks(1), vec_blocks(2), vec_blocks(3))
    resp_rd_data := Cat(vec_blocks(0)(7, 0), vec_blocks(1)(7, 0), vec_blocks(2)(7, 0), vec_blocks(3)(7, 0))
    valid_key := false.B
  }
  when(load_tag_valid) {
    tag_value := Cat(vec_blocks(0), vec_blocks(1), vec_blocks(2), vec_blocks(3))
    resp_rd_data := 4.U
    load_tag_valid := false.B
  }

  // Tag * === Tag

  ASCON.io.valid_ad := valid_ad_reg

  val load_signals = Cat(wire_cipher_stage || hash_mode, load_nonce, load_key, load_tag)


  switch(state) {
    is(s_idle) {
      io.busy := false.B
      // first_load := false.B
      valid_ad_reg := false.B
      tag_written := false.B
      valid_tag_reg := false.B
      hash_written := false.B

    }

    is(s_mem_load_req) {
      /*counter1 := counter1 +& 1.U
      when(counter1 >= "hf0".U) {
        resp_rd_data := resp_rd_data | "h1".U
        state := s_end
      }*/
      resp_rd_data := resp_rd_data | "h2000".U
      io.mem.req.bits.cmd := M_XRD // M_XRD = load, M_XWR = write
      io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
      io.mem.req.bits.data := 0.U
      io.mem.req.bits.addr :=
        Mux(wire_cipher_stage || hash_mode, plain_text_addr,
          Mux(load_nonce, nonce_addr,
            Mux(load_key, key_addr,
              Mux(load_tag, tag_addr, associated_data_addr))))
      // Memory reques sent
      when(io.mem.req.fire()) {
        // counter1 := counter1 + 1.U
        state := s_load
        when(load_signals === "b1000".U) {
          plain_text_addr := plain_text_addr + 4.U
        }.elsewhen(load_signals === "b0100".U) {
          nonce_addr := nonce_addr + 4.U
        }.elsewhen(load_signals === "b0010".U) {
          key_addr := key_addr + 4.U
        }.elsewhen(load_signals === "b0001".U) {
          tag_addr := tag_addr + 4.U
        }.otherwise {
          associated_data_addr := associated_data_addr + 4.U
        }
      }
    }
    is(s_load) {
      /*counter2 := counter2 +& 1.U
      when(counter2 >= "hf0".U) {
        resp_rd_data := resp_rd_data | "h2".U
        state := s_end
      }*/
      resp_rd_data := resp_rd_data | "h4000".U
      when(io.mem.resp.valid) {
        tag_counter := tag_counter + 1.U
        vec_blocks(block_counter) := io.mem.resp.bits.data
        block_counter := block_counter + 1.U
        when(block_counter < Mux(load_nonce || load_tag || load_key, 3.U, 1.U)) {
          state := s_mem_load_req
        }.otherwise {
          valid_ad_reg := true.B
          block_counter := 0.U
          when(load_nonce) {
            state := s_end
            valid_nonce := true.B
            load_nonce := false.B
            resp_rd_data := resp_rd_data | "h200000".U

          }.elsewhen(load_key) {
            state := s_end
            valid_key := true.B
            load_key := false.B
            resp_rd_data := resp_rd_data | "h200000".U
          }.elsewhen(load_tag) {
            state := s_end
            load_tag := false.B
            load_tag_valid := true.B
          }.otherwise {
            state := s_wait
          }
        }
      }
    }

    is(s_wait) {
      init_ascon := false.B
      load_nonce := false.B
      seed := false.B
      random := false.B
      resp_rd_data := resp_rd_data | "h1000".U
      /*counter3 := counter3 +& 1.U
      when(counter3 >= "hf0".U) {
        resp_rd_data := resp_rd_data | "h4".U
        state := s_end
      }*/
      // LOAD
      block_counter := 0.U
      when(hash_mode && !wire_cipher_stage) {
        valid_ad_reg := false.B
      }
      when(ASCON.io.load_block) {
        state := s_mem_load_req
        valid_ad_reg := true.B
        resp_rd_data := resp_rd_data | "h100".U
      }
      when(ASCON.io.C_valid) {
        //counter3 := counter3 + 1.U
        state := s_mem_write_req
        resp_rd_data := resp_rd_data | "h200".U
      }
      when( ASCON.io.finish_rand){
        state := s_end
        // random_mode := false.B
        resp_rd_data := resp_rd_data | "h40000000".U
      }

      when(valid_tag_reg) {
        resp_rd_data := resp_rd_data | "h40000".U
        when(decrypt_mode) {
          state := s_comp_tag
          tags_equal := tag_value === Cat(vec_blocks(0), vec_blocks(1), vec_blocks(2), vec_blocks(3))
        }.otherwise {
          state := s_mem_write_req
        }
      }
      when(ASCON.io.valid_hash && !(ASCON.io.write_busy || ASCON.io.C_valid)) {
        state := s_end
        hash_mode := false.B
        hash_written := true.B
        resp_rd_data := resp_rd_data | "h80000000".U
        //resp_rd_data := Cat("hff".U,counter3,counter2,counter1)
      }
      when(hash_mode && wire_cipher_stage) {
        valid_ad_reg := false.B
      }
    }
    is(s_mem_write_req) {
      resp_rd_data := resp_rd_data | "h8000".U
      /*counter4 := counter4 +& 1.U
      when(counter4 >= "hf0".U) {
        resp_rd_data := resp_rd_data | "h8".U
        state := s_end
      }*/

      io.mem.req.bits.addr := cipher_text_addr
      io.mem.req.bits.cmd := M_XWR // M_XRD = load, M_XWR = write
      io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
      io.mem.req.bits.data := vec_blocks(Mux(valid_tag_reg, block_counter, block_counter + 2.U))
      // Memory request sent
      when(io.mem.req.fire()) {
        cipher_text_addr := cipher_text_addr + 4.U
        block_counter := block_counter + 1.U
        state := s_write
        // counter2 := counter2 + 1.U
      }
    }

    is(s_write) {
      resp_rd_data := resp_rd_data | "h10000".U
      /*counter5 := counter5 + 1.U
      when(counter5 >= "hf0".U) {
        resp_rd_data := resp_rd_data | "h10".U
        state := s_end
      }*/
      when(io.mem.resp.valid) {
        //when(block_counter < 2.U) {
        tag_counter := tag_counter + 1.U
        when(block_counter < Mux(valid_tag_reg, 4.U, 2.U)) {
          state := s_mem_write_req
        }.otherwise {
          when(valid_tag_reg) {
            state := s_end
            tag_written := true.B
            //resp_rd_data := Cat(counter1, counter2, counter4, counter5)
            valid_tag_reg := false.B
          }.otherwise {
            state := s_wait
          }
          block_counter := 0.U
        }
      }
    }
    is(s_comp_tag) {
      state := s_end
      when(tags_equal) {
        resp_rd_data := "hffffffff".U
        // resp_rd_data := Cat(tag_value(103, 96), tag_value(71,64),tag_value(39,32),tag_value(7,0))
      }.otherwise {
        //resp_rd_data := "hf0f0f0f0".U
        resp_rd_data := Cat(tag_value(103, 96), tag_value(71, 64), tag_value(39, 32), tag_value(7, 0))
      }
    }


    is(s_end) {
      // resp_rd_data := resp_rd_data | "h20000".U
      // resp_rd_data := half_load_reg(47, 16)
      hash_written := false.B
      valid_tag_reg := false.B
      resp_valid := true.B
      decrypt_mode := false.B
      tags_equal := false.B
      hash_mode := false.B
      tag_written := false.B
      // resp_rd_data := 8.U // Identify the end of the program
      block_counter := 0.U
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
  }
  io.resp.valid := state === s_end || resp_valid

}

class WithRWRoCC3 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p: Parameters) => {
      val Accel = LazyModule(new WriteReadAcc3(OpcodeSet.custom0)(p))
      Accel
    }
  )
})