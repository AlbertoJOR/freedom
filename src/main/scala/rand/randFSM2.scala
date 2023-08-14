package rand

import chisel3._
import chisel3.util._

class randFSM2 extends Module {
  val io = IO(new Bundle() {
    val seed = Input(Bool())
    val rand = Input(Bool())
    val busy_per = Input(Bool())
    val valid_per = Input(Bool())
    val num_rand = Input(UInt(32.W))
    val busy_write = Input(Bool())
    val valid_seed = Input(Bool())
    val valid_rand = Output(Bool())
    val seed_mux = Output(Bool())
    val seed_valid = Output(Bool())
    val rand_mux = Output(Bool())
    val init_per = Output(Bool())
    val busy = Output(Bool())
    val type_per = Output(UInt(2.W))
    val finish = Output(Bool())
    // al new_init = Input(Bool())

  })

  val s_idle :: s_init :: s_init_ack :: s_absorb_seed :: s_rand_per :: s_absorb_ack :: s_rand_write :: s_end :: Nil = Enum(8)
  val stateReg = RegInit(s_idle)
  val finishReg = RegInit(false.B)
  val valid_reg = RegInit(false.B)
  val absorb_seed = RegInit(false.B) // first seed
  val rand_counter = RegInit(0.U(32.W))
  val num_rand = RegInit(0.U(32.W))
  val init_perm_reg = RegInit(false.B)
  val wait_to_init = RegInit(false.B)
  val rand = RegInit(false.B)
  val seed = RegInit(false.B)
  io.seed_valid := false.B

  when(io.rand) {
    rand := true.B
    num_rand := io.num_rand
    //finishReg := false.B
  }
  when(io.seed) {
    seed := true.B
    // finishReg := false.B
  }
  io.finish := finishReg
  io.busy := stateReg =/= s_idle
  io.valid_rand := valid_reg && !io.busy_write
  io.rand_mux := stateReg === s_rand_per || stateReg === s_rand_write
  io.seed_mux := stateReg === s_absorb_seed || stateReg === s_absorb_ack || stateReg === s_init || stateReg === s_init_ack
  io.init_per := init_perm_reg
  io.type_per := "b00".U // 12 rounds
  io.rand_mux := rand
  io.seed_mux := seed

  switch(stateReg) {
    is(s_idle) {
      finishReg := false.B
      valid_reg := false.B
      when(seed) {
        when(!absorb_seed) {
          stateReg := s_init
        }.otherwise {
          stateReg := s_absorb_seed
        }
      }.elsewhen(rand) {
        when(!absorb_seed) {
          stateReg := s_init
        }.otherwise {
          stateReg := s_rand_write
          rand_counter := rand_counter + 1.U
          stateReg := s_rand_write
          wait_to_init := true.B
        }
      }
    }
    is(s_init) {
      when(io.valid_seed) {
        init_perm_reg := true.B
        stateReg := s_init_ack
      }
    }
    is(s_init_ack) {
      when(io.busy_per) {
        init_perm_reg := false.B
      }.elsewhen(io.valid_per) {
        absorb_seed := true.B
        stateReg := Mux(rand, s_rand_per, s_absorb_seed)
        when(rand) {
          stateReg := s_rand_write
          rand_counter := rand_counter + 1.U
          stateReg := s_rand_write
          wait_to_init := true.B
        }.otherwise {
          stateReg := s_absorb_seed
        }
      }

    }
    is(s_absorb_seed) {
      when(io.valid_seed) {
        stateReg := s_absorb_ack
        io.seed_valid := true.B
        init_perm_reg := true.B
      }
    }
    is(s_absorb_ack) {
      when(io.busy_per) {
        init_perm_reg := false.B
      }.elsewhen(io.valid_per && !init_perm_reg) {
        stateReg := s_end
        seed := false.B
      }

    }
    is(s_rand_per) {
      when(!io.busy_write) {
        valid_reg := false.B
        when(rand_counter < num_rand) {
          rand_counter := rand_counter + 1.U
          stateReg := s_rand_write
          wait_to_init := true.B
        }.otherwise {
          rand_counter := 0.U
          num_rand := 0.U
          init_perm_reg := false.B
          wait_to_init := false.B
          rand := false.B
          stateReg := s_end
        }
      }
    }
    is(s_rand_write) {
      when(wait_to_init) {
        init_perm_reg := true.B
        wait_to_init := false.B
      }.elsewhen(io.busy_per) {
        init_perm_reg := false.B
      }.elsewhen(io.valid_per && !init_perm_reg) {
        valid_reg := true.B
        stateReg := s_rand_per
      }
    }
    is(s_end) {
      when(!io.busy_write) {
        finishReg := true.B
        io.finish := true.B
        stateReg := s_idle
      }
    }
  }
}
