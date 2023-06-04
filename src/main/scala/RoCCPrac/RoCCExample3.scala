package RoCCPrac

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._


class AccelModule3(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new AccelModuleImp3(this)
}

class AccelModuleImp3(outer: AccelModule3)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
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

  val resp_rd = RegInit(0.U(5.W)) //Reg(io.resp.bits.rd) //Register for the reply

  val message_addr = RegInit(0.U(32.W))
  val result_addr = RegInit(0.U(32.W))
  val message_len = RegInit(0.U(32.W))


  val accum_aux = RegInit(0.U(32.W))

  // alwayd add the exact amount of states in enum or it will ve a java error
  val s_idle :: s_wait :: s_load_blocks :: s_load_req :: s_end :: Nil = Enum(5)
  // Current state of the state machine
  val state = RegInit(s_idle)

  // Ready to receive new command when in idle state
  io.cmd.ready := (state === s_idle)
  // Cmd response is valid when in response state
  val resp_back_core_valid = RegInit(Bool(false))
  resp_back_core_valid := (state === s_end)
  // When finished just write 1.U to the rd destination
  val resp_rd_data = RegInit(0.U(32.W))

  //  // Memory request valid States
  //  val memory_request_states = Seq(s_load_blocks, s_write_blocks)
  //  // Map states to valid bools
  //  val memory_request_states_valid = memory_request_states.map(_ === state)
  //  // Reduce the valid states to one valid
  //  val valid_memory_request = memory_request_states_valid.reduce(_ || _ )
  val valid_memory_request = RegInit(Bool(false))


  // val MyModule2 = Module(new SimpleOperation2)
  // MyModule2.io.in_A := "h1112_1314_1516_1718".U
  // val vec_blocks = RegInit(Vec(Seq.fill(4)(0.U(32.W))))
  val block_counter = RegInit(0.U(32.W))
  val processed_blocks = RegInit(0.U(32.W))

  // Decode the instructions and initiate values
  when(io.cmd.fire()) { // cmd.fire() indicates a new instruction from the processor to the rocc
    resp_rd := io.cmd.bits.inst.rd
    io.cmd.bits.inst.funct // with function7 we have 2^7 different instructions to implement
    when(io.cmd.bits.inst.funct === 0.U) {
      // Setup of the addr
      message_addr := io.cmd.bits.rs1
      result_addr := io.cmd.bits.rs2

      resp_back_core_valid := true.B
      resp_rd_data := 1.U

      when(io.resp.fire()) {
        io.busy := false.B
        resp_back_core_valid := false.B
      }
    }
    when(io.cmd.bits.inst.funct === 1.U) {
      // Init the processing
      message_len := io.cmd.bits.rs1
      state := s_load_blocks
      block_counter := 0.U
      processed_blocks := 0.U

    }
  }
  // Response sent back to CPU End of the states
  when(io.resp.fire()) {
    state := s_idle
  }
  // Default values
  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := state === s_load_blocks
  io.mem.req.bits.addr := message_addr
  io.mem.req.bits.tag := message_addr // differentiate between responses
  io.mem.req.bits.cmd := M_XRD // M_XRD = load, M_XWR = write
  io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := 0.U // Data to be stored
  io.mem.req.bits.phys := false.B // 0 since we dont need translation

  // MyModule2.io.in_A := Cat(vec_blocks(0), vec_blocks(1))

  when(state === s_wait) {
    when(processed_blocks >= 8.U(32.W)) {
      state := s_end
    }.otherwise {
      processed_blocks := processed_blocks + 1.U
      // vec_blocks(0) := MyModule2.io.out_C(31, 0)
      // vec_blocks(1) := MyModule2.io.out_C(63, 32)
      state := s_load_blocks
    }
  }

  when(state === s_load_blocks) {
    io.mem.req.bits.cmd := M_XRD // M_XRD = load
    io.mem.req.bits.typ := MT_W
    io.mem.req.bits.data := 0.U
    io.mem.req.bits.addr := message_addr
    io.mem.req.bits.tag := message_addr

    when(io.mem.req.fire()) {
     // when(block_counter >= 2.U(32.W)) {
//        state := s_wait
//        block_counter := 0.U
//      }.otherwise {
        s_load_req
     // }
    }
  }
  when(state === s_load_req) {
    when(io.mem.resp.valid) {
      message_addr := message_addr + 4.U
      // vec_blocks(block_counter) := io.mem.resp.bits.data
      block_counter := block_counter + 1.U
      state := s_load_blocks
    }
  }

  when(state === s_end) {
    resp_back_core_valid := true.B
    resp_rd_data := block_counter// finish signal
  }

  // Response sent back to CPU
  io.resp.valid := resp_back_core_valid
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := resp_rd_data
  when(io.resp.fire()) {
    state := s_idle
    io.busy := false.B
  }


}




//
//class AccelModuleImp3(outer: AccelModule3)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
//  with HasCoreParameters {
//  // The parts of the command are as follows
//  // inst  - the parts of the instruction itself
//  //   opcode
//  //   rd  - destination register number
//  //   rs1 - first source register number
//  //   rs2 - second source register number
//  //   funct
//  //   xd  - is the destination register being used?
//  //   xs1 - is the first source register being used?
//  //   xs2 - is the second source register being used?
//  //   rs1 - the value of source register 1
//  //   rs2 - the value of source register 2
//
//  val resp_rd = RegInit(0.U(5.W)) //Reg(io.resp.bits.rd) //Register for the reply
//
//  val message_addr = RegInit(0.U(32.W))
//  val result_addr = RegInit(0.U(32.W))
//  val message_len = RegInit(0.U(32.W))
//
//
//  val accum_aux = RegInit(0.U(32.W))
//
//  // alwayd add the exact amount of states in enum or it will ve a java error
//  val s_idle :: s_wait :: s_load_blocks :: s_load_req :: s_end :: Nil = Enum(5)
//  // Current state of the state machine
//  val state = RegInit(s_idle)
//
//  // Ready to receive new command when in idle state
//  io.cmd.ready := (state === s_idle)
//  // Cmd response is valid when in response state
//  val resp_back_core_valid = RegInit(Bool(false))
//  resp_back_core_valid := (state === s_end)
//  // When finished just write 1.U to the rd destination
//  val resp_rd_data = RegInit(0.U(32.W))
//
//  //  // Memory request valid States
//  //  val memory_request_states = Seq(s_load_blocks, s_write_blocks)
//  //  // Map states to valid bools
//  //  val memory_request_states_valid = memory_request_states.map(_ === state)
//  //  // Reduce the valid states to one valid
//  //  val valid_memory_request = memory_request_states_valid.reduce(_ || _ )
//  val valid_memory_request = RegInit(Bool(false))
//
//
//  val MyModule2 = Module(new SimpleOperation2)
//  MyModule2.io.in_A := "h1112_1314_1516_1718".U
//  val vec_blocks = RegInit(Vec(Seq.fill(4)(0.U(32.W))))
//  val block_counter = RegInit(0.U(32.W))
//  val processed_blocks = RegInit(0.U(32.W))
//
//  // Decode the instructions and initiate values
//  when(io.cmd.fire()) { // cmd.fire() indicates a new instruction from the processor to the rocc
//    resp_rd := io.cmd.bits.inst.rd
//    io.cmd.bits.inst.funct // with function7 we have 2^7 different instructions to implement
//    when(io.cmd.bits.inst.funct === 0.U) {
//      // Setup of the addr
//      message_addr := io.cmd.bits.rs1
//      result_addr := io.cmd.bits.rs2
//
//      resp_back_core_valid := true.B
//      resp_rd_data := 1.U
//
//      when(io.resp.fire()) {
//        io.busy := false.B
//        resp_back_core_valid := false.B
//      }
//    }
//    when(io.cmd.bits.inst.funct === 1.U) {
//      // Init the processing
//      message_len := io.cmd.bits.rs1
//      state := s_load_blocks
//      block_counter := 0.U
//      processed_blocks := 0.U
//
//    }
//  }
//  // Response sent back to CPU End of the states
//  when(io.resp.fire()) {
//    state := s_idle
//  }
//  // Default values
//  io.busy := (state =/= s_idle)
//  io.interrupt := false.B
//  io.mem.req.valid := state === s_load_blocks
//  io.mem.req.bits.addr := message_addr
//  io.mem.req.bits.tag := message_addr // differentiate between responses
//  io.mem.req.bits.cmd := M_XRD // M_XRD = load, M_XWR = write
//  io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
//  io.mem.req.bits.data := 0.U // Data to be stored
//  io.mem.req.bits.phys := false.B // 0 since we dont need translation
//
//  MyModule2.io.in_A := Cat(vec_blocks(0), vec_blocks(1))
//
//  when(state === s_wait) {
//    processed_blocks := processed_blocks + 1.U
//    vec_blocks(0) := MyModule2.io.out_C(31, 0)
//    vec_blocks(1) := MyModule2.io.out_C(63, 32)
//    state := s_load_blocks
//  }
//
//  when(state === s_load_blocks) {
//    io.mem.req.bits.cmd := M_XRD // M_XRD = load
//    io.mem.req.bits.typ := MT_W
//    io.mem.req.bits.data := 0.U
//    io.mem.req.bits.addr := message_addr
//    io.mem.req.bits.tag := message_addr
//
//    when(io.mem.req.fire()) {
//      when(block_counter >= 2.U) {
//        state := s_wait
//        block_counter := 0.U
//      }.otherwise {
//        s_load_req
//      }
//    }
//  }
//  when(state === s_load_req) {
//    when(processed_blocks >= message_len) {
//      state := s_end
//    }.elsewhen(io.mem.resp.valid) {
//      message_addr := message_addr + 4.U
//      vec_blocks(block_counter) := io.mem.resp.bits.data
//      block_counter := block_counter + 1.U
//      state := s_load_blocks
//    }
//  }
//
//  when(state === s_end) {
//    resp_back_core_valid := true.B
//    resp_rd_data := 7.U // finish signal
//  }
//
//  // Response sent back to CPU
//  io.resp.valid := resp_back_core_valid
//  io.resp.bits.rd := resp_rd
//  io.resp.bits.data := resp_rd_data
//  when(io.resp.fire()) {
//    state := s_idle
//    io.busy := false.B
//  }
//
//
//}


class WithMyRoCC3 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p: Parameters) => {
      val Accel = LazyModule(new AccelModule3(OpcodeSet.custom0)(p))
      Accel
    }
  )
})