package RoCCPrac

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

class AccelModule2(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new AccelModuleImp2(this)
}

class AccelModuleImp2(outer: AccelModule2)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
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

  val array_addr = RegInit(0.U(32.W))
  val array_len = RegInit(0.U(32.W))
  val result_addr = RegInit(0.U(32.W))
  val processed_words = RegInit(0.U(32.W))
  val accum_value = RegInit(0.U(32.W))
  val accum_aux = RegInit(0.U(32.W))

  val s_idle :: s_mem_load_req :: s_load :: s_mem_write_req :: s_write :: s_mem_write_ack :: s_end :: s_ack_load :: Nil = Enum(8)
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

  val vec_blocks = RegInit(Vec(Seq.fill(8)(7.U(32.W))))


  val MyModule = Module(new SimpleOperation2)
  MyModule.io.in_A := Cat(vec_blocks(0), vec_blocks(1))
  vec_blocks(2) := MyModule.io.out_C(63,32)
  vec_blocks(3) := MyModule.io.out_C(31,0)

  // Decode the instructions and initiate values
  when(io.cmd.fire()) { // cmd.fire() indicates a new instruction from the processor to the rocc
    resp_rd := io.cmd.bits.inst.rd
    // io.cmd.bits.inst.funct // with function7 we have 2^7 different instructions to implement
    when(io.cmd.bits.inst.funct === 0.U) {
      // Setup of the addr
      array_addr := io.cmd.bits.rs1
      result_addr := io.cmd.bits.rs2
      resp_valid := true.B
      resp_rd_data := 1.U

      when(io.resp.fire()) {
        io.busy := false.B
        resp_valid := false.B
      }
    }
    when(io.cmd.bits.inst.funct === 1.U) {
      // Init the processing
      array_len := io.cmd.bits.rs1
      accum_value := io.cmd.bits.rs2
      state := s_mem_load_req
      block_counter := 0.U

    }
  }
  // Response sent back to CPU
  when(io.resp.fire()) {
    state := s_idle
  }


  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := (state === s_mem_load_req || state === s_mem_write_req)
  io.mem.req.bits.addr := array_addr
  io.mem.req.bits.tag := array_addr(5, 0) // differentiate between responses
  io.mem.req.bits.cmd := M_XRD // M_XRD = load, M_XWR = write
  io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := 0.U // Data to be stored
  io.mem.req.bits.phys := false.B // 0 since we dont need translation

  switch(state) {

    is(s_mem_load_req) {
      io.mem.req.bits.addr := array_addr
      io.mem.req.bits.tag := array_addr(5, 0) // differentiate between responses
      io.mem.req.bits.cmd := M_XRD // M_XRD = load, M_XWR = write
      io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
      io.mem.req.bits.data := 0.U
      // Memory request sent
      when(io.mem.req.fire()) {
        state := s_load
        array_addr := array_addr + 4.U
        processed_words := processed_words + 1.U
      }
    }
    is(s_load) {
      when(io.mem.resp.valid) {
        when(processed_words > array_len) {
          state := s_end
        }.otherwise {
          vec_blocks(block_counter) := io.mem.resp.bits.data
          block_counter := block_counter + 1.U
          when(block_counter < 1.U) {
            state := s_mem_load_req
          }.otherwise {
            state := s_mem_write_req
            block_counter := 0.U
          }
        }
      }
    }
    //    is(s_load) {
    //      when(io.mem.resp.valid) {
    //        array_addr := array_addr + 4.U
    //        processed_words := processed_words + 1.U
    //        block_counter := block_counter + 1.U
    //        vec_blocks(block_counter) := io.mem.resp.bits.data
    //
    //        state := s_ack_load
    //      }
    //    }

    is(s_ack_load) {
      when(block_counter < 2.U) {
        state := s_mem_load_req
      }.otherwise {
        state := s_mem_write_req
        block_counter := 0.U
      }

    }
    is(s_mem_write_req) {

      io.mem.req.bits.addr := result_addr
      io.mem.req.bits.tag := result_addr(5, 0) // differentiate between responses
      io.mem.req.bits.cmd := M_XWR // M_XRD = load, M_XWR = write
      io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
      io.mem.req.bits.data := vec_blocks(2.U + block_counter)
      accum_value := accum_aux
      // Memory request sent
      when(io.mem.req.fire()) {
        result_addr := result_addr + 4.U
        block_counter := block_counter + 1.U
        state := s_write
      }
    }
    is(s_write) {
      when(io.mem.resp.valid) {
        //        when(processed_words >= array_len) {
        //          state := s_end
        //        }.otherwise {
        when(block_counter < 2.U) {
          state := s_mem_write_req
        }.otherwise {
          state := s_mem_load_req
          block_counter := 0.U
        }

      }
    }


    is(s_end) {
      resp_valid := true.B
      resp_rd_data := 2.U
      block_counter := 0.U
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

class WithMyRoCC2 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p: Parameters) => {
      val Accel = LazyModule(new AccelModule2(OpcodeSet.custom0)(p))
      Accel
    }
  )
})