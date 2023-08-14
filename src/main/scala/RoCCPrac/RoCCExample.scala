package RoCCPrac
import Chisel._
import freechips.rocketchip.rocket._
import freechips.rocketchip.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._


class AccelModule(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new AccelModuleImp(this)
}

class AccelModuleImp(outer: AccelModule)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
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

  val s_idle :: s_mem_load :: s_calculate :: s_mem_write :: s_mem_ack_write :: s_end :: Nil = Enum(6)
  // Current state of the state machine
  val state = RegInit(s_idle)

  // Ready to receive new command when in idle state
  io.cmd.ready := (state === s_idle)
  // Cmd response is valid when in response state
  val resp_valid =RegInit(Bool(false))
  resp_valid := (state === s_end)
  // When finished just write 1.U to the rd destination
  val resp_rd_data = RegInit(0.U(32.W))


  val MyModule = Module(new SimpleOperation)
  MyModule.io.in_A := 0.U
  MyModule.io.in_B := 0.U

  // Decode the instructions and initiate values
  when(io.cmd.fire()) { // cmd.fire() indicates a new instruction from the processor to the rocc
    resp_rd := io.cmd.bits.inst.rd
    when(io.cmd.bits.inst.funct === 0.U) {
      // Setup of the addr
      array_addr := io.cmd.bits.rs1
      result_addr := io.cmd.bits.rs2
      resp_valid := true.B
      resp_rd_data := 1.U

      when(io.resp.fire()){
            io.busy:= false.B
            resp_valid := false.B}
    }
    when(io.cmd.bits.inst.funct === 1.U) {
      // Init the processing
      array_len := io.cmd.bits.rs1
      accum_value := io.cmd.bits.rs2
      state := s_mem_load

    }
  }
  // Response sent back to CPU
  when(io.resp.fire()) {
    state := s_idle
  }

  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := (state === s_mem_load || state === s_mem_write)
  io.mem.req.bits.addr := array_addr
  io.mem.req.bits.tag := array_addr(5, 0) // differentiate between responses
  io.mem.req.bits.cmd := M_XRD // M_XRD = load, M_XWR = write
  io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := 0.U // Data to be stored
  io.mem.req.bits.phys := false.B // 0 since we dont need translation


  when(state === s_mem_load) {
    io.mem.req.bits.addr := array_addr
    io.mem.req.bits.tag := array_addr(5, 0) // differentiate between responses
    io.mem.req.bits.cmd := M_XRD // M_XRD = load, M_XWR = write
    io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
    io.mem.req.bits.data := 0.U
    // Memory request sent
    when(io.mem.req.fire()) {
      state := s_calculate
    }
  }
  when(state === s_calculate) {
    when(io.mem.resp.valid) {
      MyModule.io.in_A := io.mem.resp.bits.data
      MyModule.io.in_B := accum_value
      accum_aux := MyModule.io.out_C

      array_addr := array_addr + 4.U
      processed_words := processed_words + 1.U
      state := s_mem_write
    }
  }
  when(state === s_mem_write) {

    io.mem.req.bits.addr := result_addr
    io.mem.req.bits.tag := result_addr(5, 0) // differentiate between responses
    io.mem.req.bits.cmd := M_XWR // M_XRD = load, M_XWR = write
    io.mem.req.bits.typ := MT_W // D = 8 bytes, W = 4, H = 2, B = 1
    io.mem.req.bits.data := accum_aux
    accum_value := accum_aux
    // Memory request sent
    when(io.mem.req.fire()) {
      result_addr := result_addr + 4.U
      state := s_mem_ack_write
    }
  }
  when(state === s_mem_ack_write) {
    when(io.mem.resp.valid) {
      when(processed_words >= array_len) {
        state := s_end
      }.otherwise {
        state := s_mem_load
      }
    }
  }
  when(state ===s_end){
   resp_valid := true.B
    resp_rd_data := 2.U
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

class WithMyRoCC extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p: Parameters) => {
      val Accel = LazyModule(new AccelModule(OpcodeSet.custom0)(p))
      Accel
    }
  )
})