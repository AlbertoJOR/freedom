package rand
import chisel3._
import chisel3.util._
class randAsconState extends Module{
  val io = IO(new Bundle() {
    val seed_value = Input(UInt(64.W))
    val seed = Input(Bool())
    val seed_valid = Input(Bool())
    val valid_state = Input(Bool())
    val new_state = Input(Vec(5, UInt(64.W)))
    val state = Output(Vec(5, UInt(64.W)))
  })
  val State_Reg = RegInit(VecInit(Seq.fill(5)(0.U(64.W))))
  io.state := State_Reg
  val seed_reg = RegInit(0.U(64.W))
  when(io.seed_valid){
    seed_reg := io.seed_value
  }
  when(io.valid_state){
    State_Reg := io.new_state
  }
  when(io.seed){
    io.state(0) := State_Reg(0) ^ seed_reg
    io.state(1) := State_Reg(1)
    io.state(2) := State_Reg(2)
    io.state(3) := State_Reg(3)
    io.state(4) := State_Reg(4)
  }

}
