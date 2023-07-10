package ascon
import chisel3._
import chisel3.util._
class KMU extends Module{
  val io = IO(new Bundle(){
    val key_out = Output(UInt(128.W))
    val key_in = Input(UInt(128.W))
    val key_id = Input(UInt(32.W))
    val store_key = Input(Bool())
    val get_key = Input(Bool())
    val M_key = Output(UInt(128.W))
  })
  val Keys = RegInit(Vec(Seq.fill(4)(7.U(128.W))))
  io.key_out := Mux(io.get_key, Keys(io.key_id), 0.U)
  when(io.store_key){
    Keys(io.key_id) := io.key_in
  }
  val Master_key = 123456789.U(128.W)
  io.M_key := Master_key

}
