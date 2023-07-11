package ascon

import chisel3._
import chisel3.util._
class KMU (size: Int ) extends Module{
  val io = IO(new Bundle(){
    val key_out = Output(UInt(128.W))
    val key_in = Input(UInt(128.W))
    val key_id = Input(UInt(32.W))
    val store_key = Input(Bool())
    val get_key = Input(Bool())
    val get_master = Input(Bool())
    val delete_key = Input(Bool())
    val M_key = Output(UInt(128.W))
  })
  val maxBits = log2Ceil(size)
  val Keys = RegInit(VecInit(Seq.fill(size)(7.U(32.W))))
  val Master_key = 123456789.U(128.W)

  io.key_out := Mux(io.get_key, Keys(io.key_id(maxBits-1, 0)), 0.U)
  io.M_key := Mux(io.get_master, Master_key, 0.U)
  when(io.store_key){
    Keys(io.key_id(maxBits-1, 0)) := io.key_in
  }
  when(io.delete_key) {
    Keys(io.key_id(maxBits-1, 0)) := 0.U
  }

}


