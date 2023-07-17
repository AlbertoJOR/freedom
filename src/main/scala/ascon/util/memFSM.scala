package ascon.util


import chisel3._
import chisel3.util._

class memFSM(read_counter :Int, write_counter :Int) extends Module{
val io = IO(new Bundle() {
 val load_block = Input(Bool())
  val write_block = Input(Bool())
  val busy_load = Output(Bool())
  val busy_write = Output(Bool())
  val load_to_write = Output(Bool())
})
  val s_rst :: s_idle :: s_wait :: s_loading :: s_wirting :: Nil = Enum(5)
  val stateReg = RegInit(s_idle)
  io.busy_load := stateReg === s_loading
  io.busy_write := stateReg === s_wirting
  io.load_to_write := stateReg === s_loading & io.write_block
  val counter = RegInit(0.U(16.W))

  switch(stateReg){
    is(s_idle){
      stateReg := s_wait
    }
    is(s_wait) {
      when(io.load_block){
        stateReg := s_loading
        counter := 0.U
      }.elsewhen(io.write_block){
        stateReg := s_wirting
        counter := 0.U
      }
    }
    is(s_loading) {
      counter := Mux(counter<(read_counter *2).asUInt, counter + 1.U, 0.U)
      when(counter >= read_counter.asUInt){
        stateReg := s_wait
        counter := 0.U
      }
      /*when(io.write_block){
        stateReg := s_wirting
        counter := 0.U
      }*/
    }
    is(s_wirting) {
      counter := Mux(counter < (write_counter *2).asUInt, counter + 1.U, 0.U)
      when(counter >= write_counter.asUInt) {
        stateReg := s_wait
        counter := 0.U
      }
    }
  }
}
