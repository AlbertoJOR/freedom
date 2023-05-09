package MatrixSum

import chisel3._
import freechips.rocketchip.config.{Field, Parameters}

class SCacheModule(val W: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val init = Input(Bool())
    val index = Input(UInt(5.W))
    val req_val = Input(Bool())
    val req_data = Input(UInt(W.W))
    val resp_rdy = Input(Bool())
    val resp_data = Output(UInt(W.W))
    val row_col = Input(Bool())
  })
  val row_particular_sum = SyncReadMem(UInt(W.W), 31)
  val col_particular_sum = SyncReadMem(UInt(W.W), 31)

  when(io.row_col) {
    when(io.req_val) {
      io.resp_data := col_particular_sum.read(io.index)
    }
    when(io.resp_rdy) {
      row_particular_sum.write(io.index, io.resp_data + row_particular_sum(io.index))
    }
  }.otherwise {
    when(io.req_val) {
      io.resp_data := col_particular_sum.read(io.index)
    }
    when(io.resp_rdy) {
      row_particular_sum.write(io.index, io.resp_data + row_particular_sum(io.index))
    }
  }
  when(io.init){
    for(i <-0 until W-1){
      row_particular_sum.write(i.U, 0.U)
      col_particular_sum.write(i.U, 0.U)
      
    }
  }
}
