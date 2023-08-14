package ascon.util


import chisel3._
import chisel3.util._

class memFSM(read_counter: Int, write_counter: Int) extends Module {
  val io = IO(new Bundle() {
    val load_block = Input(Bool())
    val write_block = Input(Bool())
    val c_valid = Input(Bool())
    val ciphering = Input(Bool())
    val init = Input(Bool())
    val busy_load = Output(Bool())
    val busy_write = Output(Bool())
    val valid = Input(Bool())
    val valid_ad = Output(Bool())

    val Data_A_in = Input(Vec(4, UInt(64.W)))
    val Data_P_in = Input(Vec(4, UInt(64.W)))
    val Data_out = Output(Vec(4, UInt(64.W)))

    val load_data = Output(UInt(64.W))
    val tag_written = Output(Bool())
    val hash_written = Output(Bool())
    val hash_valid = Input(Bool())
    val hash_mode = Input(Bool())
    val write_data = Input(UInt(64.W))
    val finish_rand = Input(Bool())
  })
  val s_rst :: s_idle :: s_wait :: s_loading :: s_wirting :: Nil = Enum(5)
  val stateReg = RegInit(s_idle)
  io.busy_load := stateReg === s_loading
  io.busy_write := stateReg === s_wirting
  val counter = RegInit(0.U(16.W))
  val valid_ad_reg = RegInit(false.B)
  val Data_File = RegInit(VecInit(Seq.fill(16)(0.U(64.W))))
  val plain_addr = RegInit(0.U(4.W))
  val as_addr = RegInit(0.U(4.W))
  val ciph_addr = RegInit(0.U(4.W))
  val write_data = RegInit(0.U(64.W))
  val load_data = RegInit(0.U(64.W))
  val tag_written = RegInit(false.B)
  val hash_written = RegInit(false.B)

  io.tag_written := tag_written
  io.hash_written := hash_written
  io.load_data := load_data
  io.valid_ad := valid_ad_reg
  when(io.c_valid){
    write_data := io.write_data
  }
for (k <- 8 until 12) {
        io.Data_out(k-8) := Data_File(k)
      }
  switch(stateReg) {
    is(s_idle) {
      tag_written := false.B
      hash_written := false.B
      valid_ad_reg := false.B
      when(io.init) {
        as_addr := 0.U
        plain_addr := 4.U
        ciph_addr := 8.U
        valid_ad_reg := false.B
        stateReg := s_wait
      }

      for (i <- 0 until 4) {
        Data_File(i) := io.Data_A_in(i)
      }
      for (j <- 4 until 8) {
        Data_File(j) := io.Data_P_in(j - 4)
      }
    }
    is(s_wait) {
      when(io.init) {
        as_addr := 0.U
        plain_addr := 4.U
        ciph_addr := 8.U
        valid_ad_reg := false.B
      }
      when(io.load_block) {
        stateReg := s_loading
        valid_ad_reg := false.B
        counter := 0.U
      }.elsewhen(io.write_block) {
        stateReg := s_wirting
        counter := 0.U
      }.elsewhen(io.valid){
        stateReg := s_idle
        tag_written := true.B
      }.elsewhen(io.hash_valid){
        stateReg := s_idle
        hash_written := true.B
      }.elsewhen(io.finish_rand){
        stateReg := s_idle
      }
      when(io.hash_mode && io.ciphering){
        valid_ad_reg := false.B
      }
    }
    is(s_loading) {
      counter := Mux(counter < (read_counter * 2).asUInt, counter + 1.U, 0.U)
      load_data := Data_File(Mux(io.ciphering,plain_addr,as_addr))
      when(counter >= read_counter.asUInt) {
        when(io.ciphering){
          plain_addr := plain_addr + 1.U
        }.otherwise{
          as_addr := as_addr + 1.U
        }
        stateReg := s_wait
        valid_ad_reg := true.B
        counter := 0.U
      }
    }
    is(s_wirting) {
      counter := Mux(counter < (write_counter * 2).asUInt, counter + 1.U, 0.U)
      Data_File(ciph_addr) := write_data
      when(counter >= write_counter.asUInt) {
        stateReg := s_wait
        ciph_addr := ciph_addr + 1.U
        counter := 0.U
      }
    }
  }
}
