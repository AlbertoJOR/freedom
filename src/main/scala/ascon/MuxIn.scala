package ascon

import chisel3._
import chisel3.util._
import ascon.util._

class MuxInRate extends Module {
  val io = IO(new Bundle() {
    val as_data = Input(UInt(64.W)) // associated data in
    val iv = Input(UInt(64.W)) // Initialization vector
    val rate_in = Input(UInt(64.W)) //S(0)

    val c_init = Input(Bool())
    val c_as_dt = Input(Bool())
    val c_a_last = Input(Bool())

    val a_bytes = Input(UInt(4.W))
    val S0_xor = Output(UInt(64.W))
  })
  val padAssociated = Module(new Padder)

  // route padding for Associated Data and Plain text
  padAssociated.io.A := io.as_data
  padAssociated.io.len := io.a_bytes

  val sel = Cat(io.c_init, io.c_as_dt, io.c_a_last)
  io.S0_xor := MuxLookup(sel, 0.U,
    Array("b100".U -> (io.iv),
      "b010".U -> (io.as_data ^ io.rate_in),
      "b001".U -> (padAssociated.io.S ^ io.rate_in),
      "b011".U -> (io.rate_in),
      "b000".U -> (0.U)

    ))

}

class MuxInCapacity extends Module {
  val io = IO(new Bundle() {
    val key = Input(UInt(128.W))
    val nonce = Input(UInt(128.W))
    val capacity_in = Input(UInt(256.W))
    val capacity_out = Output(UInt(256.W))

    val c_init = Input(Bool())
    val c_a_init = Input(Bool())
    val c_a_domain = Input(Bool())
    val c_no_a = Input(Bool())
    val c_fin = Input(Bool())
  })
  val sel = Cat(io.c_init, io.c_a_init, io.c_a_domain, io.c_no_a, io.c_fin)
  val zeros_128 = 0.U(128.W) // if "h0000.....0000".U for chisel = 0.U(1.W)
  val one_256 = 1.U(256.W)
  val one_128 = 1.U(128.W)
  io.capacity_out := MuxLookup(sel, io.capacity_in,
    Array(
      "b10000".U -> Cat(io.key, io.nonce),
      "b10001".U -> 0.U(256.W), // For  initialization of Hash
      "b01000".U -> (Cat(zeros_128, io.key) ^ io.capacity_in),
      "b00100".U -> (io.capacity_in ^ one_256),
      "b00010".U -> (Cat(zeros_128, io.key) ^ io.capacity_in ^ one_256),
      "b00001".U -> (Cat(io.key, zeros_128) ^ io.capacity_in),
      "b00101".U -> (Cat(io.key, one_128) ^ io.capacity_in),
      "b00011".U -> (Cat(io.key, io.key) ^ io.capacity_in ^ one_256)

    )
  )
}

class Separator extends Module {
  val io = IO(new Bundle() {
    val rate = Input(UInt(64.W))
    val capacity = Input(UInt(256.W))
    val S = Output(Vec(5, UInt(64.W)))
  })
  io.S(0) := io.rate
  io.S(1) := io.capacity(255, 192)
  io.S(2) := io.capacity(191, 128)
  io.S(3) := io.capacity(127, 64)
  io.S(4) := io.capacity(63, 0)


}

class CipheredMux extends Module {
  val io = IO(new Bundle() {
    val plain = Input(UInt(64.W))
    val rate = Input(UInt(64.W))
    val c_cipher = Input(Bool())
    val c_c_last = Input(Bool())
    val p_bytes = Input(UInt(4.W))
    val decrypt_mode = Input(Bool())
    val hash_mode = Input(Bool())
    val hash_stage = Input(Bool())
    val valid_per = Input(Bool())
    val valid = Output(Bool())
    val cipher_text = Output(UInt(64.W))
    val S_out = Output(UInt(64.W))
  })
  io.valid := false.B
  //io.cipher_text:= 0.U
  //io.S_out := 0.U
  val padder = Module(new PadderM)
  padder.io.P := io.plain
  padder.io.len := io.p_bytes
  padder.io.S_in := io.rate
  val out_state_plain = Wire(UInt(64.W))
  val out_state_decrypt = Wire(UInt(64.W))
  val out_cipher_text = Wire(UInt(64.W))
  val out_hash_stage = Wire(UInt(64.W))

  val out_s_0_normal_mode = Wire(UInt(64.W))

  out_cipher_text := Mux(io.c_c_last, padder.io.C, io.plain ^ io.rate) // Cipher dont add ^1
  io.cipher_text := Mux(io.hash_mode, io.rate, out_cipher_text )


  out_state_decrypt := Mux(io.c_c_last, padder.io.S_dec_out, io.plain)
  out_state_plain := Mux(io.c_c_last, padder.io.S_out, io.plain ^ io.rate)
  out_hash_stage := Mux(io.hash_stage, io.rate, out_state_plain)

  out_s_0_normal_mode := Mux(io.decrypt_mode, out_state_decrypt, out_state_plain)
  io.S_out := Mux(io.hash_mode,out_hash_stage, out_s_0_normal_mode)

  when(io.c_cipher && io.p_bytes > 0.U) { //"b10".U
    io.valid := !io.c_c_last && io.valid_per
    when(io.c_c_last) { // "b11".U
      io.valid := true.B
    }
  }.otherwise {
    io.valid := false.B
  }
}

class RateCapacityTag extends Module {
  val io = IO(new Bundle() {
    val S_i = Input(Vec(5, UInt(64.W)))
    val key = Input(UInt(128.W))
    val c_tag = Input(Bool())
    val tag = Output(UInt(128.W))
    val valid = Output(Bool())
    val rate_o = Output(UInt(64.W))
    val capacity_o = Output(UInt(256.W))
  })
  io.rate_o := io.S_i(0)
  io.capacity_o := Cat(io.S_i(1), io.S_i(2), io.S_i(3), io.S_i(4))
  io.tag := 0.U
  io.valid := false.B

  when(io.c_tag) {
    io.valid := true.B
    io.tag := Cat(io.S_i(3), io.S_i(4)) ^ io.key
  }.otherwise {
    io.valid := false.B
    io.tag := 0.U
  }
}





