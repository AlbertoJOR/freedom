package ascon.util

import chisel3._
import chisel3.util._

/** Performs the padding of 64 bit message byte
 * bye by byte
 * @param io_A message input
 * @param io_len message length in bytes
 * @param io_S padded message out
 */
class Padder extends Module {

  val io = IO(new Bundle {
    val A = Input(UInt(64.W))
    val len = Input(UInt(4.W))
    val S = Output(UInt(64.W))
  })

  val andCte = MuxLookup(io.len, "hffff_ffff_ffff_ffff".U,
    Array(0.U -> 0.U,
      1.U -> "hff00_0000_0000_0000".U,
      2.U -> "hffff_0000_0000_0000".U,
      3.U -> "hffff_ff00_0000_0000".U,
      4.U -> "hffff_ffff_0000_0000".U,
      5.U -> "hffff_ffff_ff00_0000".U,
      6.U -> "hffff_ffff_ffff_0000".U,
      7.U -> "hffff_ffff_ffff_ff00".U,
      8.U -> "hffff_ffff_ffff_ffff".U

    ))
  val xorCte = MuxLookup(io.len, "h0000_0000_0000_0000".U,
    Array(0.U -> "h8000_0000_0000_0000".U,
      1.U -> "h0080_0000_0000_0000".U,
      2.U -> "h0000_8000_0000_0000".U,
      3.U -> "h0000_0080_0000_0000".U,
      4.U -> "h0000_0000_8000_0000".U,
      5.U -> "h0000_0000_0080_0000".U,
      6.U -> "h0000_0000_0000_8000".U,
      7.U -> "h0000_0000_0000_0080".U,
      8.U -> "h0000_0000_0000_0000".U
    ))
  val and_mask = io.A & andCte
  val xor_mask = and_mask ^ xorCte
  io.S := xor_mask

}

class PadderM extends Module {

  val io = IO(new Bundle {
    val P = Input(UInt(64.W))     // Plain text
    val S_in = Input(UInt(64.W)) // State(0)

    val len = Input(UInt(4.W))
    val S_out = Output(UInt(64.W))
    val C = Output(UInt(64.W)) // CipherText
    val S_dec_out = Output(UInt(64.W))
  })

  val andCte = MuxLookup(io.len, "hffff_ffff_ffff_ffff".U,
    Array(
      0.U -> "h0000_0000_0000_0000".U,
      1.U -> "hff00_0000_0000_0000".U,
      2.U -> "hffff_0000_0000_0000".U,
      3.U -> "hffff_ff00_0000_0000".U,
      4.U -> "hffff_ffff_0000_0000".U,
      5.U -> "hffff_ffff_ff00_0000".U,
      6.U -> "hffff_ffff_ffff_0000".U,
      7.U -> "hffff_ffff_ffff_ff00".U,
      8.U -> "hffff_ffff_ffff_ffff".U

    ))


  val xorCte = MuxLookup(io.len, "h0000_0000_0000_0000".U,
    Array(0.U -> "h8000_0000_0000_0000".U,
      1.U -> "h0080_0000_0000_0000".U,
      2.U -> "h0000_8000_0000_0000".U,
      3.U -> "h0000_0080_0000_0000".U,
      4.U -> "h0000_0000_8000_0000".U,
      5.U -> "h0000_0000_0080_0000".U,
      6.U -> "h0000_0000_0000_8000".U,
      7.U -> "h0000_0000_0000_0080".U,
      8.U -> "h0000_0000_0000_0000".U
    ))
  val and_mask = (io.S_in ^ io.P) & andCte // C truncated not xor 1 yet
  io.C := and_mask
  val xor_mask = (io.P & andCte) ^io.S_in ^ xorCte // xor  State
  io.S_out := xor_mask
  val S_clear = io.S_in & ~ andCte
  val S_or = S_clear | (io.P & andCte)
  io.S_dec_out := S_or ^ xorCte


}