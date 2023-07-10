package ascon

import ascon.util._
import ascon.permutation._
import chisel3._
import chisel3.util._



class asconCtrl64 extends Module {


  val io = IO(new Bundle() {
    val asso_len = Input(UInt(32.W))
    val plain_len = Input(UInt(32.W))
    val c_init = Input(Bool())
    val bytes_pad = Output(UInt(4.W))
    val c_rate_mux = Output(UInt(3.W))
    val c_capacity_mux = Output(UInt(5.W))
   // val decrypt_mode = Input(Bool()) // Cipher mode
    //val decrypt_mode_out = Output(Bool()) // Cipher mode for the mux

    val hash_mode = Input(Bool())

    val c_cipher_mux = Output(UInt(2.W))
    val init_perm = Output(Bool()) //
    val type_per = Output(UInt(2.W)) //
    val c_tag = Output(Bool()) //
    val busy = Output(Bool()) //
    val ciphering = Output(Bool())
    val valid_per = Input(Bool()) // Asconp
    val busy_per = Input(Bool()) // Asconp
    val last_cipher_block = Output(Bool())
    val hash_index = Output(UInt(32.W))
    val valid_hash = Output(Bool())
    val hash_stage = Output(Bool())


  })

  val s_rst :: s_idle:: s_set :: s_init:: s_ad ::s_ad_absorb:: s_plain ::s_plain_absorb :: s_tag :: s_m_absorb :: s_m_absorb_process :: s_hash_squeeze :: s_hash_proces :: Nil = Enum(13)


  // Default values of the Outputs

  val bytes_pad_reg = RegInit(0.U(4.W)) // Sends value to the padder from complete block 8 to 1 bytes
  val c_rate_mux_reg = RegInit(0.U(3.W)) // (io.c_init, io.c_as_dt, io.c_a_last)
  val c_capacity_mux_reg = RegInit(0.U(5.W)) // (io.c_init, io.c_a_init, io.c_a_domain, io.c_fin)
  val c_tag_reg = RegInit(false.B)
  val init_perm_reg = RegInit(false.B)
  val type_per_reg = RegInit(0.U(2.W)) // Be careful with this 00 mean a number of permutations for b00 = 12 rounds b01 = 8 rounds b10 = 6 rounds
  val c_cipher_mux_reg = RegInit(0.U(2.W)) // C_CIPHER C_C_LAST
  val busy_reg = RegInit(false.B)
  val a_len_reg = RegInit(io.asso_len)
  val p_len_reg = RegInit(io.plain_len)
  val ciphering_reg = RegInit(false.B)
  //val decrypt_mode_reg = RegInit(false.B)

  val first_block_reg = RegInit(false.B)
  val last_block_reg = RegInit(false.B)

  //val has_associated_data = RegInit(io.asso_len >= 0.U)
  val has_associated_data = RegInit(false.B)
  val has_plain_data = RegInit(false.B)
  val has_inc_block = RegInit(false.B)
  val finish_permutation = RegInit(false.B)

  // Last Cipher block
  val last_cipher_block_reg = RegInit(false.B)
  val hash_len_reg = RegInit(32.U(32.W)) // 256 bits = 32 bytes
  val hash_index_counter = RegInit(0.U(32.W))
  val hash_stage = RegInit(false.B)
  val valid_hash = RegInit(false.B)

  // State Reg
  val stateReg = RegInit(s_rst)
  io.bytes_pad := bytes_pad_reg
  io.c_rate_mux := c_rate_mux_reg
  io.c_capacity_mux := c_capacity_mux_reg
  io.c_cipher_mux := c_cipher_mux_reg
  io.init_perm := init_perm_reg
  io.type_per := type_per_reg
  io.c_tag := c_tag_reg
  io.busy := busy_reg
  io.ciphering := ciphering_reg
  io.last_cipher_block := last_cipher_block_reg
  io.hash_index := hash_index_counter
  io.valid_hash := valid_hash
  io.hash_stage := hash_stage

  //io.decrypt_mode_out := decrypt_mode_reg


  switch(stateReg) {
    is(s_rst) {

      stateReg := s_idle
    }
    is(s_idle) {
      has_associated_data := false.B
      has_plain_data := false.B
      has_inc_block := false.B
      ciphering_reg := false.B
      last_block_reg := false.B
      //ciphering_reg := false.B
      when(io.c_init) {
        stateReg := s_set
        io.busy := true.B
        c_tag_reg := false.B
      }

    }
    is(s_set) {
      has_associated_data := io.asso_len > 0.U
      has_plain_data := io.plain_len > 0.U
      has_inc_block := io.plain_len < 8.U
      type_per_reg := "b00".U // 12 rounds
      c_rate_mux_reg := "b100".U // add IV
      c_capacity_mux_reg := Mux(io.hash_mode,"b10001".U,"b10000".U) // add key || nonce
      init_perm_reg := true.B
      stateReg := s_init
      a_len_reg := io.asso_len
      p_len_reg := io.plain_len
      last_cipher_block_reg := false.B
      hash_len_reg := 32.U
      hash_index_counter := 0.U
      hash_stage := false.B
      //decrypt_mode_reg := io.decrypt_mode

    }
    is(s_init) {
      when(io.busy_per) {
        init_perm_reg := false.B
      }
      when(io.valid_per && !io.busy_per) {
        // Add key
        c_capacity_mux_reg := Mux(io.hash_mode,"b10001".U,"b01000".U)
        stateReg := Mux(io.hash_mode,s_m_absorb,s_ad)
        valid_hash := false.B
        first_block_reg := true.B
        last_block_reg := false.B

      }


    }
    is(s_ad) {
      type_per_reg := "b10".U // 6 rondas
      when(has_associated_data) {
        when(a_len_reg >= 8.U && !io.busy_per) {
          a_len_reg := a_len_reg - 8.U
          init_perm_reg := true.B
          stateReg := s_ad_absorb
          c_rate_mux_reg := "b010".U // PROCES AN AD
          bytes_pad_reg := 8.U
          when(!first_block_reg) {
            c_capacity_mux_reg := "b00000".U
          }.otherwise {
            c_capacity_mux_reg := "b01000".U
            first_block_reg := false.B
          }
        }.elsewhen(a_len_reg >= 0.U && a_len_reg < 8.U && !last_block_reg) {
          when(!first_block_reg) {
            c_capacity_mux_reg := "b00000".U
          }
          init_perm_reg := true.B
          stateReg := s_ad_absorb
          bytes_pad_reg := a_len_reg
          c_rate_mux_reg := "b001".U
          last_block_reg := true.B
        }.elsewhen(last_block_reg) {
          stateReg := s_plain
          ciphering_reg := true.B
          c_capacity_mux_reg := "b00100".U // Domain
          first_block_reg := true.B
          last_block_reg := false.B
        }
      }.otherwise {
        // not associated data + // domain
        c_capacity_mux_reg := "b00010".U
        stateReg := s_plain
        ciphering_reg := true.B
        first_block_reg := true.B
      }

    }
    is(s_ad_absorb) {
      when(io.busy_per) {
        init_perm_reg := false.B
      }.elsewhen(io.valid_per && !init_perm_reg) {
        stateReg := s_ad
      }
    }
    is(s_plain) {
      type_per_reg := "b10".U
      c_rate_mux_reg := "b000".U
      c_cipher_mux_reg := "b10".U
      when(p_len_reg >= 8.U && !io.busy_per) {
        p_len_reg := p_len_reg - 8.U
        init_perm_reg := true.B
        stateReg := s_plain_absorb
        bytes_pad_reg := 8.U
        when(!first_block_reg) {
          c_capacity_mux_reg := "b00000".U
        }.otherwise {
          // A Domain
          first_block_reg := false.B
        }
      }.elsewhen(p_len_reg >= 0.U && p_len_reg < 8.U) {
        c_cipher_mux_reg := "b11".U // padding
        bytes_pad_reg := p_len_reg
        last_block_reg := true.B


        when(has_associated_data){
          c_capacity_mux_reg := "b00001".U
          when(has_inc_block){
            c_capacity_mux_reg := "b00101".U
          }
        }.otherwise{
          c_capacity_mux_reg := "b00010".U
          when(has_inc_block){
            c_capacity_mux_reg := "b00011".U
          }.otherwise{
            c_capacity_mux_reg := "b00001".U
          }
        }

//        when(has_associated_data && has_inc_block) {
//          c_capacity_mux_reg := "b00101".U // plain len < 8, domain + key||0*
//        }
        last_cipher_block_reg := true.B

        stateReg := s_tag
        first_block_reg := true.B
        init_perm_reg := true.B
        type_per_reg := "b00".U // 12 rounds
      }


    }
    is(s_plain_absorb) {
        when(io.busy_per) {
          init_perm_reg := false.B
          c_cipher_mux_reg := "b00".U
        }.elsewhen(io.valid_per && !init_perm_reg) {
          stateReg := s_plain
        }
      }


    is(s_tag) {
      c_cipher_mux_reg := "b00".U
      when(io.busy_per) {
        init_perm_reg := false.B

      }.elsewhen(io.valid_per && !init_perm_reg) {
        ciphering_reg := false.B
        c_tag_reg := true.B

        stateReg := s_idle
        busy_reg := false.B
      }

    }

    is(s_m_absorb) {
      type_per_reg := "b00".U // 12 rondas
      c_rate_mux_reg := "b000".U
      c_cipher_mux_reg := "b10".U
      c_capacity_mux_reg := "b00000".U
      ciphering_reg := true.B
      when(has_plain_data) {
        when(p_len_reg >= 8.U && !io.busy_per) {
          p_len_reg := p_len_reg - 8.U
          init_perm_reg := true.B
          stateReg := s_m_absorb_process
          bytes_pad_reg := 8.U
        }.elsewhen(p_len_reg >= 0.U && p_len_reg < 8.U && !last_block_reg) {
          init_perm_reg := true.B
          stateReg := s_m_absorb_process
          bytes_pad_reg := p_len_reg
          c_cipher_mux_reg := "b11".U
          last_block_reg := true.B
        }.elsewhen(last_block_reg) {
          stateReg := s_hash_squeeze
          hash_stage := true.B
          ciphering_reg := false.B
          first_block_reg := true.B
          last_block_reg := false.B
        }
      }.otherwise {
        // not associated data + // domain
        c_capacity_mux_reg := "b00000".U
        stateReg := s_hash_squeeze
        hash_stage := true.B
        ciphering_reg := false.B
        first_block_reg := true.B
      }

    }
    is(s_m_absorb_process) {
      when(io.busy_per) {
        init_perm_reg := false.B
      }.elsewhen(io.valid_per && !init_perm_reg) {
        stateReg := s_m_absorb
      }
    }



    is(s_hash_squeeze) {
      type_per_reg := "b00".U
      c_rate_mux_reg := "b011".U     // Just pas the rate
      c_cipher_mux_reg := "b10".U
      c_capacity_mux_reg := "b00000".U
      when(hash_len_reg >= 8.U && !io.busy_per) {
        hash_len_reg := hash_len_reg - 8.U
        init_perm_reg := true.B
        when(hash_len_reg <32.U){
        hash_index_counter := hash_index_counter + 1.U}
        stateReg := s_hash_proces
        bytes_pad_reg := 8.U
        when(first_block_reg) {
          first_block_reg := false.B
        }
      }.elsewhen(hash_len_reg < 8.U ) {
        last_block_reg := true.B
        last_cipher_block_reg := true.B
        stateReg := s_idle
        valid_hash := true.B
        hash_stage := false.B
        first_block_reg := true.B
        init_perm_reg := false.B
      }


    }
    is(s_hash_proces) {
        when(io.busy_per) {
          init_perm_reg := false.B
          c_cipher_mux_reg := "b00".U
        }.elsewhen(io.valid_per && !init_perm_reg) {
          stateReg := s_hash_squeeze
        }
      }


  }


}