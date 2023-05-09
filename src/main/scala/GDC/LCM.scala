package GDC

import Chisel._
import freechips.rocketchip.tile._ // 导入LazyRoCC
import freechips.rocketchip.config._ // 导入Config object
import freechips.rocketchip.diplomacy._ // 导入LazyModule


class LCM(val w: Int) extends Module {
  val io = IO(new Bundle {
    val in1 = Flipped(Valid(UInt(w.W)))
    val in2 = Flipped(Valid(UInt(w.W)))
    val out = Decoupled(UInt(w.W))
  })

  val x = Reg(UInt(w.W))
  val y = Reg(UInt(w.W))
  val a = Reg(UInt(w.W))
  val b = Reg(UInt(w.W))

  val s_idle :: s_dataIn :: s_gcdComp :: s_lcmComp :: Nil = Enum(4)
  val state = RegInit(s_idle)

  state := MuxCase(state, Seq(
    (((state === s_idle) && io.in1.valid && io.in2.valid) -> s_dataIn),
    ((state === s_dataIn) -> s_gcdComp),
    (((state === s_gcdComp) && (x === y)) -> s_lcmComp),
    (((state === s_lcmComp) && io.out.ready) -> s_idle)))

  when(state === s_dataIn) {
    x := io.in1.bits
    y := io.in2.bits
    a := io.in1.bits
    b := io.in2.bits
  }

  when(state === s_gcdComp) {
    when(x >= y) {
      x := y
      y := x
    }.otherwise {
      y := y - x
    }
  }

  io.out.bits := x
  io.out.valid := state === s_lcmComp
}

class LCMRoCCAccel(opcodes: OpcodeSet, val w : Int)(implicit p: Parameters) extends LazyRoCC(opcodes){
    override lazy val module = new LazyRoCCModuleImp(this){//作为隐式类，也可显式写在外面
        // LazyRoCCModuleImp 已经定义好 IO

        val rd = RegInit(0.U(5.W))
        val rs1Value = RegInit(0.U(w.W))
        val rs1Enable = RegInit(false.B)
        val rs2Value = RegInit(0.U(w.W))
        val rs2Enable = RegInit(false.B)

        val busy = RegInit(false.B)
        val canResp = RegInit(false.B)
        io.cmd.ready := !busy
        io.busy := busy

        val canDecode = io.cmd.fire() && (io.cmd.bits.inst.funct===0.U)
        when(canDecode){ // 每当fire时候会Rocket-core送一条指令过来
            busy := true.B
            rs1Value := io.cmd.bits.rs1
            rs1Enable := true.B
            rs2Value := io.cmd.bits.rs2
            rs2Enable := true.B
            rd := io.cmd.bits.inst.rd
        }
        val lcm = Module(new LCM(w))
        lcm.io.in1.bits := rs1Value
        lcm.io.in2.bits := rs2Value
        lcm.io.in1.valid :=  rs1Enable
        lcm.io.in2.valid := rs2Enable

        val lcmRes = RegInit(0.U(w.W))

        lcm.io.out.ready := Mux(lcm.io.out.valid, true.B, false.B)
        when(lcm.io.out.valid){
            lcmRes := lcm.io.out.bits
            canResp := true.B
        }

        io.resp.valid := canResp
        io.resp.bits.rd := rd
        io.resp.bits.data := lcmRes
        when(io.resp.fire()){
            canResp := false.B
            busy := false.B
            rs1Enable := false.B
            rs2Enable := false.B
            rs1Value := 0.U
            rs2Value := 0.U
            lcmRes := 0.U
        }

    }
}

class WithLCMRoCCAccel extends Config((site,here,up) => {
    case BuildRoCC => Seq(
        (p:Parameters) => {
            val regWidth = 32 // 寄存器位宽
            val lcmAccel = LazyModule(new LCMRoCCAccel(OpcodeSet.all, regWidth)(p))
            lcmAccel
        }
    )
})
