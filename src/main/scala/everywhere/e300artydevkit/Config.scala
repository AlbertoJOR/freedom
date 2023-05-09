// See LICENSE for license details.
package sifive.freedom.everywhere.e300artydevkit

import everywhere.e300artydevkit.{E300LCMConfig, E300MatrixSumConfig, FreedomERoCCExample2Config}
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy.{DTSModel, DTSTimebase}
import freechips.rocketchip.system._
import freechips.rocketchip.tile._
import sifive.blocks.devices.mockaon._
import sifive.blocks.devices.gpio._
import sifive.blocks.devices.pwm._
import sifive.blocks.devices.spi._
import sifive.blocks.devices.uart._
import sifive.blocks.devices.i2c._

// import files needed for developing the cores configurations in the
// main config of arty
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._


// To add a new core configuration you need to modify the DefaultFreedom EConfig
// It can be modified form the Makefile in the variable CONFIG overriding it with
// the custom configuration

// This configuration is used for the instantiation of a 64 bit processor with FPU
class With1Tiny64bitCore extends Config((site, here, up) => {
  case XLen => 64
  case RocketTilesKey => List(RocketTileParams(
    core = RocketCoreParams(
      useVM = false,
      mulDiv = Some(MulDivParams(mulUnroll = 8))),
    btb = None,
    dcache = Some(DCacheParams(
      rowBits = site(SystemBusKey).beatBits,
      nSets = 256, // 16Kb scratchpad
      nWays = 1,
      nTLBEntries = 4,
      nMSHRs = 0,
      blockBytes = site(CacheBlockBytes),
      scratch = Some(0x80000000L))),
    icache = Some(ICacheParams(
      rowBits = site(SystemBusKey).beatBits,
      nSets = 64,
      nWays = 1,
      nTLBEntries = 4,
      blockBytes = site(CacheBlockBytes)))))
  case RocketCrossingKey => List(RocketCrossingParams(
    crossingType = SynchronousCrossing(),
    master = TileMasterPortParams()
  ))
})
// The addition of the FPU
class With1Tiny64bitFPUCore extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(core = r.core.copy(
      fpu = r.core.fpu.map(_.copy(fLen = 64))))
  }
})


class TinyFPUConfig extends Config(
  new WithNoMemPort ++
    new WithNMemoryChannels(0) ++
    new WithNBanks(0) ++
    new With1Tiny64bitCore ++
    new With1Tiny64bitFPUCore ++
    new BaseConfig)
// RoCC example it includes 3 coprocessors
class FreedomERoCCExampleConfig extends Config(
  new WithNBreakpoints(2) ++
    new WithNExtTopInterrupts(0) ++
    new WithJtagDTM ++
    new WithL1ICacheWays(2) ++
    new WithL1ICacheSets(128) ++
    new WithDefaultBtb ++
    new WithRoccExample ++
    new TinyConfig
)


// Default FreedomEConfig
class DefaultFreedomEConfig extends Config (
  new WithNBreakpoints(2)        ++
  new WithNExtTopInterrupts(0)   ++
  new WithJtagDTM                ++
  new WithL1ICacheWays(2)        ++
  new WithL1ICacheSets(128)      ++
  new WithDefaultBtb             ++
  new TinyConfig
)

class Freedom64bitConfig extends Config (
  new WithNBreakpoints(2)        ++
  new WithNExtTopInterrupts(0)   ++
  new WithJtagDTM                ++
  new WithL1ICacheWays(2)        ++
  new WithL1ICacheSets(128)      ++
  new WithDefaultBtb             ++
  new TinyFPUConfig
)




// Freedom E300 Arty Dev Kit Peripherals
class E300DevKitPeripherals extends Config((site, here, up) => {
  case PeripheryGPIOKey => List(
    GPIOParams(address = 0x10012000, width = 32, includeIOF = true))
  case PeripheryPWMKey => List(
    PWMParams(address = 0x10015000, cmpWidth = 8),
    PWMParams(address = 0x10025000, cmpWidth = 16),
    PWMParams(address = 0x10035000, cmpWidth = 16))
  case PeripherySPIKey => List(
    SPIParams(csWidth = 4, rAddress = 0x10024000, defaultSampleDel = 3),
    SPIParams(csWidth = 1, rAddress = 0x10034000, defaultSampleDel = 3))
  case PeripherySPIFlashKey => List(
    SPIFlashParams(
      fAddress = 0x20000000,
      rAddress = 0x10014000,
      defaultSampleDel = 3))
  case PeripheryUARTKey => List(
    UARTParams(address = 0x10013000),
    UARTParams(address = 0x10023000))
  case PeripheryI2CKey => List(
    I2CParams(address = 0x10016000))
  case PeripheryMockAONKey =>
    MockAONParams(address = 0x10000000)
  case PeripheryMaskROMKey => List(
    MaskROMParams(address = 0x10000, name = "BootROM"))
})

// Freedom E300 Arty Dev Kit Peripherals
class E300ArtyDevKitConfig extends Config(
  new E300DevKitPeripherals    ++
  new DefaultFreedomEConfig().alter((site,here,up) => {
    case DTSTimebase => BigInt(32768)
    case JtagDTMKey => new JtagDTMConfig (
      idcodeVersion = 2,
      idcodePartNum = 0x000,
      idcodeManufId = 0x489,
      debugIdleCycles = 5)
    case RocketTilesKey => up(RocketTilesKey, site) map { r =>
      r.copy(icache = r.icache.map(_.copy(itimAddr = Some(0x8000000L))))
    }
  })
)

// New arty configuration that includes de FPU

class EFPU64bit extends Config(
  new E300DevKitPeripherals    ++
  new Freedom64bitConfig().alter((site,here,up) => {
    case DTSTimebase => BigInt(32768)
    case JtagDTMKey => new JtagDTMConfig (
      idcodeVersion = 2,
      idcodePartNum = 0x000,
      idcodeManufId = 0x489,
      debugIdleCycles = 5)
    case RocketTilesKey => up(RocketTilesKey, site) map { r =>
      r.copy(icache = r.icache.map(_.copy(itimAddr = Some(0x8000000L))))
    }
  })
)
// E310 with the RoCC Example
class E300RoCCExample extends Config(
  new E300DevKitPeripherals    ++
  new FreedomERoCCExampleConfig().alter((site,here,up) => {
    case DTSTimebase => BigInt(32768)
    case JtagDTMKey => new JtagDTMConfig (
      idcodeVersion = 2,
      idcodePartNum = 0x000,
      idcodeManufId = 0x489,
      debugIdleCycles = 5)
    case RocketTilesKey => up(RocketTilesKey, site) map { r =>
      r.copy(icache = r.icache.map(_.copy(itimAddr = Some(0x8000000L))))
    }
  })
)

// Example of a custom config
class E300RoCCExample2 extends Config(
  new E300DevKitPeripherals    ++
  new FreedomERoCCExample2Config().alter((site,here,up) => {
    case DTSTimebase => BigInt(32768)
    case JtagDTMKey => new JtagDTMConfig (
      idcodeVersion = 2,
      idcodePartNum = 0x000,
      idcodeManufId = 0x489,
      debugIdleCycles = 5)
    case RocketTilesKey => up(RocketTilesKey, site) map { r =>
      r.copy(icache = r.icache.map(_.copy(itimAddr = Some(0x8000000L))))
    }
  })
)



class E300LCM extends Config(
  new E300DevKitPeripherals    ++

    new E300LCMConfig().alter((site,here,up) => {
    case DTSTimebase => BigInt(32768)
    case JtagDTMKey => new JtagDTMConfig (
      idcodeVersion = 2,
      idcodePartNum = 0x000,
      idcodeManufId = 0x489,
      debugIdleCycles = 5)
    case RocketTilesKey => up(RocketTilesKey, site) map { r =>
      r.copy(icache = r.icache.map(_.copy(itimAddr = Some(0x8000000L))))
    }
  })
)
class E300MatrixSum extends Config(
  new E300DevKitPeripherals    ++

    new E300MatrixSumConfig().alter((site,here,up) => {
    case DTSTimebase => BigInt(32768)
    case JtagDTMKey => new JtagDTMConfig (
      idcodeVersion = 2,
      idcodePartNum = 0x000,
      idcodeManufId = 0x489,
      debugIdleCycles = 5)
    case RocketTilesKey => up(RocketTilesKey, site) map { r =>
      r.copy(icache = r.icache.map(_.copy(itimAddr = Some(0x8000000L))))
    }
  })
)