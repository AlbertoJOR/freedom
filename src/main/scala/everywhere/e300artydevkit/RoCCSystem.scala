package everywhere.e300artydevkit
import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.system._
import freechips.rocketchip.subsystem._

// Declaration of new core implementations



class FreedomERoCCExample2Config extends Config(
  new WithNBreakpoints(2) ++
    new WithNExtTopInterrupts(0) ++
    new WithJtagDTM ++
    new WithL1ICacheWays(2) ++
    new WithL1ICacheSets(128) ++
    new WithDefaultBtb ++
    new WithRoccExample2 ++
    new TinyConfig
)