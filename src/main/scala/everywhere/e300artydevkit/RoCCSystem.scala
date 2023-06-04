package everywhere.e300artydevkit
import freechips.rocketchip.config._
import freechips.rocketchip.system._
import freechips.rocketchip.subsystem._
import GDC._
import RoCCPrac.{WithMyRoCC, WithMyRoCC2, WithMyRoCC3}
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

class E300LCMConfig extends Config(
  new WithNBreakpoints(2) ++
    new WithNExtTopInterrupts(0) ++
    new WithJtagDTM ++
    new WithL1ICacheWays(2) ++
    new WithL1ICacheSets(128) ++
    new WithDefaultBtb ++
    new WithLCMRoCCAccel ++
    new TinyConfig
)

class E300MyRoCCConfig extends Config(
  new WithNBreakpoints(2) ++
    new WithNExtTopInterrupts(0) ++
    new WithJtagDTM ++
    new WithL1ICacheWays(2) ++
    new WithL1ICacheSets(128) ++
    new WithDefaultBtb ++
    new WithMyRoCC ++
    new TinyConfig
)


class E300MyRoCC3Config extends Config(
  new WithNBreakpoints(2) ++
    new WithNExtTopInterrupts(0) ++
    new WithJtagDTM ++
    new WithL1ICacheWays(2) ++
    new WithL1ICacheSets(128) ++
    new WithDefaultBtb ++
    new WithMyRoCC3 ++
    new TinyConfig
)

class E300MyRoCC2Config extends Config(
  new WithNBreakpoints(2) ++
    new WithNExtTopInterrupts(0) ++
    new WithJtagDTM ++
    new WithL1ICacheWays(2) ++
    new WithL1ICacheSets(128) ++
    new WithDefaultBtb ++
    new WithMyRoCC2 ++
    new TinyConfig
)