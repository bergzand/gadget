package gadget.cpu

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.com.jtag.Jtag
import vexriscv._
import vexriscv.plugin._
import vexriscv.ip.{InstructionCacheConfig, DataCacheConfig}

import java.nio.file.{Files, Paths}
import java.math.BigInteger
import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer

/**
 * Defines the Gadget CPU and debug peripheral
 */

case class GadgetCpuConfig(genJTAG : Boolean,
                           hardwareBreakpointCount : Int,
                           cpuPlugins : ArrayBuffer[Plugin[VexRiscv]]
                           ){
                           }

object GadgetCpuConfig{
  def default : GadgetCpuConfig = default(0, true)
  def default(resetVector : BigInt = 0,
              genJTAG : Boolean = true) = GadgetCpuConfig(
    genJTAG = genJTAG,
    hardwareBreakpointCount = 4,
    cpuPlugins = ArrayBuffer(
      new IBusCachedPlugin(
          resetVector = resetVector,
          relaxedPcCalculation = false,
          compressedGen = true,
          injectorStage = true,
          prediction = DYNAMIC_TARGET,
          historyRamSizeLog2 = 9,
          keepPcPlus4 = true,
          config = InstructionCacheConfig(
            cacheSize = 4096,
            bytePerLine = 32,
            wayCount = 1,
            addressWidth = 32,
            cpuDataWidth = 32,
            memDataWidth = 32,
            catchIllegalAccess = true,
            catchAccessFault = true,
            asyncTagMemory = false,
            twoCycleRam = true,
            twoCycleCache = true
          ),
          memoryTranslatorPortConfig = null
      ),
      new DBusCachedPlugin(
          config = DataCacheConfig(
            cacheSize = 4096,
            bytePerLine = 32,
            wayCount = 2,
            addressWidth = 32,
            cpuDataWidth = 32,
            memDataWidth = 32,
            catchIllegal = true,
            catchAccessError = true,
            catchUnaligned = true
          ),
          memoryTranslatorPortConfig = null


      ),
      new StaticMemoryTranslatorPlugin(
        ioRange = _(31 downto 28) =/= 0x0
      ),
      new DecoderSimplePlugin(
          catchIllegalInstruction = true
      ),
      new RegFilePlugin(
          regFileReadyKind = plugin.SYNC,
          zeroBoot = true
      ),
      new IntAluPlugin,
      new MulPlugin,
      new MulDivIterativePlugin(
          genMul = false,
          genDiv = true
      ),
      new SrcPlugin(
          separatedAddSub = false,
          executeInsertion = false 
      ),
      new FullBarrelShifterPlugin,
      new HazardSimplePlugin(
          bypassExecute           = true,
          bypassMemory            = true,
          bypassWriteBack         = true,
          bypassWriteBackBuffer   = true,
          pessimisticUseSrc       = false,
          pessimisticWriteRegFile = false,
          pessimisticAddressMatch = false
      ),
      new BranchPlugin(
          earlyBranch = false,
          catchAddressMisaligned = true
      ),
      new CsrPlugin(
        config = CsrPluginConfig(
          catchIllegalAccess = false,
          mvendorid      = null,
          marchid        = null,
          mimpid         = null,
          mhartid        = null,
          misaExtensionsInit = 66,
          misaAccess     = CsrAccess.NONE,
          mtvecAccess    = CsrAccess.READ_WRITE,
          mtvecInit      = 0x80000020l,
          mepcAccess     = CsrAccess.READ_WRITE,
          mscratchGen    = false,
          mcauseAccess   = CsrAccess.READ_ONLY,
          mbadaddrAccess = CsrAccess.READ_WRITE,
          mcycleAccess   = CsrAccess.NONE,
          minstretAccess = CsrAccess.NONE,
          ecallGen       = true,
          ebreakGen      = true,
          wfiGenAsWait   = true,
          wfiGenAsNop    = false,
          ucycleAccess   = CsrAccess.NONE
        )
      )
    )
  )
}

case class GadgetCpu(config : GadgetCpuConfig
                ) extends Component {
  import config._

  val axiDBusConfig = Axi4Config(
    addressWidth = 32,
    dataWidth = 32,
    useId = false,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useQos = false
  )

  val axiIBusConfig = Axi4Config(
    addressWidth = 32,
    dataWidth = 32,
    useId = false,
    useRegion = false,
    useLock = false,
    useQos = false,
    useSize = false
  )

  val io = new Bundle {
    val debugReset = in Bool()
    val debugResetOut = out Bool()
    val jtag = slave(Jtag())
    val iBus = master(Axi4ReadOnly(axiIBusConfig))
    val dBus = master(Axi4Shared(axiDBusConfig))
    val mExtIrq = in Bool()
    val mTimerIrq = in Bool()
    val mSoftIrq = in Bool()
  }

  val debugClockDomain = ClockDomain(
    clock = ClockDomain.current.readClockWire,
    reset = io.debugReset,
    frequency = FixedFrequency(ClockDomain.current.frequency.getValue)
  )

  val vexRiscVPlugins = config.cpuPlugins 
  if(config.genJTAG) vexRiscVPlugins += new DebugPlugin(debugClockDomain, config.hardwareBreakpointCount)
  val vexRiscvConfig = VexRiscvConfig(plugins = vexRiscVPlugins)
  val cpu = new VexRiscv(vexRiscvConfig)
  for (plugin <- config.cpuPlugins) plugin match {
    case plugin : IBusSimplePlugin => io.iBus <> plugin.iBus.toAxi4ReadOnly()
    case plugin : IBusCachedPlugin => io.iBus <> plugin.iBus.toAxi4ReadOnly()
    case plugin : DBusSimplePlugin => io.dBus <> plugin.dBus.toAxi4Shared()
    case plugin : DBusCachedPlugin => io.dBus <> plugin.dBus.toAxi4Shared(true)
    case plugin : CsrPlugin => {
      plugin.externalInterrupt := io.mExtIrq
      plugin.timerInterrupt := io.mTimerIrq
      plugin.softwareInterrupt := io.mSoftIrq
    }
    case plugin : DebugPlugin => plugin.debugClockDomain{
      io.debugResetOut := RegNext(plugin.io.resetOut)
      io.jtag <> plugin.io.bus.fromJtag()
    }
    case _ =>
  }
}
