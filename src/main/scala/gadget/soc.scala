package gadget.soc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.memory.sdram.SdramGeneration.SDR
import spinal.lib.memory.sdram._
import spinal.lib.memory.sdram.sdr.sim.SdramModel
import spinal.lib.memory.sdram.sdr.{SdramInterface, SdramTimings}
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.uart.{Apb3UartCtrl, Uart, UartCtrlGenerics, UartCtrlMemoryMappedConfig}
import spinal.lib.com.jtag.sim.JtagTcp
import spinal.lib.com.uart.sim.{UartDecoder, UartEncoder}
import spinal.lib.misc.{Apb3Clint}
import spinal.lib.misc.plic._

import java.nio.file.{Files, Paths}
import java.math.BigInteger
import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer

import gadget.cpu.{GadgetCpuConfig,GadgetCpu}
import gadget.plic._
import gadget.sdram._

/*
 * Specification of the W9825G6KH sdram chip
 */
object W9825G6KH {

  // Physical layout of the chip
  def layout = SdramLayout(
    generation  = SDR,
    bankWidth   = 2,
    columnWidth = 9,
    rowWidth    = 13,
    dataWidth   = 16
  )

  // Timing information of the chip
  def timingGrade6 = SdramTimings(
    bootRefreshCount = 8,
    tPOW = 200 us,
    tREF = 64 ms,
    tRC = 60 ns,
    tRFC = 60 ns,
    tRAS = 42 ns,
    tRP =  18 ns,
    tRCD = 18 ns,
    cMRD = 2,
    tWR = 7.5 ns,
    cWR = 2
  )
}

class Gadget(coreFrequency : HertzNumber, sdramFrequency : HertzNumber, sdramLayout : SdramLayout) extends Component {

  val io = new Bundle {
    val coreClock = in Bool()
    val sdramClock = in Bool()
    val asyncReset = in Bool()
    val jtag = slave(Jtag())
    val uart = master(Uart())
    val sdram = master(SdramInterface(sdramLayout))
  }

  // UART control config
  val uartCtrlConfig = UartCtrlMemoryMappedConfig.apply(
    baudrate = 115200,
    txFifoDepth = 32,
    rxFifoDepth = 32,
    writeableConfig = false,
    clockDividerWidth = 20
  )

  // Initial reset domain
  val resetCtrlDomain = ClockDomain(
    clock = io.coreClock,
    config = ClockDomainConfig(resetKind=BOOT)
  )

  // Reset controller for the global system, including the AXI bus
  val resetCtrl = new ClockingArea(resetCtrlDomain) {
    val mainClkResetUnbuffered = False

    val resetCounter = RegInit(U"6'h0")

    when (resetCounter =/= 63) {
      resetCounter := resetCounter + 1
      mainClkResetUnbuffered := True
    }
    when(BufferCC(io.asyncReset)) {
      resetCounter := 0
    }
    // Create all reset used later in the design
    // debugReset is only hooked up to the JTAG system
    // systemReset is hooked up to the rest of the system
    val debugReset = RegNext(mainClkResetUnbuffered)
    val systemReset  = RegNext(mainClkResetUnbuffered)
  }

  val coreDomain = ClockDomain(
    clock = io.coreClock,
    reset = resetCtrl.systemReset,
    frequency = FixedFrequency(coreFrequency)
  )

  // SDRAM clock domain
  val sdramDomain = ClockDomain(
    clock = io.sdramClock,
    reset = resetCtrl.systemReset,
    frequency = FixedFrequency(sdramFrequency)
  )

  val gadgetCpuConfig = GadgetCpuConfig.default

  // Core system clocking area
  val core = new ClockingArea(coreDomain) {

    // Instantiate the CPU with jtag and connect the wiring
    val cpu = GadgetCpu(gadgetCpuConfig)
    io.jtag <> cpu.io.jtag
    cpu.io.debugReset := resetCtrl.debugReset
    resetCtrl.systemReset setWhen(cpu.io.debugResetOut)

    // Add the SDRAM controller at the sdramDomain
    val sdramCtrl = Axi4SharedCCSdramCtrl(
      axiDataWidth = 32,
      axiIdWidth = 4,
      layout = sdramLayout,
      timing = W9825G6KH.timingGrade6,
      CAS = 3,
      sdramDomain
    )

    // Add and APB bridge
    val apbBridge = Axi4SharedToApb3Bridge(
      addressWidth = 20,
      dataWidth = 32,
      idWidth = 4
    )

    // And an interrupt controller
    val axiPlicCtrl = new Axi4Plic(
      sourceCount = 32,
      targetCount = 1
    )

    // Default value of the interrupts is false
    axiPlicCtrl.io.sources := (default -> false)

    // Instantiate an AXI crossbar with memory layout
    val axiCrossbar = Axi4CrossbarFactory()
    axiCrossbar.addSlaves(
      //ram.io.axi -> (0x00000000L, 16 kB),
      //ram2.io.axi -> (0x00004000L, 16 kB),
      sdramCtrl.io.axi   -> (0x00000000L, 32 MB),
      apbBridge.io.axi   -> (0x40000000L, 1 MB),
      axiPlicCtrl.io.bus -> (0x50000000L, 64 MB)
    )

    // Connect the CPU to different peripherals
    axiCrossbar.addConnections(
      cpu.io.iBus -> List(sdramCtrl.io.axi),
      cpu.io.dBus -> List(sdramCtrl.io.axi, apbBridge.io.axi, axiPlicCtrl.io.bus)
    )

    // and build it
    axiCrossbar.build()

    // Add uart
    val uartCtrl = Apb3UartCtrl(uartCtrlConfig)

    // Core local interrupts for a timer and software interrupts
    val clintCtrl = Apb3Clint(hartCount = 1)

    // Connect the different timer sources to the interrupt inputs of the cpu
    cpu.io.mTimerIrq := clintCtrl.io.timerInterrupt.orR
    cpu.io.mSoftIrq := clintCtrl.io.softwareInterrupt.orR
    cpu.io.mExtIrq := axiPlicCtrl.io.targets(0)

    axiPlicCtrl.io.sources(1) := uartCtrl.io.interrupt

    // Add the decoder for the APB bridge to the different peripherals
    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        clintCtrl.io.bus -> (0x00000, 64 kB),

        uartCtrl.io.apb -> (0x20000, 4 kB)
      )
    )

  }
  // Connect the IO to the peripheral ports
  io.uart <> core.uartCtrl.io.uart
  io.sdram <> core.sdramCtrl.io.sdram
  noIoPrefix()
}

object GadgetSpinalConfig extends SpinalConfig(mode = Verilog)

object GadgetVerilog {
  def main(args: Array[String]) {
    GadgetSpinalConfig.generateVerilog({
      val top = new Gadget(50 MHz, 100 MHz, W9825G6KH.layout)
      top.core.uartCtrl.busCtrl.printDataModel()
      top.core.clintCtrl.factory.printDataModel()
      top
    })
  }
}

import spinal.core.sim._
object GadgetSim {
  def main(args: Array[String]): Unit = {
    val simSlowDown = false
    val sdramLayout = W9825G6KH.layout
    SimConfig.allOptimisation.compile(new Gadget(50 MHz, 50 MHz, sdramLayout)).doSimUntilVoid{dut =>
      val mainClkPeriod = (1e12/(50 MHz).toDouble).toLong
      val jtagClkPeriod = mainClkPeriod*4 
      val uartBaudRate = 115200
      val uartBaudPeriod = (1e12/uartBaudRate).toLong

      // Stimulate both the main clock and the sdram clock at 50 MHz
      val coreClockDomain = ClockDomain(dut.io.coreClock, dut.io.asyncReset)
      val sdramClockDomain = ClockDomain(dut.io.sdramClock, dut.io.asyncReset)
      coreClockDomain.forkStimulus(mainClkPeriod)
      sdramClockDomain.forkStimulus(mainClkPeriod)

      val tcpJtag = JtagTcp(
        jtag = dut.io.jtag,
        jtagClkPeriod = jtagClkPeriod
      )

      val uartTx = UartDecoder(
        uartPin = dut.io.uart.txd, 
        baudPeriod = uartBaudPeriod
      )

      val uartRx = UartEncoder(
        uartPin = dut.io.uart.rxd,
        baudPeriod = uartBaudPeriod
      )

      val sdram = SdramModel(
        dut.io.sdram,
        sdramLayout,
        coreClockDomain
      )
    }
  }
}
