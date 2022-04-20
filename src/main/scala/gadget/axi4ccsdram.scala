package gadget.sdram

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.memory.sdram._
import spinal.lib.memory.sdram.sdr._

// SDRAM controller with a clock crossing fifo inserted in the AXI interface
object Axi4SharedCCSdramCtrl{
  def getAxiConfig(dataWidth : Int,idWidth : Int,layout : SdramLayout) : Axi4Config = {
    val widthFactor = dataWidth/layout.dataWidth
    Axi4Config(
      addressWidth = layout.byteAddressWidth,
      dataWidth = dataWidth,
      idWidth = idWidth,
      useLock = false,
      useRegion = false,
      useCache = false,
      useProt = false,
      useQos = false
    )
  }
}

case class Axi4SharedCCSdramCtrl(axiDataWidth : Int, axiIdWidth : Int, layout : SdramLayout, timing : SdramTimings, CAS : Int, sdramCd : ClockDomain) extends Component{
  val dataWidthFactor = axiDataWidth/layout.dataWidth
  require(dataWidthFactor != 0)
  require(isPow2(dataWidthFactor))
  val axiConfig = Axi4SharedCCSdramCtrl.getAxiConfig(axiDataWidth,axiIdWidth,layout)

  val io = new Bundle{
    val axi   = slave(Axi4Shared(axiConfig))
    val sdram = master(SdramInterface(layout))
  }

  val axi4SharedCC = Axi4SharedCC(axiConfig, ClockDomain.current, sdramCd,  4, 4, 4, 4)

  val sdramCore = new ClockingArea(sdramCd) {
    val ctrl = SdramCtrl(layout,timing,CAS,SdramCtrlAxi4SharedContext(axiConfig.idWidth))
    val ctrlBusAdapted = dataWidthFactor match {
      case 1 => ctrl.io.bus
      case _ => ctrl.io.bus.genScaledUpDriver(dataWidthFactor)
    }
    val bridge = ctrlBusAdapted.driveFrom(axi4SharedCC.io.output)
  }

  io.sdram <> sdramCore.ctrl.io.sdram
  io.axi <> axi4SharedCC.io.input
}
