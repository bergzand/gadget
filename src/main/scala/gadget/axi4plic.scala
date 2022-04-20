package gadget.plic

import spinal.core._
import spinal.lib.bus.amba4.axi._
import spinal.lib.misc.plic._
import spinal.lib._

// Quick Platform Interrupt Controller with an AXI4 interface
object Axi4Plic{
  def getAxiConfig() = 
    Axi4Config(
      addressWidth = 22,
      dataWidth    = 32,
      idWidth      = 4,
      useLock      = false,
      useRegion    = false,
      useCache     = false,
      useProt      = false,
      useQos       = false
    )
}

class Axi4Plic(sourceCount : Int, targetCount : Int) extends Component{
  val priorityWidth = 2
  val plicMapping = PlicMapping.sifive
  import plicMapping._

  val io = new Bundle {
    val bus = slave(Axi4(Axi4Plic.getAxiConfig))
    val sources = in Bits (sourceCount bits)
    val targets = out Bits (targetCount bits)
  }

  val gateways = (for ((source, id) <- (io.sources.asBools, 1 until sourceCount).zipped) yield PlicGatewayActiveHigh(
    source = source,
    id = id,
    priorityWidth = priorityWidth
  )).toSeq

  val targets = for (i <- 0 until targetCount) yield PlicTarget(
    gateways = gateways,
    priorityWidth = priorityWidth
  )

  io.targets := targets.map(_.iep).asBits

  val bus = new Axi4SlaveFactory(io.bus)
  val mapping = PlicMapper(bus, plicMapping)(
    gateways = gateways,
    targets = targets
  )
}
