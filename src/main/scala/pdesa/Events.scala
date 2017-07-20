package pdesa

import chisel3._
import chisel3.util._

class EventData(lp_id_bits: Int, time_bits: Int) extends Bundle {
  val lp_id = UInt(lp_id_bits.W)
  val time = UInt(time_bits.W)

  override def cloneType: EventData.this.type = new EventData(lp_id_bits, time_bits).asInstanceOf[this.type]
}
