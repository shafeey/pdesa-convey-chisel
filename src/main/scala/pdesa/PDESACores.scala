package pdesa

import chisel3._
import chisel3.util._

class PDESACores extends Module{
  val io = IO(new Bundle{
    val event_data = Flipped(Decoupled(new EventDataBundle()))
  })
}
