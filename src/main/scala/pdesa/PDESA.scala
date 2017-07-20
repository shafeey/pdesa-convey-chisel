package pdesa

import chisel3._
import chisel3.util._

object Specs{
  val num_cores = 64
  val num_lp = 512
  val num_events = 1000

  val num_queues = 4
  val queue_size = 256
}

class PDESA extends Module{
  val io = IO(new Bundle{
    val gvt = Output(UInt(16.W))
  })

  val cores = for (i<- 0 until Specs.num_cores) yield {
    Module(new PDESACores)
  }

}


