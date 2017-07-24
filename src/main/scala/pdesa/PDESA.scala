package pdesa

import chisel3._
import chisel3.util._

//noinspection ScalaStyle
object Specs{
  val num_cores = 64
  val num_lp = 512
  val num_events = 1000
  val time_bits = 16

  val num_queues = 4
  val queue_size = 256

  val hist_size = 16


  def lp_bits = log2Ceil(num_lp)
  def core_bits = log2Ceil(num_cores)
}

class PDESA extends Module{
  val io = IO(new Bundle{
    val gvt = Output(UInt(16.W))
  })

  val cores = for (i<- 0 until Specs.num_cores) yield {
    Module(new PDESACore(i, Specs.lp_bits, Specs.time_bits))
  }

}


