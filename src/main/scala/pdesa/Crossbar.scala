package pdesa

import chisel3._
import chisel3.util._

/** IO bundle definition for the Crossbar module.
  * The master and slave interfaces are decoupled.
  *
  * @param gen data type
  * @param n_mi number of master interfaces
  * @param n_si number of slave interfaces
  */
class CrossbarIO[T <: Data](gen: T, n_mi: Int, n_si: Int) extends Bundle {
  val mi = Flipped(Vec(n_mi, Decoupled(gen)))
  val si = Vec(n_si, Decoupled(gen))
  val addr = Vec(n_mi, Input(UInt(log2Ceil(n_si).W)))
}

/** Hardware module to direct data to target slave node.
  * Unidirectional data transfer.
  */

class Crossbar[T <: Data](gen: T, n_mi: Int, n_si: Int) extends Module {
  val io = IO(new CrossbarIO(gen, n_mi, n_si))

  //  Data buffer holds the data from the masters.
  private val m_buf = for(i<- 0 until n_mi) yield{
    val in_fifo = Queue(io.mi(i))
    in_fifo
  }

  // Input buffers pass data to Arbiters. Valid/Ready is passed based on address.
  private val arbiters = for(i<- 0 until n_si) yield{
    val rr_arbiter = new RRArbiter(gen, n_mi)
    for(j<- 0 until n_mi){
      rr_arbiter.io.in(j).valid := false.B
      when(io.addr(j) === j.U){
        rr_arbiter.io.in(j).valid := m_buf(j).valid
      }

      rr_arbiter.io.in(j).bits := m_buf(j).bits
      m_buf(j).ready := rr_arbiter.io.in(j).ready
    }
    rr_arbiter
  }

  // Output queues feed from arbiters. Keep data until slave can consume it.
  private val s_buf = for(i<- 0 until n_si) yield{
    val out_fifo = Queue(arbiters(i).io.out)
    out_fifo
  }

  // Direct output queue data to output for consumption by slave
  for(i<- 0 until n_si){
    io.si(i) <> s_buf(i)
  }
}

