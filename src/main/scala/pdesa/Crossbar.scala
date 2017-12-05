package pdesa

import chisel3._
import chisel3.util._

/** data bundle definition crossbar Input.
  * Crossbar takes an address and a data to pass to the slave.
  *
  * @param dtype data type
  * @param n_si number of slave interfaces
  */
class CrossbarBundle[T <: Data](dtype: T, n_si: Int) extends Bundle{
  val addr = Output(UInt(log2Ceil(n_si).W))
  val data = Output(dtype)

  override def cloneType: CrossbarBundle.this.type = new CrossbarBundle(dtype, n_si).asInstanceOf[this.type]
}

/** IO bundle definition for the Crossbar module.
  * The master and slave interfaces are decoupled.
  *
  * @param dtype data type
  * @param n_mi number of master interfaces
  * @param n_si number of slave interfaces
  */
class CrossbarIO[T <: Data](dtype: T, n_mi: Int, n_si: Int) extends Bundle {
  val mi = Flipped(Vec(n_mi, Decoupled(new CrossbarBundle(dtype, n_si))))
  val si = Vec(n_si, Decoupled(dtype))

  def insert(i: Int, addr: UInt, data_in: DecoupledIO[T]): Unit = {
    data_in.ready := mi(i).ready
    mi(i).valid := data_in.valid
    mi(i).bits.data := data_in.bits
    mi(i).bits.addr := Reverse(Reverse(addr)(log2Ceil(n_si)-1, 0))
  }
}

/** Hardware module to direct data to target slave node. Provides unidirectional data transfer.
  *
  * @param dtype data type
  * @param n_mi number of master interfaces
  * @param n_si number of slave interfaces
  */

class Crossbar[T <: Data](dtype: T, n_mi: Int, n_si: Int) extends Module {
  val io = IO(new CrossbarIO(dtype, n_mi, n_si))

  //  Data buffer holds the data from the masters.
  private val m_buf = for(i<- 0 until n_mi) yield{
    val in_fifo = Queue(io.mi(i))
    in_fifo
  }

  // Input buffers pass data to Arbiters. Valid/Ready is passed based on address.
  private val arbiters = for(i<- 0 until n_si) yield{
    val rr_arbiter = Module(new RRArbiter(dtype, n_mi))
    for(j<- 0 until n_mi){
      rr_arbiter.io.in(j).valid := false.B
      when(m_buf(j).bits.addr === i.U){
        rr_arbiter.io.in(j).valid := m_buf(j).valid
        m_buf(j).ready := rr_arbiter.io.in(j).ready
      }
      rr_arbiter.io.in(j).bits := m_buf(j).bits.data
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

