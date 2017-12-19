package pdesa

import chisel3._
import chisel3.util._

class Stack[T <: Data](gen: T, val entries: Int) extends Module() {
  val io = IO(new QueueIO(gen, entries))

  private val ram = Mem(entries, gen)
  private val ptr = if (entries > 1) RegInit(0.U((log2Ceil(entries)+1).W)) else 0.U

  private val empty = ptr === 0.U
  private val full = ptr === entries.U
  private val do_enq = Wire(init=io.enq.fire())
  private val do_deq = Wire(init=io.deq.fire())

  when (do_enq) {
    ram(ptr) := io.enq.bits
    ptr := ptr + 1.U
  }
  when (do_deq) {
    ptr := ptr - 1.U
  }

  io.deq.valid := !empty
  io.enq.ready := !full
  io.deq.bits := ram(ptr)

  io.count := ptr
}