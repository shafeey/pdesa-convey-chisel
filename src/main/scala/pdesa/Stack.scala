package pdesa

import chisel3._
import chisel3.util._

class Stack[T <: Data](gen: T, val entries: Int) extends Module() {
  val io = IO(new QueueIO(gen, entries))

  private val ram = Mem(entries, gen)
  private val negative_one = (~0.U((log2Ceil(entries) + 1).W)).asUInt()
  private val st_ptr = if (entries > 1) RegInit(init = negative_one) else 1.U

  private val empty = st_ptr === negative_one
  private val full = st_ptr === entries.U

  io.enq.ready := !full
  io.deq.valid := !empty

  when(io.enq.fire()){
    io.deq.valid := false.B
    st_ptr := st_ptr + 1.U
  }

  val wen = io.enq.fire()
  val ren = io.deq.fire()

  when (wen) { ram(st_ptr + 1.U) := io.enq.bits }
    .elsewhen (ren) { st_ptr := st_ptr - 1.U }
  io.deq.bits := ram(st_ptr)

}