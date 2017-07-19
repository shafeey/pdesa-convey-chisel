package pdesa

import chisel3._
import chisel3.util._
import pdesa.PriorityQueue


class EventDataBundle[D <: Data](data_type: D, LP_id_bits: Int, time_bits: Int) extends Bundle {
  val data = Output(data_type.cloneType)
  val lp = Output(UInt(LP_id_bits.W))
  val time = Output(UInt(time_bits.W))

  override def cloneType: EventDataBundle.this.type =
    new EventDataBundle(data_type, LP_id_bits, time_bits).asInstanceOf[this.type]
}

class QueueController[T <: EventDataBundle[T]](event_type: EventDataBundle[T], size: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(event_type))
    val out = Decoupled(event_type)
  })

  val pq_stages: Int = log2Ceil(size + 1)

  val pq = Module(new PriorityQueue(event_type, event_type.time, pq_stages))

  val count: UInt = pq.io.count
  val deq_valid = Mux(count === 0.U, false.B, pq.io.ready)
  val enq_ready = Mux(count < size.U && io.out.ready, pq.io.ready, false.B)

  io.in.ready := enq_ready
  pq.io.in.data := io.in.bits
  pq.io.in.priority := io.in.bits.time

  io.out.valid := deq_valid
  io.out.bits := pq.io.out

  /* Enqueue operation when an event is valid and queue is ready for operation
   * Dequeue when output is ready to receive and queue is ready for operation
   * Replace when both input and output are requesting
   */
  val enq_op: Bool = enq_ready && io.in.valid
  val deq_op: Bool = deq_valid && io.out.ready
  pq.io.op := Cat(deq_op, enq_op)
}