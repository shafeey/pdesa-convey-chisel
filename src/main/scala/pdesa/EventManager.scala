package pdesa

import chisel3._
import chisel3.util._
import pdesa._


protected class EventDataBundle[D <: Data](data_type: D, LP_id_bits: Int, time_bits: Int) extends Bundle {
  val data = data_type.cloneType
  val lp = UInt(LP_id_bits.W)
  val time = UInt(time_bits.W)

  override def cloneType: EventDataBundle.this.type =
    new EventDataBundle(data_type, LP_id_bits, time_bits).asInstanceOf[this.type]
}

protected class QueueController[T <: Data](event_type: EventDataBundle[T], size: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(event_type))
    val out = Decoupled(event_type)
  })

  val pq_stages: Int = log2Ceil(size + 1)

  val pq = Module(new PriorityQueue(event_type.data, event_type.time, pq_stages))

  val count: UInt = pq.io.count
  val deq_valid = Mux(count === 0.U, false.B, pq.io.ready)
  val enq_ready = Mux(count < size.U && io.out.ready, pq.io.ready, false.B)

  io.in.ready := enq_ready
  pq.io.in.data := io.in.bits.data
  pq.io.in.priority := io.in.bits.time

  io.out.valid := deq_valid
  io.out.bits.data := pq.io.out.data
  io.out.bits.time := pq.io.out.priority

  /* Enqueue operation when an event is valid and queue is ready for operation
   * Dequeue when output is ready to receive and queue is ready for operation
   * Replace when both input and output are requesting
   */
  val enq_op: Bool = enq_ready && io.in.valid
  val deq_op: Bool = deq_valid && io.out.ready
  pq.io.op := Cat(deq_op, enq_op)
}

class EventDispatchBundle extends Bundle {
  val msg = new EventMsg(Specs.lp_bits, Specs.time_bits)
  val tag = UInt(Specs.core_bits.W)
}

class EventManagerIO(num_ifc: Int) extends Bundle {
  val in = Flipped(Vec(num_ifc, Decoupled(new EventMsg(Specs.lp_bits, Specs.time_bits))))
  val out = Vec(num_ifc, Valid(new EventDispatchBundle))
  val evt_req = Flipped(Vec(num_ifc, Decoupled(UInt(Specs.core_bits.W))))
}

class EventManager(num_q: Int) extends Module {
  val io = IO(new EventManagerIO(num_q))

  val queues = for (i <- 0 until num_q) yield {
    Module(new QueueController(new EventDataBundle(io.in(0).bits, Specs.lp_bits, Specs.time_bits), Specs.queue_size))
  }

  queues.zipWithIndex.foreach {
    case (q, i) =>
      /* Process dequeue when an event is requested */
      q.io.out.nodeq()
      io.evt_req(i).nodeq()
      io.out(i).valid := false.B
      when(io.evt_req(i).valid){
        q.io.out.deq()
        when(q.io.out.fire()){
          io.evt_req(i).deq()
          io.out(i).valid := true.B
          io.out(i).bits.tag := io.evt_req(i).bits
          io.out(i).bits.msg := q.io.out.bits.data
        }
      }

      /* Direct incoming events to the queue */
      val evt_bundle = Wire(q.io.in.bits)
      evt_bundle.time := io.in(i).bits.time
      evt_bundle.data := io.in(i).bits
      io.in(i).ready := q.io.in.ready
      when(io.in(i).valid){
        q.io.in.enq(evt_bundle)
      }
  }
}
