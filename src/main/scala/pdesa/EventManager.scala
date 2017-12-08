package pdesa

import chisel3._
import chisel3.util._
import pdesa._


protected class EventDataBundle[D <: EventMsg](data_type: D, LP_id_bits: Int, time_bits: Int) extends Bundle {
  val data = data_type.cloneType
  val lp = UInt(LP_id_bits.W)
  val time = UInt(time_bits.W)

  override def cloneType: EventDataBundle.this.type =
    new EventDataBundle(data_type, LP_id_bits, time_bits).asInstanceOf[this.type]
}

protected class QueueController[T <: EventMsg](event_type: EventDataBundle[T], size: Int, order_by: T => UInt) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(event_type))
    val out = Decoupled(event_type)
  })

  val pq_stages: Int = log2Ceil(size + 1)
  val pq = Module(new PriorityQueue(event_type.data, order_by, pq_stages))

  val count: UInt = pq.io.count
  val deq_valid = Mux(count === 0.U, false.B, pq.io.ready)
  val enq_ready = Mux(count < size.U || io.out.ready, pq.io.ready, false.B)

  io.in.ready := enq_ready
  pq.io.in.data := io.in.bits.data

  io.out.valid := deq_valid
  io.out.bits.data := pq.io.out.data
  io.out.bits.lp := pq.io.out.data.lp_id
  io.out.bits.time := pq.io.out.data.time

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

  def get_target_queue_addr: UInt = {
    val lower_bits_rev = Reverse(msg.lp_id)(log2Ceil(Specs.num_queues)-1, 0)
    Reverse(lower_bits_rev)
  }
}

class EventAckMsg extends Bundle{
  val tag = UInt(Specs.core_bits.W)
}

class EventManagerIO(num_ifc: Int) extends Bundle {
  // TODO: Update EventManager Test to reflect change in interface
  val in = Flipped(Vec(num_ifc, Decoupled(new EventDispatchBundle)))
  val out = Vec(num_ifc, Valid(new EventDispatchBundle))
  val evt_req = Flipped(Vec(num_ifc, Decoupled(UInt(Specs.core_bits.W))))
  val ack = Vec(num_ifc, Valid(new EventAckMsg))
  val queue_min = Valid(UInt(Specs.time_bits.W))
  val recommended_q = Output(UInt(Specs.num_queues.W))
  val init = Flipped(Decoupled(Bool()))
  val conf = new Bundle{
    val num_init_events = Input(UInt(64.W))
  }
}

class EventManager(num_q: Int) extends Module {
  val io = IO(new EventManagerIO(num_q))

  val order_by: EventMsg => UInt = d => Cat(d.time, !d.cancel_evt).asUInt()
  val queues = for (i <- 0 until num_q) yield {
    Module {
      new QueueController(new EventDataBundle(io.in(0).bits.msg, Specs.lp_bits, Specs.time_bits), Specs.queue_size, order_by)
    }
  }

  val initializer = Module(new InitializationHelper)
  initializer.io.init <> io.init
  initializer.io.num_init_events := io.conf.num_init_events
  val init_event = initializer.io.event.map(_.bits.msg)

  queues.zipWithIndex.foreach {
    case (q, i) =>
      /* Process dequeue when an event is requested */
      q.io.out.nodeq()
      q.io.in.noenq()
      io.ack(i).valid := false.B

      io.evt_req(i).ready := q.io.out.valid
      initializer.io.req(i).nodeq()
      io.out(i).valid := false.B
      when(io.evt_req(i).valid || initializer.io.req(i).valid){
        q.io.out.deq()
        when(q.io.out.fire()){
          io.evt_req(i).deq()
          initializer.io.req(i).deq()
          io.out(i).valid := true.B
          io.out(i).bits.tag := Mux(!io.init.valid, io.evt_req(i).bits, initializer.io.req(i).bits)
          io.out(i).bits.msg := q.io.out.bits.data
          printf("Event popped from Queue %d, time: %d, LP %d --> Core %d\n", i.U, io.out(i).bits.msg.time,
            io.out(i).bits.msg.lp_id, io.out(i).bits.tag)
        }
      }

      /* Direct incoming events or initial events to the queue */
      val evt_bundle = Wire(q.io.in.bits) // clones the type
      evt_bundle.time := Mux(!io.init.valid, io.in(i).bits.msg.time, init_event(i).time)
      evt_bundle.data := Mux(!io.init.valid, io.in(i).bits.msg, init_event(i))
      io.in(i).ready := q.io.in.ready
      initializer.io.event(i).ready := q.io.in.ready
      when(io.in(i).valid || initializer.io.event(i).valid){
        q.io.in.enq(evt_bundle)
        printf("Event pushed to Queue %d, time: %d, LP: %d\n", i.U, evt_bundle.data.time, evt_bundle.data.lp_id)
      }

      /* Generate acknowledgement message for the origination core */
      // TODO: Update tests to reflect addition of Ack
      io.ack(i).valid := false.B
      when(io.in(i).valid && q.io.in.ready){
        io.ack(i).valid := true.B
        io.ack(i).bits.tag := io.in(i).bits.tag
      }
  }

  /* Supply minimum timestamp among queues for gvt computation */
  val queue_min = queues.map(_.io.out).map(x => Cat(!x.valid, x.bits.time)).reduce((a,b) => Mux(a < b, a, b))
  io.queue_min.valid := !Reverse(queue_min)(0)
  io.queue_min.bits := queue_min(Specs.time_bits - 1, 0)

  // recommend queue with minimum timestamp for draining
  val min_q_id = queues.map(_.io.out).zipWithIndex
    .map(x => (Cat(!x._1.valid, x._1.bits.time), x._2.U))
    .reduce((a,b) => (Mux(a._1 < b._1, a._1, b._1), Mux(a._1 < b._1, a._2, b._2)))._2
  val r_min_q_id = RegNext(min_q_id)
  io.recommended_q := r_min_q_id
}

class InitializationHelper extends Module {
  val io = IO(new Bundle {
    val init = Flipped(Decoupled(Bool()))
    val event = Vec(Specs.num_queues, Decoupled(new EventDispatchBundle))
    val req = Vec(Specs.num_queues, Decoupled(UInt(Specs.core_bits.W)))
    val num_init_events = Input(UInt(64.W))
  })

  val num_events = (io.num_init_events >> log2Ceil(Specs.num_queues)).asUInt()
  val evt_cnt = RegInit(0.U(64.W))
  val req_cnt = RegInit(0.U(Specs.core_bits.W))

  val sIDLE :: sEVENT :: sREQ :: sEND :: Nil = Enum(4)
  val state = RegInit(sIDLE)
  io.init.nodeq()
  io.event.foreach(_.noenq())
  io.req.foreach(_.noenq())
  io.req.foreach(_.bits := req_cnt)
  switch(state) {
    is(sIDLE) {
      when(io.init.valid) {
        state := sEVENT
        evt_cnt := 0.U
        req_cnt := 0.U
        printf("Start sending initial events\n")
      }
    }
    is(sEVENT) {
      when(Cat(io.event.map(_.ready)).andR()) { //When all the queues are ready
        evt_cnt := evt_cnt + 1.U
        for (q <- 0 until Specs.num_queues) {
          io.event(q).valid := true.B
          io.event(q).bits.msg.setValue(lp_id = Cat(q.U, evt_cnt(Specs.lp_bits - log2Ceil(Specs.num_queues) -1, 0)),
            time = 0.U, cancel = false.B)
        }
        when(evt_cnt === num_events) {
          state := sREQ
          printf("Start sending initial requests\n")
        }
      }
    }
    is(sREQ){
      for(q <- 0 until Specs.num_queues){
        io.req(q).valid := req_cnt(log2Ceil(Specs.num_queues)-1, 0) === q.U
      }
      when(Cat(io.req.map(_.ready)).orR){
        req_cnt := req_cnt + 1.U
        when(req_cnt === (Specs.num_cores-1).U){state := sEND}
      }
    }
    is(sEND) {
      io.init.deq()
      when(!io.init.valid) {
        state := sIDLE
      }
    }
  }
}
