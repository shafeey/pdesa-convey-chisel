package pdesa

import chisel3._
import chisel3.util._

class CoreFinishedSignal extends Bundle{
  val core_id = UInt(Specs.core_bits.W)
  val last_lp = UInt(Specs.lp_bits.W)
  val hist_size = UInt(log2Ceil(Specs.hist_size).W)

  def get_target_queue_addr: UInt = {
    val lower_bits_rev = Reverse(last_lp)(log2Ceil(Specs.num_queues)-1, 0)
    Reverse(lower_bits_rev)
  }

  def setValue(core_id: UInt, lp: UInt, hist_size: UInt): Unit = {
    this.core_id := core_id
    this.last_lp := lp
    this.hist_size := hist_size
  }
}

class CoreStallController extends Module{
  val io = IO(new Bundle{
    val evt_msg = Flipped(Vec(Specs.num_queues, Valid(new EventDispatchBundle)))
    val evt_msg_passthru = Vec(Specs.num_queues, Decoupled(new EventDispatchBundle))
    val finished = Flipped(Vec(Specs.num_queues, Decoupled(new CoreFinishedSignal)))
    val finished_passthru = Vec(Specs.num_queues, Decoupled(new CoreFinishedSignal))
//    val start_target = Vec(Specs.num_queues, Valid(UInt(Specs.core_bits.W)))
    val start_target = Vec(Specs.num_queues, Valid(new StartMsg))
  })


  // Only process finished events when a queue isn't issuing events
  for(q<- 0 until Specs.num_queues){
    io.finished_passthru(q).bits := io.finished(q).bits
    io.finished_passthru(q).valid := io.finished(q).valid & (~io.evt_msg(q).valid).asUInt()
    io.finished(q).ready := io.finished_passthru(q).ready & (~io.evt_msg(q).valid).asUInt()
  }

  /* Keep record of LP associated to the cores.
   * Since LPs are grouped, keep one set of record for each group */
  val core_active = Seq.fill(Specs.num_core_grp)(RegInit(0.U(Specs.num_cores.W)))
  val core_lp_assoc = Seq.fill(Specs.num_core_grp)(RegInit(Vec(Seq.fill(Specs.num_cores)(0.U(Specs.lp_bits.W)))))
  val core_time_assoc = Seq.fill(Specs.num_core_grp)(RegInit(Vec(Seq.fill(Specs.num_cores)(0.U(Specs.time_bits.W)))))

  val sig_start = RegInit(init = Vec(Seq.fill(Specs.num_queues)(0.U(Specs.num_cores.W))))

  /* Update core-lp-time association records for each group */
  for(g <- 0 until Specs.num_queues) {
    val active_msg = io.evt_msg(g).bits.msg
    val tgt_core = io.evt_msg(g).bits.tag

    val returned_core = io.finished(g).bits.core_id
    val returned_lp = io.finished(g).bits.last_lp
    val r_returned_core = RegNext(returned_core)
    val r_returned_lp = RegNext(returned_lp)

    val r_issued = RegInit(false.B)
    r_issued := io.evt_msg(g).valid
    val r_returned = RegInit(false.B)
    r_returned := io.finished(g).fire()
    when(io.evt_msg(g).valid) {
      /* When new event are issued: mark core active, update table entries */
      core_active(g) := core_active(g) | (1.U(Specs.core_bits.W) << tgt_core).asUInt
//      core_active(g)(tgt_core) := true.B
      core_lp_assoc(g)(tgt_core) := active_msg.lp_id
      core_time_assoc(g)(tgt_core) := active_msg.time
    }.elsewhen(io.finished(g).fire()){
      // core activeness updated with finished core signal
      core_active(g) := core_active(g) & (~(1.U(Specs.core_bits.W) << returned_core)).asUInt
//      core_active(g)(returned_core) := false.B
    }

    val min_reducer = Module(new MinResolverBinTree(Specs.num_cores, 1))
    /* Find existing cores having same LP */
    // Register them to prevent synthesis nightmare later
    val r_tgt_core = RegNext(tgt_core)
    val r_lp_id = RegNext(active_msg.lp_id)
    val r_active = RegNext(next = core_active(g))

    val core_lp_matches = Wire(Vec(Specs.num_cores, Bool()))
    for(i <- 0 until Specs.num_cores){core_lp_matches(i) := Mux(r_active(i), core_lp_assoc(g)(i) === r_lp_id, false.B)}
    val matches = core_lp_matches.asUInt().orR()
    // if no active cores matches, should create a start signal
    val sig_start = Pipe(enqValid = r_issued & !matches, enqBits = r_tgt_core, latency = min_reducer.getLatency)
    val tgt_lp_delayed = Pipe(enqValid = true.B, enqBits = r_lp_id, latency = min_reducer.getLatency)

    /* When cores finish: search for stalling cores */
    val stall_matches = Wire(Vec(Specs.num_cores, Bool()))
    for(c<- 0 until Specs.num_cores){
      stall_matches(c) := Mux(r_active(c), core_lp_assoc(g)(c) === r_returned_lp && r_returned, false.B)
    }
    // Find the lowest TS among active cores
    min_reducer.io.mask := stall_matches.asUInt() & (~(1.U << r_returned_core)).asUInt()
    min_reducer.io.time.zip(core_time_assoc(g)).foreach( x => x._1 := x._2)
    val min_id = min_reducer.io.min_id
    val min_vld = min_reducer.io.valid
    val returned_lp_delayed = Pipe(enqValid = true.B, enqBits = r_returned_lp, latency = min_reducer.getLatency)

    // Create a start signal generated because some core has returned
    io.start_target(g).valid := sig_start.valid || min_vld
    io.start_target(g).bits.core_id := Mux(sig_start.valid, sig_start.bits, min_id)
    io.start_target(g).bits.lp_id := Mux(sig_start.valid, tgt_lp_delayed.bits, returned_lp_delayed.bits)
  }
}

private class MinResolverBinTree(wid: Int, stage_per_cycle: Int) extends Module{
  val io = IO(new Bundle{
    val mask = Input(UInt(Specs.num_cores.W))
//    val mask = Input(Vec(wid, Bool()))
    val time = Input(Vec(wid, UInt(Specs.time_bits.W)))

    val valid = Output(Bool())
    val min_id = Output(UInt(log2Ceil(wid).W))
  })
  val mask: Vec[Bool] = Vec(io.mask.toBools)

  val num_stages = log2Ceil(wid)
//  val s_in = for(s <- 0 to num_stages) yield {Reg(Vec(1 << s, UInt(Specs.time_bits.W)))}
//  val act = for(s <- 0 to num_stages) yield {RegInit(Vec(Seq.fill(1<<s)(false.B)))}
//  val idx = for(s <- 0 to num_stages) yield {Reg(Vec(1<<s, UInt((log2Ceil(wid)-s).W)))}
  val s_in = for(s <- 0 to num_stages) yield {RegInit(Vec(Seq.fill(1 << s)(0.U(Specs.time_bits.W))))}
  val act = for(s <- 0 to num_stages) yield {RegInit(Vec(Seq.fill(1<<s)(false.B)))}
  val idx = for(s <- 0 to num_stages) yield {RegInit(Vec(Seq.fill(1<<s)(0.U(32.W))))}

  s_in(num_stages).zip(io.time).foreach{case(r, in) => r := in}
  act(num_stages).zip(mask).foreach{case(a, m) => a := m}
  idx(num_stages).zipWithIndex.foreach{case(i, j) => i := j.U}
  for(s <- 0 until num_stages){
    for(i <- 0 until (1<<s)) {
      val l_vld = act(s + 1)(i * 2)
      val r_vld = act(s + 1)(i * 2 + 1)
      val l_idx = idx(s + 1)(i * 2)
      val r_idx = idx(s + 1)(i * 2 + 1)
      val left = s_in(s + 1)(i * 2)
      val right = s_in(s + 1)(i * 2 + 1)

      s_in(s)(i) := Mux(l_vld && r_vld, Mux(left < right, left, right), Mux(l_vld, left, right))
      idx(s)(i) := Mux(l_vld && r_vld, Mux(left < right, l_idx, r_idx), Mux(l_vld, l_idx, r_idx))
      act(s)(i) := l_vld || r_vld
    }
  }

  io.valid := act(0)(0)
  io.min_id := idx(0)(0)
  def getLatency: Int = num_stages + 1
}

class HistCountManager extends Module{
  val io = IO(new Bundle{
    val start_target = Flipped(Vec(Specs.num_queues, Valid(new StartMsg)))
    val start_msg = Vec(Specs.num_queues, Decoupled(new StartMsg))
    val finished = Flipped(Vec(Specs.num_queues, Valid(new CoreFinishedSignal)))
  })

  val histCount = Seq.fill(Specs.num_queues)(
    RegInit(Vec(Seq.fill(Specs.num_lp/Specs.num_queues)(0.U(log2Ceil(Specs.hist_size).W))))
  )

  for(g <- 0 until Specs.num_queues){
    when(io.finished(g).valid){
      histCount(g)(io.finished(g).bits.last_lp) := io.finished(g).bits.hist_size
    }

    io.start_msg(g).valid := false.B
    when(io.start_target(g).valid){
      val target_lp = io.start_target(g).bits.lp_id
      val target_core = io.start_target(g).bits.core_id
      io.start_msg(g).valid := true.B
      io.start_msg(g).bits.hist_size := histCount(g)(target_lp)
      io.start_msg(g).bits.core_id := target_core
    }
  }
}

class StartMsg extends Bundle{
  val hist_size = UInt(log2Ceil(Specs.hist_size).W)
  val core_id = UInt(Specs.core_bits.W)
  val lp_id = UInt(Specs.lp_bits.W)
}

class ControllerIO extends Bundle {
  val evt_msg = Flipped(Vec(Specs.num_queues, Valid(new EventDispatchBundle)))
  val evt_req = Vec(Specs.num_queues, Decoupled(UInt(Specs.core_bits.W)))
  val out_msg = Vec(Specs.num_queues, Decoupled(new EventDispatchBundle))

  val finished = Flipped(Vec(Specs.num_queues, Decoupled(new CoreFinishedSignal)))
  val start_sig = Vec(Specs.num_queues, Decoupled(new StartMsg))

  val recommended_q = Input(UInt(Specs.num_queues.W))
  val force_sync = Input(Bool())
}

class Controller extends Module {
  val io = IO(new ControllerIO)

/*****************
* Receive signal when a core returns
* X1) Update History data count
* X2) Update activeness information and release stalled cores
* X3) Create event request for the returned core
* When events are issued
* X4) Check conflict, de-stall if possible
**/

  val hist_count = Module(new HistCountManager)
  val stall_manager = Module(new CoreStallController)

  for(q<- 0 until Specs.num_queues){
    // X1 - Update hist data count
    hist_count.io.finished(q).valid := io.finished(q).fire()
    hist_count.io.finished(q).bits := io.finished(q).bits
    // X4 - Embed hist data with de-stall signal
    hist_count.io.start_target := stall_manager.io.start_target
  }
  val start_msg_queue = hist_count.io.start_msg.map(Queue(_, entries = 8, flow = true))
  io.start_sig.zip(start_msg_queue).foreach{case(o, q) => o <> q}

  // X2, X4 - Update activeness information and release stalled cores
  for(q<- 0 until Specs.num_queues){
    stall_manager.io.evt_msg(q) <> io.evt_msg(q)
    stall_manager.io.finished(q) <> io.finished(q)
  }

  // Each of these queues holds a request for event from a core
  val issue_reqs: Seq[DecoupledIO[UInt]] = stall_manager.io.finished_passthru.map{ m =>
    val w = Wire(new DecoupledIO(io.evt_req.head.bits.cloneType))
    w.valid := m.valid
    w.bits := m.bits.core_id
    m.ready := w.ready
    Queue(w, entries = Specs.num_cores, flow = false)
  }

  // X3 - Create event request
  // Connect requests to queue
  val requestServer = Module(new EventRequestServer)
  requestServer.io.req_in.zip(issue_reqs).foreach(x => x._1 <> x._2)
  io.evt_req.zip(requestServer.io.req_out).foreach(x => x._1 <> x._2)
  requestServer.io.recommended_q := io.recommended_q
  requestServer.io.foreced_sync := io.force_sync

//   Each event queue has a token to serve one of the request queues. Tokens rotate on each cycle.
//  val tokens: Seq[UInt] = for(i <- 0 until Specs.num_queues) yield RegInit((1 << i).U(Specs.num_queues.W))
//  tokens.head := tokens.last
//  for(t<- 1 until Specs.num_queues){tokens(t) := tokens(t-1)}

  for(i<- 0 until Specs.num_queues){
    when(io.evt_req(i).fire()){
      printf("~~~~ Requesting new event for EP: %d\n", io.evt_req(i).bits)
    }
  }
}

protected class EventRequestServer extends Module{
  val io = IO(new Bundle{
    val req_in = Flipped(Vec(Specs.num_queues, DecoupledIO(UInt(Specs.core_bits.W))))
    val req_out = Vec(Specs.num_queues, DecoupledIO(UInt(Specs.core_bits.W)))
    val recommended_q = Input(UInt(Specs.num_queues.W))
    val foreced_sync = Input(Bool())
  })

  /* When more than one events are present, input and outputs are served by a token system
   * Each out channel has a token to serve one of the request input. Tokens rotate on each cycle. */
  val tokens: Seq[UInt] = for(i <- 0 until Specs.num_queues) yield RegInit((1 << i).U(Specs.num_queues.W))
  tokens.head := tokens.last
  for(t<- 1 until Specs.num_queues){tokens(t) := tokens(t-1)}

  val single_requester = !(Cat(io.req_in.map(_.valid)) & (Cat(io.req_in.map(_.valid)) - 1.U)).orR
  val arbiter = Seq.fill(Specs.num_queues)(Module(new Arbiter(gen = io.req_in.head.bits, n = Specs.num_queues)))
  for(i<- 0 until Specs.num_queues){
    arbiter(i).io.in.zip(io.req_in).foreach(a => {
      a._1.valid := a._2.valid
      a._1.bits := a._2.bits
    })
    arbiter(i).io.out.ready := io.req_out(i).ready && io.recommended_q === i.U

    val muxed_valid = Mux1H(tokens(i), io.req_in.map(_.valid))
    val muxed_bits = Mux1H(tokens(i), io.req_in.map(_.bits))
    val muxed_ready = tokens.map(_(i)).zip(io.req_out.map(_.ready)).map(x => x._1 & x._2).reduce(_ | _)

    io.req_out(i).valid := Mux(single_requester, arbiter(i).io.out.valid && io.recommended_q === i.U , muxed_valid) && !io.foreced_sync
    io.req_out(i).bits := Mux(single_requester, arbiter(i).io.out.bits, muxed_bits)
    io.req_in(i).ready := Mux(single_requester, arbiter.map(_.io.in(i).ready).reduce(_ | _), muxed_ready) && !io.foreced_sync
  }
}

object Controller extends App {
  chisel3.Driver.execute(args, () => {
    new Controller
  })
}
