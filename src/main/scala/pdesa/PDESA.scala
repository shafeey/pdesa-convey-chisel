package pdesa

import chisel3._
import chisel3.util._

//noinspection ScalaStyle
//object Specs {
//  val num_cores = 64
//  val num_lp = 512
//  val num_events = 1000
//  val time_bits = 16
//
//  val num_core_grp = 8
//  val num_queues = 4
//  val queue_size = 255
//
//  val hist_size = 16
//
//  val sim_end_time = 1000
//
//  def lp_bits = log2Ceil(num_lp)
//
//  def core_bits = log2Ceil(num_cores)
//}
object Specs {
  val num_cores = 64
  val num_lp = 256
  val num_events = 256
  val time_bits = 16

  val num_core_grp = 8
  val num_queues = 4
  val queue_size = 255

  val hist_size = 16

  val NUM_MEM_BYTE = 1
  val mem_addr_wid = 32

  def lp_bits = log2Ceil(num_lp)
  def core_bits = log2Ceil(num_cores)
  def num_cores_per_grp = num_cores/num_core_grp

  val MEM_RD_CMD = 1.U
  val MEM_WR_CMD = 2.U
  val MEM_SIZE_BYTE = 0.U
}

class PDESA extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val target_gvt = Input(UInt(Specs.time_bits.W))
    val done = Valid(UInt(Specs.time_bits.W))

    val dbg: DebugSignalBundle = new DebugSignalBundle
  })

//  val global_start = RegInit(init = false.B)
  val gvt = RegInit(0.U(Specs.time_bits.W))

  // Instantiate cores
  val cores = for (i <- 0 until Specs.num_cores) yield {
    Module(new PDESACore(i, Specs.lp_bits, Specs.time_bits))
  }
  cores.foreach(_.io.gvt := gvt)

  val gen_xbar = Module(new Crossbar(new EventDispatchBundle, Specs.num_core_grp, Specs.num_queues))
  val gen_arbs = for (i <- 0 until Specs.num_core_grp) yield {
    Module(new RRArbiter(new EventDispatchBundle, Specs.num_cores / Specs.num_core_grp))
  }

  val evt_mgr = Module(new EventManager(Specs.num_queues))

  /* Deliver event from core to event queue through arbiter and crossbar */
  for (i <- 0 until Specs.num_core_grp) {
    for (j <- 0 until Specs.num_cores / Specs.num_core_grp) {
      // cores to arbiters
      cores(i * Specs.num_cores / Specs.num_core_grp + j).io.generated_evt <> gen_arbs(i).io.in(j)
    }
    gen_xbar.io.insert(i, gen_arbs(i).io.out.bits.get_target_queue_addr, gen_arbs(i).io.out) // arbiter to xbar input
  }

  for (i <- 0 until Specs.num_queues) {
    gen_xbar.io.si(i) <> evt_mgr.io.in(i) // Crossbar to event manager input
  }

  /* Returns acknowledgement from Event Manager to the originating core */
  def ackFilter(id: Int, in: DecoupledIO[EventAckMsg]) : Valid[EventAckMsg] = {
    val out = Wire(Valid(new EventAckMsg))
    out.valid := in.valid && in.bits.tag === id.U
    out.bits := in.bits
    out
  }

  val ack_xbar = Module(new Crossbar(new EventAckMsg, Specs.num_queues, Specs.num_core_grp))
  for(i<- 0 until Specs.num_queues){
    val ack_ret = Wire(Decoupled(new EventAckMsg))
    // Convert ValidIO to DecoupledIO
    ack_ret.valid := evt_mgr.io.ack(i).valid
    ack_ret.bits := evt_mgr.io.ack(i).bits
    val ack_target = evt_mgr.io.ack(i).bits.tag
    ack_xbar.io.insert(i, ack_target, ack_ret)
  }
  for(i<- 0 until Specs.num_core_grp){
    ack_xbar.io.si(i).ready := true.B // Convert DecoupledIO to ValidIO
  }
  for (i <- 0 until Specs.num_cores) {
    cores(i).io.dispatch_ack := ackFilter(i, ack_xbar.io.si(i / Specs.num_cores_per_grp))
  }

  /* Deliver event from event queue to cores through a crossbar and then broadcast */
  def evtIssueFilter(id: Int, in: DecoupledIO[EventDispatchBundle]): Valid[EventMsg] = {
    val out = Wire(Valid(new EventMsg(Specs.lp_bits, Specs.time_bits)))
    out.valid := in.valid && in.bits.tag === id.U
    out.bits := in.bits.msg
    out
  }

  val issue_xbar = Module(new Crossbar(new EventDispatchBundle, Specs.num_queues, Specs.num_core_grp))
  for(i<- 0 until Specs.num_queues){
    val issue_evt = Wire(Decoupled(new EventDispatchBundle))
    // Convert ValidIO to DecoupledIO
    issue_evt.valid := evt_mgr.io.out(i).valid
    issue_evt.bits := evt_mgr.io.out(i).bits
    val issue_target = evt_mgr.io.out(i).bits.tag(Specs.core_bits - 1, Specs.core_bits - log2Ceil(Specs.num_core_grp))
    issue_xbar.io.insert(i, issue_target, issue_evt)
  }
  for(i<- 0 until Specs.num_core_grp){
    issue_xbar.io.si(i).ready := true.B // Convert DecoupledIO to ValidIO
  }
  for (i <- 0 until Specs.num_cores) {
    cores(i).io.issued_evt := evtIssueFilter(i, issue_xbar.io.si(i / Specs.num_cores_per_grp))
  }

  /* Controller snoops in the events issued to the cores */
  val controller = Module(new Controller)
  controller.io.evt_msg.zip(issue_xbar.io.mi).foreach{case(c, x) =>
    c.valid := x.fire() // Snoops in when a message is pushed onto the crossbar.
    c.bits := x.bits.data // Only need the data bundle
    /* Xbar delay is variable, so start signal from controller may arrive before event.
     * Cores should be ready to catch start signal when in idle */
    // TODO: Modify cores to catch start signal anytime after idle
  }

  /* Cores receive start signal from controller */
  def startFilter(id: Int, in: DecoupledIO[StartMsg]) : Valid[StartMsg] = {
    val out = Wire(Valid(new StartMsg))
    out.valid := in.valid && in.bits.core_id === id.U
    out.bits := in.bits
    out
  }

  val start_xbar = Module(new Crossbar(new StartMsg, Specs.num_queues, Specs.num_core_grp))
  for(i<- 0 until Specs.num_queues){
    val start_sig = controller.io.start_sig(i)
    start_xbar.io.insert(i, start_sig.bits.core_id, start_sig)
  }
  start_xbar.io.si.foreach(_.ready := true.B)// Convert DecoupledIO to ValidIO
  for (i <- 0 until Specs.num_cores) {
    val destall = startFilter(i, start_xbar.io.si(i / Specs.num_cores_per_grp))
//    val g_start_msg = Wire(new StartMsg)
//    g_start_msg.hist_size := 0.U
//    cores(i).io.start.bits := Mux(global_start, g_start_msg, destall.bits)
//    cores(i).io.start.valid := global_start || destall.valid
    cores(i).io.start.bits := destall.bits
    cores(i).io.start.valid := destall.valid
  }

  /* Cores signals controllers when they return */
  val ret_arbs = for (i <- 0 until Specs.num_core_grp) yield {
    Module(new RRArbiter(new CoreFinishedSignal, Specs.num_cores / Specs.num_core_grp))
  }
  val ret_xbar = Module(new Crossbar(new CoreFinishedSignal, Specs.num_core_grp, Specs.num_queues))
  for (i <- 0 until Specs.num_core_grp) {
    for (j <- 0 until Specs.num_cores / Specs.num_core_grp) {
      // cores to arbiters
      cores(i * Specs.num_cores / Specs.num_core_grp + j).io.finished <> ret_arbs(i).io.in(j)
    }
    ret_xbar.io.insert(i, ret_arbs(i).io.out.bits.last_lp, ret_arbs(i).io.out) // arbiter to xbar input
  }
  for(q <- 0 until Specs.num_queues){
    controller.io.finished(q) <> ret_xbar.io.si(q)
  }

  /* Controller request events for idle cores */
  evt_mgr.io.evt_req.zip(controller.io.evt_req).foreach{case(e, c) => e <> c}

  /* Connect cores to event history for requesting past events*/
  val evt_hist_mgr = Module(new EventHistoryManager)
  val hist_arb = Module(new RRArbiter(evt_hist_mgr.io.hist_req.bits.cloneType, Specs.num_cores))
  for(i<- 0 until Specs.num_cores){
    cores(i).io.hist_req <> hist_arb.io.in(i)
  }
  hist_arb.io.out <> evt_hist_mgr.io.hist_req

  /* Event history is broadcast to cores */
  def histFilter(id: Int, in: Valid[EventHistoryRsp]): Valid[EventHistoryRsp] = {
    val out = Wire(Valid(new EventHistoryRsp(Specs.lp_bits, Specs.time_bits)))
    out.valid := in.valid && in.bits.EP_id === id.U
    out.bits := in.bits
    out
  }
  for(i <- 0 until Specs.num_cores){cores(i).io.hist_rsp <> histFilter(i, evt_hist_mgr.io.hist_rsp)}

  /* GVT updates */
  val queue_min_window = RegInit(Vec(Seq.fill(4)(0.U(Specs.time_bits.W))))
  queue_min_window.head := Mux(evt_mgr.io.queue_min.valid, evt_mgr.io.queue_min.bits, ~(0.U(Specs.time_bits.W)))
  for(i<- 1 until 4){ queue_min_window(i) := queue_min_window(i-1) }
  val queue_min = queue_min_window.reduce((a,b) => Mux(a < b, a, b))

  val gvt_resolver = Module(new GVTResolver)
  gvt_resolver.io.queue_min.bits := queue_min
  gvt_resolver.io.queue_min.valid := true.B

  gvt_resolver.io.last_processed_ts.zip(cores).foreach{case(t, c) => t := c.io.last_proc_ts}

  when(gvt_resolver.io.gvt.valid){
    gvt := gvt_resolver.io.gvt.bits
  }

  /* Control states */
  io.done.valid := false.B
  io.done.bits := gvt

  evt_mgr.io.init.noenq()

  val sIDLE::sINIT::sRUNNING::sEND::Nil = Enum(4)
  val state = RegInit(sIDLE)

  gvt_resolver.io.compute := state === sRUNNING

  val force_sync = RegInit(false.B)
  controller.io.force_sync := force_sync

  assert(Specs.hist_size >= 8, "History size set too low. Set greater that 8")
  val hist_afull_size = Specs.hist_size/2 + Specs.hist_size/4
  val hist_afull = cores.map(_.io.finished)
    .map(x => Mux(x.fire(), x.bits.hist_size >= hist_afull_size.U, false.B))
    .reduce(_ | _)

  switch(state){
    is(sIDLE){
      when(io.start) {
        state := sINIT
        printf(">>> Initializing simulator <<<\n")
      }
    }
    is(sINIT){
      evt_mgr.io.init.enq(true.B)
      when(evt_mgr.io.init.ready){
        printf(">>> Initialization complete: Run simulation <<<\n")
        state := sRUNNING
//        global_start := true.B
      }
    }
    is(sRUNNING){
//      global_start := false.B
      when(gvt > io.target_gvt){
        state := sEND
      }
      when(force_sync){
        when(!Cat(cores.map(_.io.processing)).orR()){ // All cores stopped processing, sync done
          /* DEBUG */ printf("Synchronization done\n")
          force_sync := false.B
          gvt := queue_min
        }
      }.otherwise{ // Force a synchronization when history size is too large
        force_sync := hist_afull
        /* DEBUG */ when(!force_sync && hist_afull){printf("Forcing synchronization\n")}
      }
    }
    is(sEND){
      when(!Cat(cores.map(_.io.processing)).orR()) {
        state := sIDLE
        printf(">>> Reached end of simulation <<<\n")
        io.done.valid := true.B
      }
    }
  }

  cores.foreach(_.io.run := state === sRUNNING)

  /* Debug connections */
  def makeDbgIO[T <: Data](in: DecoupledIO[T]) : ValidIO[T] = {
    val v = Wire(Valid(in.bits.cloneType))
    v.valid := in.fire()
    v.bits := in.bits
    v
  }
  def makeDbgIO[T <: Data](in: ValidIO[T]) : ValidIO[T] = {
    val v = Wire(Valid(in.bits.cloneType))
    v <> in
    v
  }

  io.dbg.enqueue.zip(evt_mgr.io.in).foreach(w => w._1 := makeDbgIO(w._2))
  io.dbg.issue.zip(evt_mgr.io.out).foreach(w => w._1 := makeDbgIO(w._2))
  io.dbg.start.zip(controller.io.start_sig).foreach(w => w._1 := makeDbgIO(w._2))
  io.dbg.gen.zip(cores.map(_.io.generated_evt)).foreach(w => w._1 := makeDbgIO(w._2))
  io.dbg.finish.zip(cores.map(_.io.finished)).foreach(w => w._1 := makeDbgIO(w._2))
  io.dbg.gvt := gvt
  io.dbg.core_gvt.zip(cores.map(_.io.dbg.core_gvt)).foreach(x => x._1 := x._2)
  io.dbg.active_lp.zip(cores.map(_.io.dbg.active_lp)).foreach(x=> x._1 := x._2)
}

class DebugSignalBundle extends Bundle{
  val enqueue = Vec(Specs.num_queues, Valid(new EventDispatchBundle))
  val issue = Vec(Specs.num_queues, Valid(new EventDispatchBundle))
  val start = Vec(Specs.num_queues, Valid(new StartMsg))
  val gen = Vec(Specs.num_cores, Valid(new EventDispatchBundle))
  val finish = Vec(Specs.num_cores, Valid(new CoreFinishedSignal))
  val gvt = Output(UInt(Specs.time_bits.W))
  val core_gvt = Output(Vec(Specs.num_cores, UInt(Specs.time_bits.W)))
  val active_lp = Output(Vec(Specs.num_cores, UInt((Specs.lp_bits+1).W)))
}

object PDESA extends App {
  val options = Array(
    "--split-modules",
//    "--log-level", "Info",
    "--target-dir", "verilog"
  )
  chisel3.Driver.execute(options, () => {
    new PDESA
  })
}