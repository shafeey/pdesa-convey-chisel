package pdesa

import chisel3._
import chisel3.util._

//noinspection ScalaStyle
object Specs {
  val num_cores = 64
  val num_lp = 512
  val num_events = 1000
  val time_bits = 16

  val num_core_grp = 8
  val num_queues = 4
  val queue_size = 255

  val hist_size = 16


  def lp_bits = log2Ceil(num_lp)

  def core_bits = log2Ceil(num_cores)
}

class PDESA extends Module {
  val io = IO(new Bundle {
    val target_gvt = Input(UInt(Specs.time_bits.W))
    val done = Output(Bool())
  })

  // Instantiate cores
  val cores = for (i <- 0 until Specs.num_cores) yield {
    Module(new PDESACore(i, Specs.lp_bits, Specs.time_bits))
  }

  val gen_xbar = Module(new Crossbar(cores(0).io.generated_evt.cloneType, Specs.num_core_grp, Specs.num_queues))

  val gen_arbs = for (i <- 0 until Specs.num_core_grp) yield {
    Module(new RRArbiter(cores(0).io.generated_evt.cloneType, Specs.num_cores / Specs.num_core_grp))
  }

  val evt_mgr = Module(new EventManager(Specs.num_queues))


  /* Deliver event from core to event queue through arbiter and crossbar */
  for (i <- 0 until Specs.num_core_grp) {
    for (j <- 0 until Specs.num_cores / Specs.num_core_grp) {
      // cores to arbiters
      cores(i * Specs.num_cores / Specs.num_core_grp + j).io.generated_evt <> gen_arbs(i).io.in(j)
    }
    gen_arbs(i).io.out <> gen_xbar.io.mi(i) // arbiter to xbar input
  }

  for (i <- 0 to Specs.num_queues) {
    gen_xbar.io.si(i) <> evt_mgr.io.in(i) // Crossbar to event manager input
  }

  /* Returns acknowledgement from Event Manager to the originating core */


  /* Deliver event from event queue to cores through a crossbar and then broadcast */
  def evtIssueFilter(id: Int, in: DecoupledIO[EventDispatchBundle]): Valid[EventMsg] = {
    val out = Valid(new EventMsg(Specs.lp_bits, Specs.time_bits))
    out.valid := Mux(in.bits.tag === id.U, true.B, false.B)
    out.bits := in.bits.msg
    out
  }

  val issue_xbar = Module(new Crossbar(new EventDispatchBundle, Specs.num_queues, Specs.num_core_grp))

  for (i <- 0 until Specs.num_cores) {
    cores(i).io.issued_evt := evtIssueFilter(i, issue_xbar.io.si(i/Specs.num_core_grp))
  }
  for(i<- 0 until Specs.num_queues){
    issue_xbar.io.mi(i) <> evt_mgr.io.out(i)
  }
  for(i<- 0 until Specs.num_core_grp){
    issue_xbar.io.si(i).ready := true.B // Convert DecoupledIO to ValidIO
  }

  /* Connect cores to event history */
  val evt_hist_mgr = Module(new EventHistoryManager)
  val hist_arb = Module(new RRArbiter(evt_hist_mgr.io.hist_req.bits.cloneType, Specs.num_cores))
  for(i<- 0 until Specs.num_cores){
    cores(i).io.hist_req <> hist_arb.io.in(i)
  }
  hist_arb.io.out <> evt_hist_mgr.io.hist_req
}


