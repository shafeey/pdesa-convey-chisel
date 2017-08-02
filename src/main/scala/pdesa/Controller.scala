package pdesa

import chisel3._
import chisel3.util._

class Controller extends Module {
  val io = IO(new Bundle {
    val evt_msg = Flipped(Vec(Specs.num_queues, Decoupled(new EventDispatchBundle)))

    val core_done = Flipped(Vec(Specs.num_cores, Valid(UInt(log2Ceil(Specs.hist_size).W))))
    val stall_release = Vec(Specs.num_cores, Valid(UInt(log2Ceil(Specs.hist_size).W)))

    val gvt = Output(UInt(Specs.time_bits.W))
  })

  // Keep track of active cores
  //  val active_cores = RegInit(Vec(Seq.fill(Specs.num_cores)(false.B)))
  val active_cores = RegInit(UInt(Specs.num_cores.W), 0.U)
  // remove activeness when a core is done
  val next_active_reset_mask = Wire(active_cores)
  val next_active_set_masks = Seq.fill(Specs.num_queues)(Wire(active_cores))
  for (i <- 0 until Specs.num_cores) {
    /* Mask bit set if core has finished */
    next_active_reset_mask(i) := io.core_done(i).valid
    /* Mask bit set if core is going to receive an event */
    for (j <- 0 until Specs.num_queues) {
      next_active_set_masks(j)(i) := (io.evt_msg(j).fire() && io.evt_msg(j).bits.tag === i.U)
    }
  }
  // BitwiseOR the masks
  active_cores := ((~next_active_reset_mask) & active_cores) | next_active_set_masks.reduce(_ | _)

  /* Keep record of events sent to the cores.
   * Since LPs are grouped, keep one set of record for each group and
   * update them for event issued from a particular LP group. */
  val current_LP_in_core = Seq.fill(Specs.num_queues)(Reg(Vec(Seq.fill(Specs.num_cores)(UInt(Specs.lp_bits.W)))))
  for(i<- 0 until Specs.num_queues){
    if()
  }


  // Find minimum time of active cores+queues to compute GVt

  // Receive event completion message from cores
  // Send stall release signal to cores


}
