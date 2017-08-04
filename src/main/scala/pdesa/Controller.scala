package pdesa

import chisel3._
import chisel3.util._

class Controller extends Module {
  val io = IO(new Bundle {
    val evt_msg = Flipped(Vec(Specs.num_queues, Decoupled(new EventDispatchBundle)))
    val out_msg = Vec(Specs.num_queues, Decoupled(new EventDispatchBundle))

    val core_done = Flipped(Vec(Specs.num_cores, Valid(UInt(log2Ceil(Specs.hist_size).W))))
    val stall_release = Vec(Specs.num_cores, Valid(UInt(log2Ceil(Specs.hist_size).W)))

    val queue_min = Flipped(Vec(Specs.num_queues, Valid(Specs.time_bits)))
    val gvt = Output(UInt(Specs.time_bits.W))
  })

  /* Keep a record of finished cores, clear record when queued for de-stalling */
  val done_valid = Seq.fill(Specs.num_cores)(RegInit(false.B))
  val done_bits = Seq.fill(Specs.num_cores)(Reg(io.core_done.head.bits.cloneType))
  val done_reset = Seq.fill(Specs.num_cores)(Wire(Bool()))

  for (i <- 0 until Specs.num_cores) {
    when(io.core_done(i).valid) {
      done_valid(i) := true.B
      done_bits(i) := io.core_done(i).bits
    }
    when(done_reset(i)) {
      done_valid(i) := false.B
    }
  }

  /* Send requests for de-stalling for finished cores based on there current LP */
  val done_arb = Module(new RRArbiter(io.core_done.head.bits.cloneType, Specs.num_cores))
  for (i <- 0 until Specs.num_cores) {
    done_arb.io.in(i).valid := done_valid(i)
    done_arb.io.in(i).valid := done_bits(i)
  }

  val done_core = RegNext(done_arb.io.chosen)
  val done_hist_cnt = RegNext(done_arb.io.out.bits)
  val do_destall = RegNext(done_arb.io.out.valid)
  done_arb.io.out.deq()

  /* Keep record of history count for LPs */
  val num_lp_per_queue = Specs.num_lp / Specs.num_queues
  val hist_count = Seq.fill(Specs.num_queues)(Mem(UInt(log2Ceil(Specs.hist_size).W), num_lp_per_queue))

  /* Check each LP group for activeness of this core
   * Update corresponding hist table and send de-stall signal */
  val destall = Seq.fill(Specs.num_queues)(Wire(Bool()))
  val done_lp = Seq.fill(Specs.num_queues)(Wire(UInt(Specs.lp_bits.W)))
  for (n <- 0 until Specs.num_queues) {
    when(active_cores(n)(done_core)) {
      done_lp(n) := core_assoc_table(n)(done_core)
      hist_count(n).write(done_lp, done_hist_cnt)
      destall(n) := true.B
    }
  }

  // Keep track of active cores
  //  val active_cores = RegInit(Vec(Seq.fill(Specs.num_cores)(false.B)))
  val active_cores = Seq.fill(Specs.num_queues)(RegInit(UInt(Specs.num_cores.W), 0.U))
  for (n <- 0 until Specs.num_queues) {
    // remove activeness when a core is done
    val next_active_reset_mask: UInt = UIntToOH(done_core, Specs.num_cores)
    val next_active_set_masks: UInt = Mux(io.evt_msg(n).fire(), UIntToOH(io.evt_msg(n).bits.tag), 0.U)
    // BitwiseOR the masks
    active_cores(n) := ((~next_active_reset_mask).asUInt & active_cores(n)) | next_active_set_masks
  }


  /* Keep record of events sent to the cores.
   * Since LPs are grouped, keep one set of record for each group and
   * update them for event issued from a particular LP group. */
  val active_lp_id: IndexedSeq[UInt] = io.evt_msg.map(_.bits.msg.lp_id)
  val active_time: IndexedSeq[UInt] = io.evt_msg.map(_.bits.msg.time)

  val core_assoc_table: Seq[Vec[UInt]] = Seq.fill(Specs.num_queues) {
    Reg(Vec(Seq.fill(Specs.num_cores)(0.U(Specs.lp_bits.W)))) // Store occupying LP id for each core
  }
  val core_time_table: Seq[Vec[UInt]] = Seq.fill(Specs.num_queues) {
    Reg(Vec(Seq.fill(Specs.num_cores)(0.U(Specs.time_bits.W)))) // Store occupying time for each core
  }

  for (i <- 0 until Specs.num_queues) {
    when(io.evt_msg(i).fire()) {
      core_assoc_table(i)(io.evt_msg(i).bits.tag) := active_lp_id(i)
      core_time_table(i)(io.evt_msg(i).bits.tag) := active_time(i)
    }
  }

  /* Find out stall-release target for issued event */
  val release_valid = Seq.fill(Specs.num_queues)(Reg(UInt(Specs.num_cores.W)))
  val release_idx = Seq.fill(Specs.num_queues)(Reg(UInt(Specs.core_bits.W)))
  val release_hist_cnt = Seq.fill(Specs.num_queues)(Reg(UInt(log2Ceil(Specs.hist_size).W)))
  for (n <- 0 until Specs.num_queues) {
    val active_targets = Wire(UInt(Specs.num_cores.W))
    when(destall(n)) {
      /* When destalling due to processor being finished is present
       * it gets priority and event issue is blocked */
      val tgt = active_cores(n).toBools().zip(core_assoc_table(n))
        .map(x => x._1 & (x._2 === done_lp(n)))
      active_targets := (~UIntToOH(done_core)).asUInt & Vec(tgt).asUInt

      io.evt_msg(n).nodeq
    }.elsewhen(io.evt_msg(n).valid) {
      val msg = io.evt_msg(n).bits
      active_targets := Vec(active_cores(n).toBools().zip(core_assoc_table(n))
        .map(x => x._1 & (x._2 === active_lp_id(n)))).asUInt

      io.out_msg(n).enq(io.evt_msg(n).deq)
    }

    val indices = 0 until Specs.num_queues map (_.U(Specs.core_bits.W))
    val release_tgt_idx_with_valid = (active_targets.toBools(), core_time_table(n), indices).zipped.toList
      .reduce((a, b) => {
        val c = a._1 | b._1
        val t = Mux(Cat(~a._1, a._2) > Cat(~b._1, b._2), b._2, a._2)
        val i = Mux(Cat(~a._1, a._2) > Cat(~b._1, b._2), b._3, a._3)
        (c, t, i)
      })

    release_valid(n) := Mux(release_tgt_idx_with_valid._1, UIntToOH(release_tgt_idx_with_valid._3), 0.U)
//    release_idx(n) := release_tgt_idx_with_valid._3
    release_hist_cnt(n) := hist_count(n).read(active_lp_id(n))
  }

  for(i<- 0 until Specs.num_cores){
    io.stall_release(i).valid := Vec(release_valid.map(v => v(i))).asUInt().orR()
    io.stall_release(i).data := Mux1H(release_valid.map(v => v(i)), release_hist_cnt)
  }

  // TODO: Send request for event to queue
  // TODO: Pass EventManager output through controller before sending to arbiter

  /* find out the minimum timestamp among active cores and queues */
  

}

object Controller extends App {
  chisel3.Driver.execute(args, () => {
    new Controller
  })
}