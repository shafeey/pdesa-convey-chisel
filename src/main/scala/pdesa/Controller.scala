package pdesa

import chisel3._
import chisel3.util._

class ConrollerIO extends Bundle {
  val evt_msg = Flipped(Vec(Specs.num_queues, Decoupled(new EventDispatchBundle)))
  val out_msg = Vec(Specs.num_queues, Decoupled(new EventDispatchBundle))

  val core_done = Flipped(Vec(Specs.num_cores, Valid(UInt(log2Ceil(Specs.hist_size).W))))
  val stall_release = Vec(Specs.num_cores, Valid(UInt(log2Ceil(Specs.hist_size).W)))

  val queue_min = Flipped(Vec(Specs.num_queues, Valid(UInt(Specs.time_bits.W))))
  val gvt = Output(UInt(Specs.time_bits.W))
}

class DoneCoresHandler extends Module{
  class DoneCoreInfo extends Bundle{
    val hist_cnt = Output(UInt(log2Ceil(Specs.hist_size).W))
    val core_id = Output(UInt(log2Ceil(Specs.core_bits).W))
  }

  val io = IO(new Bundle {
    val core_done = Flipped(Vec(Specs.num_cores, Valid(UInt(log2Ceil(Specs.hist_size).W))))
    val out = Decoupled(new DoneCoreInfo)
  })

  /* Keep a record of finished cores, clear record when queued for de-stalling */
  private val done_valid = RegInit(Vec(Seq.fill(Specs.num_cores)(false.B)))
  private val done_bits = Reg(Vec(Seq.fill(Specs.num_cores)(io.core_done.head.bits.cloneType)))

  private val arb = Module(new RRArbiter(io.core_done.head.bits.cloneType, Specs.num_cores))

  for (i <- 0 until Specs.num_cores) {
    when(io.core_done(i).valid) {
      done_valid(i) := true.B
      done_bits(i) := io.core_done(i).bits
    }
    when(arb.io.in(i).fire()) {
      done_valid(i) := false.B
    }
  }

  for (i <- 0 until Specs.num_cores) {
    arb.io.in(i).valid := done_valid(i)
    arb.io.in(i).bits := done_bits(i)
  }

  val done_buffer = Module(new Queue(new DoneCoreInfo, 1, pipe = true))
  done_buffer.io.enq.bits.core_id := arb.io.chosen
  done_buffer.io.enq.bits.hist_cnt := arb.io.out.bits
  done_buffer.io.enq.valid := arb.io.out.valid
  arb.io.out.ready := done_buffer.io.enq.ready

  io.out <> done_buffer.io.deq
}

class Controller extends Module {
  val io = IO(new ConrollerIO)




//  /* Keep a record of finished cores, clear record when queued for de-stalling */
//  private val done_valid = Seq.fill(Specs.num_cores)(RegInit(false.B))
//  private val done_bits = Seq.fill(Specs.num_cores)(Reg(io.core_done.head.bits.cloneType))
//  val done_reset = Vec(Specs.num_cores,Wire(Bool()))
//
//  for (i <- 0 until Specs.num_cores) {
//    when(io.core_done(i).valid) {
//      done_valid(i) := true.B
//      done_bits(i) := io.core_done(i).bits
//    }
//    when(done_reset(i)) {
//      done_valid(i) := false.B
//    }
//  }
//
//  /* Send requests for de-stalling for finished cores based on there current LP */
//  val done_arb = Module(new RRArbiter(io.core_done.head.bits.cloneType, Specs.num_cores))
//  for (i <- 0 until Specs.num_cores) {
//    done_arb.io.in(i).valid := done_valid(i)
//    done_arb.io.in(i).valid := done_bits(i)
//  }
//
//  val done_core = RegNext(done_arb.io.chosen)
//  val done_hist_cnt = RegNext(done_arb.io.out.bits)
//  val do_destall = RegNext(done_arb.io.out.valid)
//  done_arb.io.out.deq()
  /* Handle processing finished signals from cores */
  val finished_cores_handler = Module(new DoneCoresHandler)
  finished_cores_handler.io.core_done <> io.core_done
  private val finished = finished_cores_handler.io.out

  /* Every action is processed depending on the scenarios:
   * An issue or finished signal may arrive at the same time.
   * Event issue is handled first, and then finished signals.
   */

  /* Maintain and update states */

  // Keep record of history count for LPs
  private val num_lp_per_queue = Specs.num_lp / Specs.num_queues
  private val hist_count = Seq.fill(Specs.num_queues)(Mem(UInt(log2Ceil(Specs.hist_size).W), num_lp_per_queue))

  // Keep track of active cores
  val active_cores = Seq.fill(Specs.num_queues)(RegInit(UInt(Specs.num_cores.W), 0.U))
  for (n <- 0 until Specs.num_queues) {
    // remove activeness when a core is finished, but no event issue in this cycle
    when(io.evt_msg(n).valid){
      val next_active_reset_mask: UInt = UIntToOH(done_core, Specs.num_cores)
      val next_active_set_masks: UInt = Mux(io.evt_msg(n).fire(), UIntToOH(io.evt_msg(n).bits.tag), 0.U)
      // BitwiseOR the masks
      active_cores(n) := ((~next_active_reset_mask).asUInt & active_cores(n)) | next_active_set_masks
    }
  }

  // Check each LP group for activeness of this core
  // Update corresponding hist table and send de-stall signal
  val destall = Seq.fill(Specs.num_queues)(Wire(Bool()))
  val done_lp = Seq.fill(Specs.num_queues)(Wire(UInt(Specs.lp_bits.W)))
  for (n <- 0 until Specs.num_queues) {
    when(active_cores(n)(done_core)) {
      done_lp(n) := core_assoc_table(n)(done_core)
      hist_count(n).write(done_lp(n), done_hist_cnt)
      destall(n) := true.B
    }
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
  private val release_valid = Seq.fill(Specs.num_queues)(Reg(UInt(Specs.num_cores.W)))
  private val release_hist_cnt = Seq.fill(Specs.num_queues)(Reg(UInt(log2Ceil(Specs.hist_size).W)))

  for (n <- 0 until Specs.num_queues) {
    io.out_msg(n).noenq()
    io.evt_msg(n).nodeq()
    val active_targets = Wire(UInt(Specs.num_cores.W))
    when(io.evt_msg(n).valid) {
      /* When event issued from queue, it gets priority
       * and destalling due to processor being finished is blocked */
      val msg = io.evt_msg(n).bits
      active_targets := Vec(active_cores(n).toBools().zip(core_assoc_table(n))
        .map(x => x._1 & (x._2 === active_lp_id(n)))).asUInt
      io.out_msg(n).enq(io.evt_msg(n).deq)
    }.elsewhen(destall(n)) {
      val tgt = active_cores(n).toBools().zip(core_assoc_table(n))
        .map(x => x._1 & (x._2 === done_lp(n)))
      active_targets := (~UIntToOH(done_core)).asUInt & Vec(tgt).asUInt
      done_reset(done_core) := true.B
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
    release_hist_cnt(n) := hist_count(n).read(active_lp_id(n))
  }

  for (i <- 0 until Specs.num_cores) {
    io.stall_release(i).valid := Vec(release_valid.map(v => v(i))).asUInt().orR()
    io.stall_release(i).data := Mux1H(release_valid.map(v => v(i)), release_hist_cnt)
  }

  // TODO: Send request for event to queue
  // TODO: Pass EventManager output through controller before sending to arbiter

  /* find out the minimum timestamp among active cores and queues */
  val core_min: UInt = CoreMinResolver(active_cores, core_time_table)
  val queue_min: UInt = io.queue_min map (x => Cat(~x.valid, x.bits)) reduce ((a, b) => Mux(a < b, a, b))
  val gvt = RegNext(Mux(core_min < queue_min, core_min, queue_min))
  io.gvt := gvt
}

protected class CoreMinResolver extends Module {
  val io = IO(new Bundle {
    val active_cores = Seq.fill(Specs.num_queues)(Input(UInt(Specs.num_cores.W)))
    val core_times = Seq.fill(Specs.num_queues)(Input(Vec(Specs.num_cores, UInt(Specs.time_bits.W))))

    val min = Output(UInt(Specs.time_bits.W))
    val vld = Output(Bool())
  })
  val num_segments: Int = 4
  private val num_cores_per_segment = Specs.num_cores / num_segments

  val min_times = Seq.fill(Specs.num_queues)(Wire(UInt(Specs.time_bits.W)))
  val min_vlds = Seq.fill(Specs.num_queues)(Wire(Bool()))

  val min_time_ctr = RegInit(0.U(log2Ceil(num_cores_per_segment + 1).W))
  min_time_ctr := Mux(min_time_ctr === num_cores_per_segment.U, 0.U, min_time_ctr + 1.U)

  for (q <- 0 until Specs.num_queues) {
    val min_seg_times = Seq.fill(num_segments)(Wire(UInt(Specs.time_bits.W)))
    val min_seg_vld = Seq.fill(num_segments)(Wire(Bool()))
    for (g <- 0 until num_segments) {
      val min_core_time1 = RegInit(-1.U(Specs.time_bits.W))
      val min_core_time2 = RegInit(-1.U(Specs.time_bits.W))

      val vld1 = RegInit(false.B)
      val vld2 = RegInit(false.B)


      when(min_time_ctr === num_cores_per_segment.U) {
        min_core_time2 := min_core_time1
        min_core_time1 := -1.U
        vld2 := vld1
        vld1 := false.B
      }.otherwise {
        val core_index = Cat(g.U, min_time_ctr)
        when(io.active_cores(q)(core_index)) {
          when(min_core_time1 > io.core_times(q)(core_index)) {
            min_core_time1 := io.core_times(q)(core_index)
          }
          when(min_core_time2 > io.core_times(q)(core_index)) {
            min_core_time2 := io.core_times(q)(core_index)
          }
          vld1 := true.B
          vld2 := true.B
        }
      }

      min_seg_times(g) := min_core_time2
      min_seg_vld(g) := vld2
    }

    /* Find minimum among the segments */
    val t = min_seg_times.zip(min_seg_vld).reduce((a, b) => {
      val tmp = Mux(Cat(a._2, a._1) < Cat(b._2, b._1), Cat(a._2, a._1), Cat(b._2, b._1))
      (tmp(Specs.time_bits - 1, 0), tmp(Specs.time_bits))
    })
    min_times(q) := t._1
    min_vlds(q) := t._2
  }

  /* Find Minimum among the queue groups */
  val min = RegInit(0.U(Specs.time_bits.W))
  val vld = RegInit(false.B)

  when(min_time_ctr === num_cores_per_segment.U) {
    val t = min_times.zip(min_vlds).reduce((a, b) => {
      val tmp = Mux(Cat(a._2, a._1) < Cat(b._2, b._1), Cat(a._2, a._1), Cat(b._2, b._1))
      (tmp(Specs.time_bits - 1, 0), tmp(Specs.time_bits))
    })
    min := t._1
    vld := t._2
  }

  io.min := min
  io.vld := vld
}
protected object CoreMinResolver {
  def apply(active_cores: Seq[UInt], core_times: Seq[Vec[UInt]]): UInt = {
    val core_min_res = Module(new CoreMinResolver)
    core_min_res.io.active_cores zip active_cores foreach { case (sink, src) => sink := src }
    core_min_res.io.core_times zip core_times foreach { case (d, s) =>
      d zip s foreach { case (sink, src) => sink := src }
    }
    val out = Cat(~core_min_res.io.vld, core_min_res.io.min)
    out
  }
}

object Controller extends App {
  chisel3.Driver.execute(args, () => {
    new Controller
  })
}