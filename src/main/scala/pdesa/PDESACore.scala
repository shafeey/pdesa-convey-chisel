package pdesa

import chisel3._
import chisel3.util._

// TODO: Implement logic to account for ack before quitting
// TODO: Drive last processed timestamp for GVT computation
class PDESACore(core_id: Int, lp_bits: Int, time_bits: Int) extends Module {
  val io = IO(new Bundle {
    // Event exchange conversations
    // TODO: Update test - change of interface
    val generated_evt = Decoupled(new EventDispatchBundle)
    val issued_evt = Flipped(Valid(new EventMsg(lp_bits, time_bits)))
    val dispatch_ack = Flipped(Valid(new EventAckMsg))

    // Command interface
    val start = Flipped(Valid(new StartMsg)) // Also sends the history data count
    val req_evt = Valid(Bool())
    val finished = Decoupled(new CoreFinishedSignal)

    val run = Input(Bool())
    val processing = Output(Bool())

    // GVT update
    val last_proc_ts = Output(UInt(time_bits.W))
    val gvt = Input(UInt(time_bits.W))

    // Memory Access Interface
    // TODO: Memory Access

    // Event History interface
    // TODO: Event history
    val hist_req = Decoupled(new EventHistoryReq(lp_bits, time_bits))
    val hist_rsp = Flipped(Valid(new EventHistoryRsp(lp_bits, time_bits)))

    val dbg = new Bundle{
      val core_gvt = Output(UInt(Specs.time_bits.W))
      val active_lp = Output(UInt((Specs.lp_bits+1).W))
    }
  })

  /* State Machine */
  val sIDLE :: sSTALL :: sHIST_RD :: sLD_MEM :: sLD_RTN :: sPROC_DELAY :: sGEN_EVT :: sHIST_WR :: sST_MEM :: sST_RTN :: sFINALISE :: sWAIT :: sRESTART :: sNil = Enum(13)
  val state = RegInit(sIDLE)
  io.processing := (state =/= sIDLE ) && (state =/= sSTALL)

  // IDLE state task processing
  val event_data = Reg(io.issued_evt.bits.cloneType)
  val event_valid = RegInit(false.B)
  val gvt = RegInit(io.gvt.cloneType, init = 0.U)
//  val event_requested = RegInit(false.B)

  io.req_evt.valid := false.B
  val r_last_processed_ts = RegInit(0.U(Specs.time_bits.W))
  io.last_proc_ts := r_last_processed_ts

  def IDLE_task = {
//    when(!event_requested) {
//      event_requested := true.B
//      io.req_evt.valid := true.B
//      stalled := true.B
//    }
    when(io.issued_evt.valid) {
      /* DEBUG */
      val dbg_ne = io.issued_evt.bits
      when(dbg_ne.cancel_evt) {
        printf("** EP %d # Cancellation event at time: %d, LP: %d --> GVT: %d\n", core_id.U, dbg_ne.time, dbg_ne.lp_id, io.gvt)
      }.otherwise {
        printf("** EP %d # Received event at time: %d, LP: %d --> GVT: %d\n", core_id.U, dbg_ne.time, dbg_ne.lp_id, io.gvt)
      } /* DEBUG */

      event_data := io.issued_evt.bits
      event_valid := true.B
      gvt := io.gvt
      state := sSTALL
      r_last_processed_ts := io.issued_evt.bits.time
//      event_requested := false.B // Clear flag for next iteration
    }
  }

  // Stall logic
  val stalled = RegInit(true.B)
  val hist_cnt = RegInit(0.U(log2Ceil(Specs.hist_size).W))
  when(io.start.valid) {
    hist_cnt := io.start.bits.hist_size
    stalled := false.B
  }

  def STALL_task = {
    when(!stalled && io.run) {
      printf("** EP %d # Start execution - time: %d, LP: %d\n", core_id.U, event_data.time, event_data.lp_id)
      state := sHIST_RD
    }
  }

  // Event hist read
  io.hist_req.noenq()
  def HIST_RD_task = {
    when(hist_cnt =/= 0.U) {
      io.hist_req.valid := true.B
      io.hist_req.bits.setRead(core_id.U, event_data.lp_id, hist_cnt)
      when(io.hist_req.fire) {
        printf("++> EP %d # Request for history. Count: %d, LP: %d\n", core_id.U, hist_cnt, event_data.lp_id)
        state := sLD_MEM
      }
    }.otherwise(state := sLD_MEM)
  }

  // event history receive queue
  val hist_queue_enq = Wire(EnqIO(new EventHistoryMsg(lp_bits, time_bits)))
  val hist_data_valid = io.hist_rsp.valid && io.hist_rsp.bits.EP_id === core_id.U &&
    io.hist_rsp.bits.op === EventHistroyCmnd.sOP_RD && io.hist_rsp.bits.msg.origin_time >= gvt
  hist_queue_enq.valid := hist_data_valid
  hist_queue_enq.bits := io.hist_rsp.bits.msg
  val hist_queue = Queue(hist_queue_enq, Specs.hist_size)
  // TODO: Sizes can be adjusted
  val hist_recv_cnt = RegInit(0.U(log2Ceil(Specs.hist_size).W))
  when(hist_queue_enq.fire) {
    printf("<++ EP %d # Received history. Count: %d, LP: %d (", core_id.U, hist_recv_cnt + 1.U, event_data.lp_id)
    when(hist_queue_enq.bits.cancel_evt) {
      printf("Cancel at time: %d)\n", hist_queue_enq.bits.origin_time)
    }.otherwise {
      printf("-->%d, time: %d-->%d)\n", hist_queue_enq.bits.target_lp, hist_queue_enq.bits.origin_time, hist_queue_enq.bits.target_time)
    }

    hist_recv_cnt := hist_recv_cnt + 1.U
  }

  val hist_received = hist_recv_cnt === hist_cnt

  // Event history filtering
  val filt_enq = Wire(EnqIO(new EventHistoryMsg(lp_bits, time_bits)))
  val filt_queue = Queue(filt_enq, Specs.hist_size * 2)
  val match_found = RegInit(false.B)
  val match_data = Reg(new EventHistoryMsg(lp_bits, time_bits))
  val need_cancellation = RegInit(false.B)

  when(state === sIDLE) {
    match_found := false.B
    need_cancellation := false.B
  }

  hist_queue.nodeq()
  filt_enq.noenq()
  when(hist_queue.valid && filt_enq.ready) {
    when(hist_queue.bits.cancel_evt =/= event_data.cancel_evt &&
      hist_queue.bits.origin_time === event_data.time && !match_found) {
      /* Event in stack and the current event are cancellation for each other */
      /* Don't put back the entry to history again.
       * Set a flag to indicate the match is found, to prevent cancellation twice.
       * The current event will not go into history after being processed.
       */
      match_found := true.B
      match_data := hist_queue.deq() // Save and discard this history entry
      when(event_data.cancel_evt === true.B) {
        need_cancellation := true.B
      }
    }.otherwise {
      filt_enq.enq(hist_queue.deq())
    }
  }

  // Memory access
  // TODO: Implement memory access later. Simulate by a delay only
  val mem_delay_counter = Reg(63.U.cloneType)
  when(state === sIDLE) {
    mem_delay_counter := 0.U
  }
    .elsewhen(state === sLD_MEM) {
      mem_delay_counter := mem_delay_counter + 1.U
    }

  def MEM_LD_task = {
    when(mem_delay_counter === 32.U) {
      state := sGEN_EVT
    }
  }

  // Processing delay
  val delay_counter = RegInit(255.U.cloneType, init = 0.U)

  when(state === sIDLE) {
    delay_counter := 0.U
  }

  def PROC_DELAY_task = {
    delay_counter := delay_counter + 1.U
    when(delay_counter === 100.U) {
      state := sGEN_EVT
    }
  }


  // Event Generation
  val rand_lp = LFSR(seed = 15821 ^ core_id)
  val rand_offset = LFSR(seed = 25879 ^ core_id)

  val evt_out_q = Module(new Queue(Wire(new EventMsg(lp_bits, time_bits)), Specs.hist_size))
  evt_out_q.io.enq.noenq()

  // TODO: Update test for change in interface
  io.generated_evt.bits.tag := core_id.U
  io.generated_evt.bits.msg := evt_out_q.io.deq.bits
  io.generated_evt.valid := evt_out_q.io.deq.valid
  evt_out_q.io.deq.ready := io.generated_evt.ready


  filt_queue.nodeq()

  val evt_generated = RegInit(0.U(log2Ceil(Specs.hist_size).W))
  when(state === sIDLE){evt_generated := 0.U}
    .elsewhen(evt_out_q.io.enq.fire()){evt_generated := evt_generated + 1.U}

  val evt_ack_counter = RegInit(0.U(log2Ceil(Specs.hist_size).W))
  when(state === sIDLE){evt_ack_counter := 0.U}
    .elsewhen(io.dispatch_ack.valid){evt_ack_counter := evt_ack_counter + 1.U}

  //  when(state === sGEN_EVT){
  def GEN_task = {
    when(!hist_queue.valid) {
      /* Wait until all history events have been processed */
      when(match_found) {
        when(need_cancellation) {
          // Enqueue cancellation msg
          evt_out_q.io.enq.bits.time := match_data.target_time
          evt_out_q.io.enq.bits.lp_id := match_data.target_lp
          evt_out_q.io.enq.bits.cancel_evt := true.B
          evt_out_q.io.enq.valid := true.B
          when(evt_out_q.io.enq.fire()) {
            printf("** EP %d # Cancellation(quash) event at time: %d, LP: %d --> GVT: %d\n", core_id.U, match_data.target_time, match_data.target_lp, gvt)
            state := sHIST_WR
          }
        }.otherwise {
          printf("** EP %d # Event quashed. LP: %d\n", core_id.U, event_data.lp_id)
          state := sHIST_WR
        }
        /* If cancellation not needed, no new event shall be generated */
      }.otherwise {
        when(!event_data.cancel_evt) {
          /* Generate a regular new event randomly */
          val gen_evt_time = event_data.time + rand_offset(4, 0) + 20.U
          val gen_evt_lp = rand_lp(Specs.lp_bits - 1, 0)
          val gen_evt_type = false.B
          evt_out_q.io.enq.bits.time := gen_evt_time
          evt_out_q.io.enq.bits.lp_id := gen_evt_lp
          evt_out_q.io.enq.bits.cancel_evt := gen_evt_type
          evt_out_q.io.enq.valid := true.B

          filt_enq.bits.origin_time := event_data.time
          filt_enq.bits.target_time := gen_evt_time
          filt_enq.bits.target_lp := gen_evt_lp
          filt_enq.bits.cancel_evt := gen_evt_type
          filt_enq.valid := true.B
          //TODO: This may be source of future bug since we're not checking ready for filt_queue

          when(evt_out_q.io.enq.fire) {
            printf("** EP %d (LP %d)# Generate event for LP: %d at time: %d\n", core_id.U, event_data.lp_id, evt_out_q.io.enq.bits.lp_id, evt_out_q.io.enq.bits.time)
            state := sHIST_WR
          }
        }.otherwise{ // The cancel events will pushed to history where it waits for matching event to arrive
          filt_enq.bits.origin_time := event_data.time
          filt_enq.bits.target_lp := event_data.lp_id
          filt_enq.bits.cancel_evt := true.B
          filt_enq.valid := true.B
          printf("** EP %d (LP %d)# Pushed cancel event at time: %d\n", core_id.U, event_data.lp_id, event_data.time)
          state := sHIST_WR
        }
      }
    }
  }

  // HIST Write
  /* Create rollback events when some events prematurely processed */
  val hist_write_q = Module(new Queue(new EventHistoryMsg(lp_bits, time_bits), Specs.hist_size))
  hist_write_q.io.enq.noenq()
  hist_write_q.io.deq.nodeq()

  val rollback_not_cancel = Reg(Bool())
  val hist_written = Reg(UInt(log2Ceil(Specs.hist_size).W))
  when(state === sIDLE) {
    rollback_not_cancel := false.B
    hist_written := 0.U
  }

  def HIST_WR_task = {
    when(filt_queue.valid) {
      when(filt_queue.bits.origin_time > event_data.time && !filt_queue.bits.cancel_evt) {
        when(rollback_not_cancel) {
          /* Rollback event */
          evt_out_q.io.enq.bits.setValue(event_data.lp_id, filt_queue.bits.origin_time, false.B)
          evt_out_q.io.enq.valid := true.B
          when(evt_out_q.io.enq.ready) {
            rollback_not_cancel := false.B
            filt_queue.deq() /* Remove entry only once during rollback+cancel generation */
//            printf("EP %d # Rollback event at time: %d, LP: %d\n", core_id.U, filt_queue.bits.origin_time, event_data.lp_id)
          }
        }.otherwise {
          /* Cancel event */
          evt_out_q.io.enq.bits.setValue(filt_queue.bits.target_lp, filt_queue.bits.target_time, true.B)
          evt_out_q.io.enq.valid := true.B
          when(evt_out_q.io.enq.ready) {
            rollback_not_cancel := true.B
//            printf("EP %d # Cancellation event at time: %d, LP: %d\n", core_id.U, filt_queue.bits.target_time, filt_queue.bits.target_lp)
          }
        }
      }.otherwise {
        io.hist_req.valid := true.B
        io.hist_req.bits.setWrite(core_id.U, event_data.lp_id, hist_written, filt_queue.bits)
        when(io.hist_req.ready) {
          filt_queue.ready := true.B
//          printf("EP %d # Write back history. Count: %d--> (%d, %d, %d)\n", core_id.U, hist_written + 1.U, filt_queue.bits.target_lp, filt_queue.bits.target_time, filt_queue.bits.cancel_evt)
          hist_written := hist_written + 1.U
        }
      }
    }.otherwise {
      state := sFINALISE
    }
  }

  // Hist_write success count
  val hist_wr_success_cnt = RegInit(0.U(log2Ceil(Specs.hist_size).W))

  val hist_wr_success = io.hist_rsp.valid && io.hist_rsp.bits.EP_id === core_id.U &&
    io.hist_rsp.bits.op === EventHistroyCmnd.sOP_WR
  when(hist_wr_success) {
    hist_wr_success_cnt := hist_wr_success_cnt + 1.U
  }

  // Finalise
  io.finished.noenq()
  def FINALISE_task = {
    when(hist_wr_success_cnt === hist_written && !evt_out_q.io.deq.valid && evt_generated === evt_ack_counter) {
      io.finished.valid := true.B
      io.finished.bits.setValue(core_id = core_id.U, lp = event_data.lp_id, hist_size = hist_written)
      when(io.finished.fire()) {
        state := sIDLE
        printf("** EP %d # Wrapping up: hist size %d\n", core_id.U, hist_written)
        reinitialize_task
      }
    }
  }


  // RESTART logic
  def reinitialize_task = {
    hist_cnt := 0.U
    stalled := true.B
    hist_recv_cnt := 0.U
    hist_wr_success_cnt := 0.U
    r_last_processed_ts := ~(0.U(Specs.time_bits.W))
  }




  switch(state) {
    is(sIDLE) {
      IDLE_task
    }
    is(sSTALL) {
      STALL_task
    }
    is(sHIST_RD) {
      // Wait for hist read to be finished
      // Go to LD Mem when done
      HIST_RD_task
    }
    is(sLD_MEM) {
      // put memory request out
      // Wait for LD memory finished
      // Go to PROC_DELAY when done
      MEM_LD_task
    }
    is(sPROC_DELAY) {
      // Insert predefined delay
      // Go to GEN_EVT when done
      PROC_DELAY_task
    }
    is(sGEN_EVT) {
      // Generate next events
      // Go to HIST_WR
      GEN_task
    }
    is(sHIST_WR) {
      // Write back history
      // Go to ST_MEM
      HIST_WR_task
    }
    //    is(sST_MEM) {
    // Write back memory
    // GO to SEND_EVT
    //    }
    is(sFINALISE) {
      // SEND out all events
      // SEND evt request when done
      // Become IDLE
      FINALISE_task
    }
  }

  // Debug
  io.dbg.core_gvt := gvt
  io.dbg.active_lp := Cat((state =/= sIDLE).asUInt(), event_data.lp_id)
}

object PDESACore extends App {
  chisel3.Driver.execute(args, () =>
    new PDESACore(core_id = 3, lp_bits = 8, time_bits = 16))
}