package pdesa

import chisel3._
import chisel3.util._
import conveywrapper._

object ModelSpecs{
  val land_delay = 8
  val dep_delay = 16
  val arrival_delay = 32
}

//noinspection ScalaStyle
// TODO: Implement logic to account for ack before quitting
// TODO: Drive last processed timestamp for GVT computation
class PDESACore(core_id: Int, lp_bits: Int, time_bits: Int) extends Module with PlatformParams{
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
    val addr = Input(UInt(MEM_ADDR_WID.W))
    val memPort = new ConveyMemMasterIF(rtnctlWidth)

    // Event History interface
    // TODO: Event history
    val hist_req = Decoupled(new EventHistoryReq(lp_bits, time_bits))
    val hist_rsp = Flipped(Valid(new EventHistoryRsp(lp_bits, time_bits)))

    val conf = new Bundle{
      val proc_delay = Input(UInt(Specs.time_bits.W))
      val num_mem_access = Input(UInt(64.W))
    }

    val report = new Bundle{
      val stalled = Output(Bool())
      val mem = Output(Bool())
    }

    val dbg = new Bundle{
      val core_gvt = Output(UInt(Specs.time_bits.W))
      val active_lp = Output(UInt((Specs.lp_bits+1).W))
    }
  })

  /* random numbers */
  val random_number_generator = LFSR(seed = 35179 ^ core_id)
  val current_rand = RegInit(0.U(random_number_generator.getWidth.W))
  val rand_lp = current_rand
  val rand_offset = RegNext(rand_lp)
  val rand_mem_delay = RegNext(rand_offset)

  /* State Machine */
  val sIDLE :: sSTALL :: sHIST_RD :: sLD_MEM :: sLD_RTN :: sPROC_DELAY :: sROLLBACK :: sGEN_EVT :: sHIST_WR :: sST_MEM :: sST_RTN :: sFINALISE :: sNil = Enum(12)
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

  val rollback_not_cancel = Reg(Bool())
  val state_reversed = Reg(Bool())

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
      state := sSTALL
      r_last_processed_ts := io.issued_evt.bits.time
//      event_requested := false.B // Clear flag for next iteration
      current_rand := random_number_generator
    }
  }

  // Stall logic
  val stalled = RegInit(true.B)
  val hist_cnt = RegInit(0.U(log2Ceil(Specs.hist_size).W))
  when(io.start.valid) {
    hist_cnt := io.start.bits.hist_size
    gvt := io.gvt
    stalled := false.B

    // Rollback logic
    rollback_not_cancel := true.B
    state_reversed := false.B
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
    printf("<++ EP %d # Received history. Count: %d, LP: %d (origin time: %d, cancel: %d)\n",
      core_id.U, hist_recv_cnt + 1.U, event_data.lp_id, hist_queue_enq.bits.origin_time, hist_queue_enq.bits.cancel_evt)
//    when(hist_queue_enq.bits.cancel_evt) {
//      printf("Cancel at time: %d)\n", hist_queue_enq.bits.origin_time)
//    }.otherwise {
//      printf("-->%d, time: %d-->%d)\n", hist_queue_enq.bits.target_lp, hist_queue_enq.bits.origin_time, hist_queue_enq.bits.target_time)
//    }

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

  val rollback_q = Module(new Stack(filt_enq.bits, entries = Specs.hist_size))
  rollback_q.io.enq.noenq()
  rollback_q.io.deq.nodeq()

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
        rollback_q.io.enq.enq(hist_queue.deq())
      }
    }.otherwise {
      when(hist_queue.bits.origin_time > event_data.time && !hist_queue.bits.cancel_evt) {
        rollback_q.io.enq.enq(hist_queue.deq())
        printf("____ Rollback necessary at core %d, lp %d at time %d\n", core_id.U, event_data.lp_id, event_data.time)
        // TODO: put cancel events to rb queue too, then put them in history in correct order. may get lost otherwise
      }.otherwise {
        filt_enq.enq(hist_queue.deq())
      }
    }
  }

  // Memory access
  val mem_delay_target = RegInit(0.U(10.W))
  val mem_delay_counter = RegInit(0.U(10.W))
  when(state === sIDLE) {
    mem_delay_counter := 0.U
    mem_delay_target := 220.U + rand_mem_delay(5,0)
  }
  .elsewhen(state === sLD_MEM) {
    mem_delay_counter := mem_delay_counter + 1.U
  }

  io.memPort.req.noenq()
  io.memPort.req.bits.driveDefaults()

  val lp_state = Reg(new LPState)

  val rtnCtl = Cat(event_data.time, event_data.lp_id, core_id.U(Specs.core_bits.W))
  val req_vaddr = io.addr + (event_data.lp_id << log2Ceil(Specs.NUM_MEM_BYTE)).asUInt()
  def MEM_LD_task = {
    val req = Wire(io.memPort.req.bits.cloneType)
    req.addr := req_vaddr
    req.cmd := MEM_RD_CMD.U
    req.rtnCtl := rtnCtl
    req.size := MEM_SIZE_BYTE.U

    io.memPort.req.enq(req)
    when(io.memPort.req.fire()) {
      state := sLD_RTN
    }
  }

  def LD_RTN_task = {
    when(io.memPort.rsp.valid && io.memPort.rsp.bits.cmd === MEM_RD_DATA.U && io.memPort.rsp.bits.rtnCtl === rtnCtl){
      state := sPROC_DELAY
      when(event_data.time === 0.U){ // the first event, set initial state
        lp_state.furthest_landing_slot := 0.U
        lp_state.waiting_time := 0.U
        lp_state.num_landed := 0.U
      }.otherwise {
        lp_state := io.memPort.rsp.bits.readData.asTypeOf(lp_state.cloneType)
      }
    }
  }

  def MEM_ST_task = {
    val req = Wire(io.memPort.req.bits.cloneType)
    req.addr := req_vaddr
    req.cmd := MEM_WR_CMD.U
    req.rtnCtl := rtnCtl
    req.size := MEM_SIZE_BYTE.U
    req.writeData := lp_state.asUInt()

    io.memPort.req.enq(req)
    when(io.memPort.req.fire()) {
      state := sST_RTN
    }
  }

  def ST_RTN_task = {
    when(io.memPort.rsp.valid && io.memPort.rsp.bits.cmd === MEM_WR_COMPLETE.U && io.memPort.rsp.bits.rtnCtl === rtnCtl){
      state := sFINALISE
    }
  }

  // Processing delay
  val delay_counter = RegInit(255.U.cloneType, init = 0.U)

  when(state === sIDLE) {
    delay_counter := 0.U
  }

  def PROC_DELAY_task = {
    delay_counter := delay_counter + 1.U
    when(delay_counter === io.conf.proc_delay) {
      state := sROLLBACK
    }
  }

  val branch_taken = Reg(new EventHistoryBranchPath)

  def ARRIVAL_task = {
    when(lp_state.furthest_landing_slot < event_data.time){
      branch_taken.landing_slot_open := true.B
      lp_state.furthest_landing_slot := event_data.time + ModelSpecs.land_delay.U
    }.otherwise{
      branch_taken.landing_slot_open := false.B
      lp_state.furthest_landing_slot := lp_state.furthest_landing_slot + ModelSpecs.land_delay.U
      lp_state.waiting_time := lp_state.waiting_time + (lp_state.furthest_landing_slot - event_data.time)
    }
  }

  def ARRIVAL_rb_task = {
    val rb_msg = rollback_q.io.deq.bits
    when(!rollback_q.io.deq.bits.branch_taken.landing_slot_open){
      lp_state.waiting_time := lp_state.waiting_time - (rb_msg.rng_seed - rb_msg.origin_time)
    }
    lp_state.furthest_landing_slot := rb_msg.rng_seed // this event doesn't use rng, so we use its place for state bkup

    // rollback event to same LP, time based on branch taken, type Landing
  }

  def LANDING_task = {
    lp_state.num_landed := lp_state.num_landed + 1.U
    // Schedule departure
  }

  def LANDING_rb_task = {
    lp_state.num_landed := lp_state.num_landed - 1.U
    // Rollbac event to rnd lp, rnd time based on rng type DEPARTURE
  }

  def DEPARTURE_task = {
    // schedule arrival event
  }

  def DEPARTURE_rb_task = {
    // rollback event by rng
  }

  /* Create rollback events and reverse states when some events get processed out of order*/
  val rb_entries = rollback_q.io.deq
  val rb_rand_lp = Wire(rb_entries.bits.rng_seed.cloneType)
  val rb_rand_offset = LFSR.next_state_comb(rb_rand_lp)
  def ROLLBACK_task = {
    when(!hist_queue.valid) {
      when(rb_entries.valid) {
        when(!state_reversed) {
          switch(rb_entries.bits.event_type) {
            is(0.U) { // Arrival
              ARRIVAL_rb_task
            }
            is(1.U) { // Landing
              LANDING_rb_task
            } // No state change happens for departure, so it's omitted for now
          }
          state_reversed := true.B
          printf("____ Executing rollback at core %d, lp %d at time %d\n", core_id.U, event_data.lp_id, event_data.time)
        }

        val rb_event = Wire(evt_out_q.io.enq.bits)
        val rb_cancel_event = Wire(evt_out_q.io.enq.bits)
        switch(rb_entries.bits.event_type) {
          is(0.U) {
            rb_event.setValue(event_data.lp_id, rb_entries.bits.origin_time, event_type = 0.U, false.B)
            when(rb_entries.bits.branch_taken.landing_slot_open) {
              rb_cancel_event.setValue(event_data.lp_id, rb_entries.bits.origin_time + ModelSpecs.land_delay.U, event_type = 1.U, true.B)
            }.otherwise {
              rb_cancel_event.setValue(event_data.lp_id, rb_entries.bits.rng_seed + ModelSpecs.land_delay.U, event_type = 1.U, true.B)
            }
          }
          is(1.U) {
            rb_event.setValue(event_data.lp_id, rb_entries.bits.origin_time, event_type = 1.U, false.B)
            rb_cancel_event.setValue(event_data.lp_id, rb_entries.bits.origin_time + ModelSpecs.dep_delay.U + rb_rand_offset(3,0), event_type = 2.U, true.B)
          }
          is(2.U) {
            rb_event.setValue(event_data.lp_id, rb_entries.bits.origin_time, event_type = 2.U, false.B)
            rb_cancel_event.setValue(rb_rand_lp, rb_entries.bits.origin_time + ModelSpecs.arrival_delay.U + rb_rand_offset(4,0), event_type = 0.U, true.B)
          }
        }

        when(rollback_not_cancel) {
          rb_rand_lp := rb_entries.bits.rng_seed
          /* Rollback event */
          evt_out_q.io.enq.bits := rb_event
          evt_out_q.io.enq.valid := true.B
          when(evt_out_q.io.enq.ready) {
            rollback_not_cancel := false.B
            //filt_queue.deq() /* Remove entry only once during rollback+cancel generation */
          }
        }.otherwise {
          /* Cancel event */
          evt_out_q.io.enq.bits := rb_cancel_event
          evt_out_q.io.enq.valid := true.B
          when(evt_out_q.io.enq.ready) {
            rb_entries.deq()
            rollback_not_cancel := true.B
            state_reversed := false.B
          }
        }
      }.otherwise {
        state := sGEN_EVT
      }
    }
  }


  // Event Generation
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


//  val did_it_once = RegInit(false.B)
  //  when(state === sGEN_EVT){
  def GEN_task = {
    when(!hist_queue.valid) {
      /* Wait until all history events have been processed */
      when(match_found) {
        state := sHIST_WR // Rollback was done previously if needed, no more to do
//        when(need_cancellation) {
//          // Enqueue cancellation msg
//          evt_out_q.io.enq.bits.time := match_data.target_time
//          evt_out_q.io.enq.bits.lp_id := match_data.target_lp
//          evt_out_q.io.enq.bits.cancel_evt := true.B
//          evt_out_q.io.enq.valid := true.B
//          when(evt_out_q.io.enq.fire()) {
//            printf("** EP %d # Cancellation(quash) event at time: %d, LP: %d --> GVT: %d\n", core_id.U, match_data.target_time, match_data.target_lp, gvt)
//            state := sHIST_WR
//          }
//        }.otherwise {
//          printf("** EP %d # Event quashed. LP: %d\n", core_id.U, event_data.lp_id)
//          state := sHIST_WR
//        }
        /* If cancellation not needed, no new event shall be generated */
      }.otherwise {
        when(!event_data.cancel_evt) {
          val gen_evt = Wire(evt_out_q.io.enq.bits)
          switch(event_data.event_type){
            is(0.U){ //Arrival
              ARRIVAL_task
              when(lp_state.furthest_landing_slot < event_data.time){
                gen_evt.setValue(event_data.lp_id, event_data.time + ModelSpecs.land_delay.U, event_type = 1.U, false.B)
              }.otherwise{
                gen_evt.setValue(event_data.lp_id, lp_state.furthest_landing_slot + ModelSpecs.land_delay.U, event_type = 1.U, false.B)
              }
            }
            is(1.U){ // Landing
              LANDING_task
              gen_evt.setValue(event_data.lp_id, event_data.time + ModelSpecs.dep_delay.U + rand_offset(3,0), event_type = 2.U, false.B)
            }
            is(2.U){ // Departure
              DEPARTURE_task
              gen_evt.setValue(rand_lp, event_data.time + ModelSpecs.arrival_delay.U + rand_offset(4,0), event_type = 0.U, false.B)
            }
          }

//          if(core_id == 0){
//            when(!did_it_once && event_data.time > 150.U && !event_data.cancel_evt && event_data.event_type === 0.U){
//              gen_evt.setValue(event_data.lp_id, 50.U, 0.U, false.B)
//              printf("DUMMY %d\n", event_data.lp_id)
//            }
//          }

          evt_out_q.io.enq.enq(gen_evt)

          filt_enq.bits.origin_time := event_data.time
          filt_enq.bits.event_type := event_data.event_type
          when(event_data.event_type === 0.U){
            filt_enq.bits.rng_seed := lp_state.furthest_landing_slot
          }.otherwise{
            filt_enq.bits.rng_seed := current_rand
          }
          filt_enq.bits.branch_taken := branch_taken
          filt_enq.bits.cancel_evt := false.B
          filt_enq.valid := true.B
          //TODO: This may be source of future bug since we're not checking ready for filt_queue

          when(evt_out_q.io.enq.fire) {
            printf("** EP %d (LP %d)# Generate event for LP: %d at time: %d\n", core_id.U, event_data.lp_id, evt_out_q.io.enq.bits.lp_id, evt_out_q.io.enq.bits.time)
            state := sHIST_WR
          }
        }.otherwise{ // The cancel events will pushed to history where it waits for matching event to arrive
          filt_enq.bits.origin_time := event_data.time
//          filt_enq.bits.target_lp := event_data.lp_id
          filt_enq.bits.event_type := event_data.event_type
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

  val hist_written = Reg(UInt(log2Ceil(Specs.hist_size).W))
  when(state === sIDLE) {
    rollback_not_cancel := false.B
    hist_written := 0.U
  }

  def HIST_WR_task = {
    when(filt_queue.valid) {
      when(filt_queue.bits.origin_time > event_data.time && !filt_queue.bits.cancel_evt) {
        filt_queue.deq() /* Remove entry, we already handled rollback in ROLLBACK phase */
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
      state := sST_MEM
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
    is(sLD_RTN){
      LD_RTN_task
    }
    is(sROLLBACK){
      ROLLBACK_task
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
    is(sST_MEM){
      MEM_ST_task
    }
    is(sST_RTN){
      ST_RTN_task
    }
    is(sFINALISE) {
      // SEND out all events
      // SEND evt request when done
      // Become IDLE
      FINALISE_task
    }
  }

  // Report
  io.report.stalled := state === sSTALL
  io.report.mem := (state === sLD_RTN) || (state === sST_RTN)

  // Debug
  io.dbg.core_gvt := gvt
  io.dbg.active_lp := Cat((state =/= sIDLE).asUInt(), event_data.lp_id)
}

object PDESACore extends App {
  chisel3.Driver.execute(args, () =>
    new PDESACore(core_id = 3, lp_bits = 8, time_bits = 16))
}

class LPState extends Bundle{
  val num_landed = UInt(16.W)
  val waiting_time = UInt(24.W)
  val furthest_landing_slot = UInt(24.W)
}