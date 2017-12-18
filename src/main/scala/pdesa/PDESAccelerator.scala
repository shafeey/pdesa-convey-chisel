package pdesa

import chisel3._
import chisel3.util._
import conveywrapper._
import cae._

class PDESAccelerator extends Accelerator with PlatformParams{
  val io = IO(new AcceleratorIF(numMemPorts, numAEGReg))

  val pdesa_engine = Module(new PDESA)

  /* Interface registers */
  val ADDR_A1 = 0
  val SIM_END_TIME = 1
  val NUM_INIT_EVENTS = 2
  val NUM_MEM_ACCESS = 3
  val CORE_MASK = 4
  val NUM_LP_MASK =5
  val NUM_DELAYS = 6

  val RET_GVT = 7
  val TOT_CYCLES = 8
  val TOT_STALLS = 9
  val TOT_EVENTS = 10
  val TOT_ANTIMSG = 11
  val TOT_Q_CONF = 12
  val TOT_HIST_CONF = 13
  val TOT_MEM_CONF = 14
  val TOT_MEM_DELAY = 15
  val TOT_PROC = 16

  val MAX_SIM_TIME = (~0.U(32.W)).asUInt()
  val sim_time_counter = RegInit(0.U(32.W))
  val max_sim_time_reached = sim_time_counter === MAX_SIM_TIME
  val unimplemented_inst = RegNext(next = io.start && io.inst =/= 0.U(5.W), init = false.B)
  io.exception := Cat(max_sim_time_reached,
    false.B, // empty to match the Vadd examples host-side code
    false.B, // Will be OR'ed with the Register ID exception by the wrapper
    unimplemented_inst)

  (0 until numMemPorts).foreach(plugMemPort)
  (0 until numAEGReg).foreach(plugRetPort)

  val sIDLE :: sRUNNING :: sEND :: Nil = Enum(3)
  val state = RegInit(sIDLE)

  switch(state){
    is(sIDLE){
      when(io.start && io.inst === 0.U){
        state := sRUNNING
      }
    }
    is(sRUNNING){
      when(io.aeid === 0.U) {
        sim_time_counter := sim_time_counter + 1.U
        when(pdesa_engine.io.done.valid) {
          state := sEND
        }
      }.otherwise{
        state := sEND
      }
    }
    is(sEND){
      state := sIDLE
    }
  }

  // report back
  when(pdesa_engine.io.done.valid){
    io.retPort(RET_GVT).valid := true.B
    io.retPort(RET_GVT).bits := pdesa_engine.io.done.bits

    io.retPort(TOT_CYCLES).valid := true.B
    io.retPort(TOT_CYCLES).bits := pdesa_engine.io.report.total_cycles
    io.retPort(TOT_STALLS).valid := true.B
    io.retPort(TOT_STALLS).bits := pdesa_engine.io.report.total_stalls
    io.retPort(TOT_EVENTS).valid := true.B
    io.retPort(TOT_EVENTS).bits := pdesa_engine.io.report.total_events
    io.retPort(TOT_ANTIMSG).valid := true.B
    io.retPort(TOT_ANTIMSG).bits := pdesa_engine.io.report.total_antimsg
    io.retPort(TOT_Q_CONF).valid := true.B
    io.retPort(TOT_Q_CONF).bits := pdesa_engine.io.report.total_q_conflict
    io.retPort(TOT_HIST_CONF).valid := true.B
    io.retPort(TOT_HIST_CONF).bits := pdesa_engine.io.report.total_hist_conflict
    io.retPort(TOT_MEM_CONF).valid := true.B
    io.retPort(TOT_MEM_CONF).bits := pdesa_engine.io.report.total_mem_conflict
    io.retPort(TOT_MEM_DELAY).valid := true.B
    io.retPort(TOT_MEM_DELAY).bits := pdesa_engine.io.report.total_mem_time
    io.retPort(TOT_PROC).valid := true.B
    io.retPort(TOT_PROC).bits := pdesa_engine.io.report.total_proc
  }

  pdesa_engine.io.start := state === sRUNNING
  pdesa_engine.io.target_gvt := io.regPort(SIM_END_TIME)
  pdesa_engine.io.addr := io.regPort(ADDR_A1)
  pdesa_engine.io.conf.proc_delay := io.regPort(NUM_DELAYS)
  pdesa_engine.io.conf.num_init_events := io.regPort(NUM_INIT_EVENTS)
  pdesa_engine.io.conf.num_mem_access := io.regPort(NUM_MEM_ACCESS)
  pdesa_engine.io.memPort.zip(io.memPort).foreach(mp => mp._1 <> mp._2)

  /* Keep engines in other AE disabled. Only run in AE 0 */
  pdesa_engine.reset := reset || (io.aeid =/= 0.U) || (state === sEND)

  io.finished := state === sEND
}

object ExecutionParameters {
  val params: Array[String] =
  Array(
    "--target-dir", "./verilog",
    "--split-modules"
  )
}

object Accel extends App{
  chisel3.Driver.execute(ExecutionParameters.params, () =>
    new ConveyWrapper(() => new PDESAccelerator)
  )

  // This copies the modified cae_pers.v from the resources directory
  // to the target directory with other compiled files
  chisel3.Driver.execute(ExecutionParameters.params, () => new CAEPersInvoker)
}