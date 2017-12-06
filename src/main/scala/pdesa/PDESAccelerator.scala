package pdesa

import chisel3._
import chisel3.util._
import conveywrapper._
import cae._

class PDESAccelerator extends Accelerator with PlatformParams{
  val io = IO(new AcceleratorIF(numMemPorts, numAEGReg))

  val pdesa_engine = Module(new PDESA)

  /* Interface registers */
  val sADDR_A1 :: sSIM_END_TIME :: sNUM_INIT_EVENTS :: sNUM_MEM_ACCESS :: sNUM_LP_MASK :: sRET_GVT :: sTOT_CYCLES :: sTOT_STALLS :: sTOT_EVENTS :: sTOT_ANTIMSG :: sTOT_QCONF :: sTOT_HIST_CONF :: sTOT_MEM_DELAY :: sSTATUS :: Nil = Enum(14)

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
    io.retPort(sRET_GVT).valid := true.B
    io.retPort(sRET_GVT).bits := pdesa_engine.io.done.bits

    io.retPort(sTOT_CYCLES).valid := true.B
    io.retPort(sTOT_CYCLES).bits := pdesa_engine.io.report.total_cycles
    io.retPort(sTOT_STALLS).valid := true.B
    io.retPort(sTOT_STALLS).bits := pdesa_engine.io.report.total_stalls
    io.retPort(sTOT_EVENTS).valid := true.B
    io.retPort(sTOT_EVENTS).bits := pdesa_engine.io.report.total_events
    io.retPort(sTOT_ANTIMSG).valid := true.B
    io.retPort(sTOT_ANTIMSG).bits := pdesa_engine.io.report.total_antimsg
    io.retPort(sTOT_QCONF).valid := true.B
    io.retPort(sTOT_QCONF).bits := pdesa_engine.io.report.total_q_conflict
    io.retPort(sTOT_HIST_CONF).valid := true.B
    io.retPort(sTOT_HIST_CONF).bits := pdesa_engine.io.report.total_hist_conflict
    io.retPort(sTOT_MEM_DELAY).valid := true.B
    io.retPort(sTOT_MEM_DELAY).bits := pdesa_engine.io.report.total_mem_time
  }

  pdesa_engine.io.start := state === sRUNNING
  pdesa_engine.io.target_gvt := io.regPort(sSIM_END_TIME)
  pdesa_engine.io.addr := io.regPort(sADDR_A1)
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