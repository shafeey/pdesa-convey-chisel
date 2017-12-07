package cae

import chisel3.util.HasBlackBoxResource
import chisel3._

class CAEPers extends BlackBox with HasBlackBoxResource{
  val io = IO(new Bundle(){
    val clk = Input(Clock())
    val i_reset = Input(Bool())
  })

  setResource("/cae_pers.v")
}

class CAEPersInvoker extends Module{
  val io= IO(new Bundle())

  val cae_pers = Module(new CAEPers)
  cae_pers.io.clk := clock
  cae_pers.io.i_reset := reset
}