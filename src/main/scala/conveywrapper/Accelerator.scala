package conveywrapper

import chisel3._
import chisel3.util._

class AcceleratorIF(numMemPorts: Int, numReg: Int) extends Bundle with PlatformParams{
  val start = Input(Bool())
  val inst = Input(UInt(5.W))
  val finished = Output(Bool())
  val exception = Output(UInt(16.W))
  val aeid = Input(UInt(4.W))

  val memPort = Vec(numMemPorts, new ConveyMemMasterIF(rtnctlWidth))
  val regPort = Input(Vec(numReg, UInt(memDataBits.W)))
  val retPort = Vec(numReg, Valid(UInt(memDataBits.W)))
}

abstract class Accelerator extends Module{
  val io : AcceleratorIF // = IO(new AcceleratorIF(numMemPorts, numAEGReg, numAEGReg))
  // drive default values for memory read port i
  def plugMemPort(i: Int): Unit ={
    io.memPort(i).req.bits.driveDefaults()
  }

  def plugRetPort(i: Int): Unit = {
    io.retPort(i).valid := false.B
  }
}
