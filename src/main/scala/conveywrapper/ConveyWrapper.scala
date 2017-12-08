package conveywrapper

import chisel3._
import chisel3.util._

trait PlatformParams extends MemParams{
  val numMemPorts: Int = 8 // Important: Also change the NUM_MC_PORT parameter in resources/cae_pers.v and Makefile
  val rtnctlWidth: Int = memIDBits
  val maxNumMemPorts: Int = 8

  val numAEGReg: Int = 16
  val widAEGReg: Int = 64
}

trait MemParams{
  val memAddrBits: Int = 32
  val memDataBits: Int = 64
  val memIDBits: Int = 32
  val memMetaBits: Int = 8
  val sameIDInOrder: Boolean = true

  val MEM_ADDR_WID = 48
  val MEM_RD_CMD: Int = 1
  val MEM_WR_CMD: Int = 2
  val MEM_RD_DATA: Int = 2
  val MEM_WR_COMPLETE: Int = 3
  val MEM_SIZE_BYTE: Int = 3 // 8 byte rd/wr
}

class ConveyWrapper(accelerator: () => Accelerator) extends Module with PlatformParams{
  val io = IO(new ConveyPersonalityVerilogIF(numMemPorts, rtnctlWidth))

  val accel = Module(accelerator())
  // Wait for a valid instruction to be dispatched
  // Then act as a status flag
  val regBusy = RegInit(false.B)
  val regInst = RegInit(0.U(5.W))
  when(!regBusy){
    regBusy := io.dispInstValid
    regInst := io.dispInstData
  }.elsewhen(accel.io.finished){
    regBusy := false.B
  }

  io.dispIdle := !regBusy
  io.dispStall := regBusy

  // Use the Convey AEG dispatch interface
  // Write application arguments to register file
  val regFile = Module(new RegFile(numAEGReg, log2Ceil(numAEGReg), widAEGReg))

  //CAE set AEG_CNT in the dispatch module which remains static for a personality
  io.dispAegCnt := numAEGReg.U

  // RegFile slave interface for host commands
  // The RegFile stores application arguments from the host and the return values from the CAE
  regFile.io.extIF.cmd.bits.regID := io.dispRegID
  regFile.io.extIF.cmd.bits.read := io.dispRegRead
  regFile.io.extIF.cmd.bits.write := io.dispRegWrite
  regFile.io.extIF.cmd.bits.writeData := io.dispRegWrData
  regFile.io.extIF.cmd.valid := io.dispRegRead || io.dispRegWrite

  io.dispRtnValid := regFile.io.extIF.readData.valid
  io.dispRtnData := regFile.io.extIF.readData.bits

  accel.io.regPort <> regFile.io.regOut
  accel.io.retPort <> regFile.io.regIn

  // Exception when reading AEG Registers
  val invalidAEGId = RegNext(next = (io.dispRegWrite || io.dispRegRead) && (io.dispRegID >= numAEGReg.U), init = false.B)

  // Create exception from Accelerator response and AEG reading error
  io.dispException := accel.io.exception | (invalidAEGId << 1).asUInt()

  // plug the CSR IF
  io.csrReadAck := false.B
  io.csrReadData := 0.U

  // Memory ports wiring
  io.mcReqValid := 0.U
  io.mcReqRtnCtl := 0.U
  io.mcReqData := 0.U
  io.mcReqAddr := 0.U
  io.mcReqSize := 0.U
  io.mcReqCmd := 0.U
  io.mcReqSCmd := 0.U
  io.mcResStall := 0.U
  io.mcReqFlush := 0.U

  // wire up memory port adapters
  // TODO add support for write flush
  if(numMemPorts > maxNumMemPorts) {
    throw new Exception("Too many mem ports in accelerator")
  }

  //  val memReqParams = new MemReqParams(memAddrBits, memDataBits, memIDBits, memMetaBits, sameIDInOrder)

  //  val memPorts: Seq[ConveyMemMasterIF] = Wire(Vec(numMemPorts, new ConveyMemMasterIF(rtnctlWidth)))

  //  for(i <- 0 until numMemPorts) { accel.io.memPort(i) <> memPorts(i) }

  // Convey's interface semantics (stall-valid) are a bit more different than
  // just a decoupled (inverted ready)-valid:
  // X1) valid and stall asserted together can still mean a transferred element
  //     (i.e valid may not go down for up to 2 cycles after stall is asserted)
  // X2) valid on Convey IF must actually go down after stall is asserted

  def mpHelper(extr: ConveyMemMasterIF => Bits): Bits = {
    Cat(accel.io.memPort.map(extr).reverse)
  }
  type mp = ConveyMemMasterIF

  io.mcReqRtnCtl := mpHelper({mp => mp.req.bits.rtnCtl})
  io.mcReqAddr := mpHelper({mp => mp.req.bits.addr})
  io.mcReqSize := mpHelper({mp => mp.req.bits.size})
  io.mcReqCmd := mpHelper({mp => mp.req.bits.cmd})
  io.mcReqSCmd := mpHelper({mp => mp.req.bits.scmd})
  io.mcReqData := mpHelper({mp => mp.req.bits.writeData})

  // memory response handling:
  // compensate for interface semantics mismatch for memory responses (X1) with
  // little queues
  // - personality receives responses through queue
  // - Convey mem.port's stall is driven by "almost full" from queue
  // - Upto 8 more responses can arrive after stall is asserted
  val respQueElems = 16
  val respQueues = Seq.fill(numMemPorts)(
    Module(
      new Queue(new ConveyMemResponse(memIDBits, memDataBits), respQueElems)
    ).io)

  // an "almost full" derived from the queue count is used
  // to drive the Convey mem resp port's stall input
  // this is quite conservative (stall when FIFO is half full) but
  // it seems to work (there may be a deeper problem here)
  val respStall = Cat(respQueues.map(x => x.count >= (respQueElems/2).U))
  // Cat concatenation order needs to be reversed
  io.mcResStall := Reverse(respStall)

  // drive personality inputs
  for(i <- 0 until numMemPorts) {
    respQueues(i).enq.valid := io.mcResValid(i)
    respQueues(i).enq.bits.rtnCtl := io.mcResRtnCtl(32*(i +1)-1, 32*i)
    respQueues(i).enq.bits.readData := io.mcResData(64*(i +1)-1, 64*i)
    respQueues(i).enq.bits.cmd := io.mcResCmd(3*(i +1)-1, 3*i)
    respQueues(i).enq.bits.scmd := io.mcResSCmd(4*(i +1)-1, 4*i)
    // note that we don't use the enq.ready signal from the queue --
    // we generate our own variant of ready when half-full

    // adapter receives responses from this queue
    accel.io.memPort(i).rsp <> respQueues(i).deq

    // drive req.ready from inverse of req.stall
    accel.io.memPort(i).req.ready := ~io.mcReqStall(i)
  }

  // to compensate for X2, we AND the valid with the inverse of stall before
  // outputting valid
  // TODO do not create potential combinational loop -- make queue-based sln
  io.mcReqValid := mpHelper({mp => mp.req.valid & mp.req.ready})

  // print some warnings to remind the user to change the Makefile values
  println(s"====> Remember to set NUM_MC_PORTS=$numMemPorts in Makefile.include")
  val numRtnCtlBits: Int = memIDBits
  println(s"====> Remember to set RTNCTL_WIDTH=$numRtnCtlBits in Makefile.include")

  // instantiate the accelerator
  val regWrapperReset = RegInit(init = false.B) //, clock = Driver.implicitClock)
  // permits controlling the accelerator's reset from both the wrapper's reset,
  // and by using a special register file command (see hack further down :)
  accel.reset := reset | regWrapperReset

  // take precaution. Accelerator must not start issuing memreqs before dispatch comes.

  // Relay start signal and instruction to accelerator
  val regInstValid = RegNext(next = !regBusy && io.dispInstValid)

  accel.io.start := regInstValid
  accel.io.inst := regInst
  accel.io.aeid := io.aeid


  val cycle = RegInit(0.U(64.W))
  cycle := cycle + 1.U

  for(p <- 0 until numMemPorts){
    when(io.mcReqValid(p) && io.mcReqCmd(p*3 + 2, p*3) === 6.U){
      printf("Cycle: %d==> MP %d writes to address: %x -- Data: %x\n", cycle, p.U, io.mcReqAddr(p*48 + 47, p*48), io.mcReqData(p*64 + 63, p*64))
    }
  }
  accel.io.memPort.zipWithIndex.foreach{case(mp, id) =>
    when(mp.req.bits.cmd === 6.U){
    }
  }
}
