package pdesa

import chisel3._
import chisel3.util._
import chisel3.iotesters.{AdvTester, PeekPokeTester}
import org.scalatest._

import scala.collection.mutable

//class MemWrapper extends Module{
//  val io = IO{new Bundle{
//    val addr = Input(UInt(3.W))
//    val enable = Input(Bool())
//    val wen = Input(Bool())
//
//    val wrdat = Input(UInt(8.W))
//    val rddat = Output(UInt(8.W))
//
//  }}
//
//  val width:Int = 8
//  val addr = Wire(UInt(width.W))
//  val dataIn = Wire(UInt(width.W))
//  val dataOut = Wire(UInt(width.W))
//  val enable = Wire(Bool())
//  val write = Wire(Bool())
//
//  write := io.wen
//  addr := io.addr
//  dataIn := io.wrdat
//  io.rddat := dataOut
//  enable := io.enable
//
//  // assign data...
//
//  // Create a synchronous-read, synchronous-write memory (like in FPGAs).
//  val mem = SyncReadMem(1024, UInt(width.W))
//  // Create one write port and one read port.
//  when (write) { mem.write(addr, dataIn) }
//    .otherwise { dataOut := mem.read(addr, enable) }
//}

class HistoryTesterBase(c: EventHistoryManager) extends AdvTester(c) {
  // request driver
  val req_driver = new DecoupledSource(c.io.hist_req, (in: EventHistoryReq, r: HistReq) => {
    reg_poke(in.count, r.cnt)
    reg_poke(in.op, r.op)
    reg_poke(in.origin_lp, r.lp)
    reg_poke(in.EP_id, r.cid)
    reg_poke(in.msg.origin_time, r.msg.time)
    reg_poke(in.msg.target_time, r.msg.target_time)
    reg_poke(in.msg.target_lp, r.msg.target_lp)
    reg_poke(in.msg.cancel_evt, r.msg.cancel)
  })

  // response handler
  val rsp_handler = new ValidSink(c.io.hist_rsp, (out: EventHistoryRsp) => {
    val msg = HistMsg(peek(out.msg.origin_time), peek(out.msg.target_time), peek(out.msg.target_lp), peek(out.msg.cancel_evt))
    HistRsp(peek(out.EP_id), peek(out.op), msg)
  })

  protected def sendReadReq(cid: BigInt, lp: BigInt, count: BigInt) = {
    //    assert(count > 0, "Read count should be at least 1.")
    val msg = HistMsg(0, 0, 0, 0)
    val r = HistReq(cid, lp, EventHistroyCmnd.sOP_RD, count, msg)
    req_driver.inputs.enqueue(r)
  }

  protected def sendWriteReq(cid: BigInt, lp: BigInt, count: BigInt, msg: HistMsg) = {
    //    assert(count > 0, "Read count should be at least 1.")
    val r = HistReq(cid, lp, EventHistroyCmnd.sOP_WR, count, msg)
    req_driver.inputs.enqueue(r)
  }

  protected def expectReadRsp(cid: BigInt, count: BigInt, msg: List[HistMsg]) = {
    msg.foreach(m => {
      eventually(rsp_handler.outputs.nonEmpty)
      val rsp: HistRsp = rsp_handler.outputs.dequeue()
      expect(rsp == HistRsp(cid, EventHistroyCmnd.sOP_RD, m), "Returned history don't match.")
      println(rsp.toString)
    })
  }

  protected def expectWriteRsp(cid: BigInt, tag: BigInt) = {
    eventually(rsp_handler.outputs.nonEmpty)
    val rsp: HistRsp = rsp_handler.outputs.dequeue()
    expect(rsp.cid == cid, "Write response destination core id doesn't match")
  }

  protected def rndMsg() = {
    HistMsg(rnd.nextInt(1 << Specs.time_bits), rnd.nextInt(1 << Specs.time_bits), rnd.nextInt(Specs.num_lp), rnd.nextInt(1))
  }

}

class HistoryTesterSimple(c: EventHistoryManager) extends HistoryTesterBase(c) {
  val m1 = HistMsg(11, 12, 13, 0)
  val m2 = rndMsg()
  val m3 = rndMsg()

  sendWriteReq(cid = 5, lp = 2, count = 0, m1)
  expectWriteRsp(cid = 5, tag = 0)
  takestep()

  sendReadReq(cid = 3, lp = 2, count = 1)
  expectReadRsp(cid = 3, count = 1, List(m1))
  takestep()
}

class HistoryTesterMulti(c: EventHistoryManager) extends HistoryTesterBase(c) {
  val m1 = HistMsg(11, 12, 13, 0)
  val m2 = HistMsg(21, 22, 23, 0)
  val m3 = HistMsg(31, 32, 33, 1)

  takestep()

  sendWriteReq(cid = 5, lp = 2, count = 0, m1)
  sendWriteReq(cid = 5, lp = 2, count = 1, m2)

  expectWriteRsp(cid = 5, tag = 0)
  expectWriteRsp(cid = 5, tag = 1)

  takestep()

  sendReadReq(cid = 3, lp = 2, count = 2)
  expectReadRsp(cid = 3, count = 3, List(m1, m2))
  takestep()
  takestep()
}

class HistoryTesterMixed(c: EventHistoryManager) extends HistoryTesterBase(c) {
  val q1 = new mutable.ListBuffer[HistMsg]
  val q2 = new mutable.ListBuffer[HistMsg]
  val q3 = new mutable.ListBuffer[HistMsg]

  0 until 3 foreach (x => {
    q1 += rndMsg()
    sendWriteReq(cid = 5, lp = 2, count = x, q1.last)
    q2 += rndMsg()
    sendWriteReq(cid = 6, lp = 3, count = x, q2.last)
    q3 += rndMsg()
    sendWriteReq(cid = 7, lp = 4, count = x, q3.last)
  })

  sendReadReq(cid = 2, lp = 2, count = 3)
  sendReadReq(cid = 3, lp = 3, count = 3)
  sendReadReq(cid = 4, lp = 4, count = 3)

  0 until 3 foreach (x => {
    expectWriteRsp(cid = 5, tag = x)
    expectWriteRsp(cid = 6, tag = x)
    expectWriteRsp(cid = 7, tag = x)
  })

  expectReadRsp(cid = 2, count = 3, q1.result())
  expectReadRsp(cid = 3, count = 3, q2.result())
  expectReadRsp(cid = 4, count = 3, q3.result())

  takestep()
  takestep()
}

class HistoryTest extends FreeSpec with Matchers {
  val options = Array("--backend-name", "verilator", "--fint-write-vcd")

  "History table should manage event history properly" - {
        "with simple event read" in {
          chisel3.iotesters.Driver.execute(options,
            () => new EventHistoryManager) { c =>
            new HistoryTesterSimple(c)
          } should be(true)
        }

        "with multiple write and read" in {
          chisel3.iotesters.Driver.execute(options,
            () => new EventHistoryManager) { c =>
            new HistoryTesterMulti(c)
          } should be(true)
        }

    "with write and read from different core/lp" in {
      chisel3.iotesters.Driver.execute(options,
        () => new EventHistoryManager) { c =>
        new HistoryTesterMixed(c)
      } should be(true)
    }

  }
}
