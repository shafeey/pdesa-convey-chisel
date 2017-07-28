package pdesa

import chisel3._
import chisel3.iotesters.AdvTester
import org.scalatest._

import scala.collection.mutable

object CoreTestSpecs {
  val n_sample = 1000
  val n_mi = 8
  val n_si = 4
  val dtype = 0.U(16.W)
}

case class EventData(lp: BigInt, time: BigInt, cancel: BigInt)

case class HistMsg(time: BigInt, target_time: BigInt, target_lp: BigInt, cancel: BigInt)

case class HistReq(cid: BigInt, lp: BigInt, op: BigInt, cnt: BigInt, msg: HistMsg)

case class HistRsp(cid: BigInt, op: BigInt, msg: HistMsg)


class CoreTesterBase(c: PDESACore) extends AdvTester(c) {

  // Event request handlers
  protected val req_evt_handler = ValidSink(c.io.req_evt)
  protected val evt_complete_handler = ValidSink(c.io.done)

  // Event exchange handler
  protected val issued_evt_driver = new ValidSource(c.io.issued_evt, (in: EventMsg, dat: EventData) => {
    reg_poke(in.lp_id, dat.lp)
    reg_poke(in.time, dat.time)
    reg_poke(in.cancel_evt, dat.cancel)
  })

  protected val gen_evt_handler = new DecoupledSink(c.io.generated_evt, (out: EventMsg) => {
    EventData(peek(out.lp_id), peek(out.time), peek(out.cancel_evt))
  })

  // event history
  protected val hist_cnt_driver = new ValidSource(c.io.hist_cnt, (in: UInt, dat: BigInt) => {
    reg_poke(in, dat)
  })
  protected val hist_req_handler = new DecoupledSink(c.io.hist_req, (out: EventHistoryReq) => {
    val msg = HistMsg(peek(out.msg.origin_time), peek(out.msg.target_time),
      peek(out.msg.target_lp), peek(out.msg.cancel_evt))
    HistReq(peek(out.EP_id), peek(out.origin_lp), peek(out.op), peek(out.count), msg)
  })

  protected val hist_rsp_driver = new ValidSource(c.io.hist_rsp, (in: EventHistoryRsp, dat: HistRsp) => {
    reg_poke(in.msg.origin_time, dat.msg.time)
    reg_poke(in.msg.target_time, dat.msg.target_time)
    reg_poke(in.msg.target_lp, dat.msg.target_lp)
    reg_poke(in.msg.cancel_evt, dat.msg.cancel)

    reg_poke(in.EP_id, dat.cid)
    reg_poke(in.op, dat.op)
  })

  protected def issueEvent(ed: EventData): Unit = {
    issued_evt_driver.inputs.enqueue(ed)
  }

  protected def issueEvent(lp: Int, time: Int): Unit = {
    issued_evt_driver.inputs.enqueue(EventData(lp, time, 0))
  }

  private def sendHist(hd: HistRsp): Unit = {
    hist_rsp_driver.inputs.enqueue(hd)
  }

  private def ackHist(hd: HistRsp): Unit = {
    sendHist(hd)
  }

  protected def setGvt(gvt: Int): Unit = {
    wire_poke(c.io.gvt, gvt)
  }

  protected def setHistCnt(n: BigInt): Unit = {
    hist_cnt_driver.inputs.enqueue(n)
  }

  protected def waitForHistRdReq(lp_id: BigInt, hist_cnt: BigInt): HistReq = {
    eventually(hist_req_handler.outputs.nonEmpty)
    val rd_req: HistReq = hist_req_handler.outputs.dequeue()
    expect(rd_req.lp == lp_id, "LP id doesn't match.")
    expect(rd_req.cnt == hist_cnt, "Hist count doesn't match.")
    expect(rd_req.op == 0, "Hist operation != READ")
    rd_req
  }

  protected def sendHistMsg(req: HistReq, msg: HistMsg) = {
    val rsp = HistRsp(req.cid, req.op, msg)
    sendHist(rsp)
  }

  protected def waitForGenEvt(time: BigInt): EventData = {
    eventually(gen_evt_handler.outputs.nonEmpty)
    val gen_evt: EventData = gen_evt_handler.outputs.dequeue()
    expect(gen_evt.cancel == 0, "Received event type not correct")
    expect(gen_evt.time > time, "Received event time is not valid")
    gen_evt
  }

  protected def waitForHistWrReq(exp_msg: HistMsg) = {
    eventually(hist_req_handler.outputs.nonEmpty)
    val w: HistReq = hist_req_handler.outputs.dequeue()
    expect(w.msg == exp_msg, "HistWrite msg doesn't match expected msg.")
    w
  }

  protected def waitForEvtCompletion() = {
    eventually(evt_complete_handler.outputs.nonEmpty)
    expect(c.io.done.valid, 1, "Event completion signal not received")
  }

  protected def waitUntilEvtReq() = {
    while (peek(c.io.req_evt.valid) != 1) takestep()
  }
}

//noinspection ScalaStyle:magic.number
class SimpleCoreTester(c: PDESACore) extends CoreTesterBase(c) {
  var lp_id = 5
  var time = 17

  var hist_cnt = 1
  val msg = HistMsg(time = 13, target_time = 27, target_lp = 9, cancel = 0)

  setGvt(10)
  waitUntilEvtReq()
  issueEvent(lp_id, time)
  1 to 3 foreach { _ => takestep() }
  setHistCnt(1)

  val rd_req = waitForHistRdReq(lp_id, hist_cnt)
  1 to 5 foreach { _ => takestep() }
  sendHistMsg(rd_req, msg)

  val gen_evt = waitForGenEvt(time)
  val evt_msg = HistMsg(time, gen_evt.time, gen_evt.lp, gen_evt.cancel)

  val w1 = waitForHistWrReq(msg)
  val w2 = waitForHistWrReq(evt_msg)
  takestep()
  takestep()

  sendHistMsg(w1, HistMsg(0, 0, 0, 0))
  sendHistMsg(w2, HistMsg(0, 0, 0, 0))

  waitForEvtCompletion()
  takestep()
}

class CoreTest extends FreeSpec with Matchers {
  "Core should generate a simple event" - {
    "using interpreter" in {
      chisel3.iotesters.Driver.execute(
        Array("--backend-name", "verilator", "--fint-write-vcd"),
        () => new PDESACore(3, 8, 16)) { c =>
        new SimpleCoreTester(c)
      } should be(true)
    }
  }
}