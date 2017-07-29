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

  protected def sendHistMsg(req: HistReq, msg: HistMsg): Unit = {
    val rsp = HistRsp(req.cid, req.op, msg)
    sendHist(rsp)
  }

  protected def sendHistMsg(req: HistReq): Unit = {
    /* Write acknowlegement */
    sendHistMsg(req, HistMsg(0, 0, 0, 0))
  }

  private def waitForEvent(): EventData = {
    eventually(gen_evt_handler.outputs.nonEmpty)
    val gen_evt: EventData = gen_evt_handler.outputs.dequeue()
    gen_evt
  }

  protected def waitForGenEvt(time: BigInt): EventData = {
    val gen_evt = waitForEvent()
    expect(gen_evt.cancel == 0, "Received event type not correct")
    expect(gen_evt.time > time, "Received event time is not valid")
    gen_evt
  }

  protected def waitForRollbackEvt(time: BigInt, lp: BigInt): EventData = {
    val gen_evt = waitForEvent()
    expect(gen_evt.cancel == 0, "Received event type not correct")
    expect(gen_evt.time == time, "Received event time is not valid")
    gen_evt
  }

  protected def waitForCancelEvt(time: BigInt): EventData = {
    val gen_evt = waitForEvent()
    expect(gen_evt.cancel == 1, "Received event type not correct")
    expect(gen_evt.time == time, "Cancellation event time is not valid")
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
    expect(hist_req_handler.outputs.isEmpty, "Unwanted pending history request.")
    expect(gen_evt_handler.outputs.isEmpty, "Unwanted pending event message.")
  }

  protected def waitUntilEvtReq() = {
    while (peek(c.io.req_evt.valid) != 1) takestep()
  }
  protected def checkHistWritePosition(w: HistReq, n: Int) = {
    expect(w.cnt == n, "History write position isn't correct")
  }
}

//noinspection ScalaStyle:magic.number
class CoreSimpleTester(c: PDESACore) extends CoreTesterBase(c) {
  var gvt = 10
  var lp_id = 5
  var time = 17

  var hist_cnt = 1
  val msg = HistMsg(time = 13, target_time = 27, target_lp = 9, cancel = 0)

  setGvt(gvt)
  waitUntilEvtReq()
  issueEvent(lp_id, time)
  1 to 3 foreach { _ => takestep() }
  setHistCnt(hist_cnt)

  val rd_req = waitForHistRdReq(lp_id, hist_cnt)
  1 to 5 foreach { _ => takestep() }
  sendHistMsg(rd_req, msg)

  val gen_evt = waitForGenEvt(time)
  val evt_msg = HistMsg(time, gen_evt.time, gen_evt.lp, gen_evt.cancel)

  val w1 = waitForHistWrReq(msg)
  checkHistWritePosition(w1, 0)
  val w2 = waitForHistWrReq(evt_msg)
  checkHistWritePosition(w2, 1)
  takestep()
  takestep()

  sendHistMsg(w1)
  sendHistMsg(w2)

  waitForEvtCompletion()
  takestep()
}

class CoreRollbackTester(c: PDESACore) extends CoreTesterBase(c) {
  var gvt = 35
  var lp_id = 5
  var time = 51

  var hist_cnt = 2
  val msg1 = HistMsg(time = 39, target_time = 67, target_lp = 9, cancel = 0)
  /* This event should be rolled back */
  val msg2 = HistMsg(time = 53, target_time = 73, target_lp = 11, cancel = 0)

  setGvt(gvt)
  waitUntilEvtReq()
  issueEvent(lp_id, time)
  1 to 3 foreach { _ => takestep() }
  setHistCnt(hist_cnt)

  val rd_req = waitForHistRdReq(lp_id, hist_cnt)
  1 to 5 foreach { _ => takestep() }
  sendHistMsg(rd_req, msg1)
  sendHistMsg(rd_req, msg2)

  val gen_evt = waitForGenEvt(time)
  val evt_msg = HistMsg(time, gen_evt.time, gen_evt.lp, gen_evt.cancel)

  val cncl_evt = waitForCancelEvt(time = 73)
  val rb_evt = waitForRollbackEvt(time = 53, lp = lp_id)

  val w1 = waitForHistWrReq(msg1)
  checkHistWritePosition(w1, 0)
  val w2 = waitForHistWrReq(evt_msg)
  checkHistWritePosition(w2, 1)

  takestep()
  takestep()

  sendHistMsg(w1)
  sendHistMsg(w2)

  waitForEvtCompletion()
  takestep()
}

class CoreMultiRollbackTester(c: PDESACore) extends CoreTesterBase(c) {
  var gvt = 35
  var lp_id = 5
  var time = 51

  var hist_cnt = 2
  /* Both events should be rolled back */
  val msg1 = HistMsg(time = 79, target_time = 97, target_lp = 9, cancel = 0)
  val msg2 = HistMsg(time = 53, target_time = 73, target_lp = 11, cancel = 0)

  setGvt(gvt)
  waitUntilEvtReq()
  issueEvent(lp_id, time)
  1 to 3 foreach { _ => takestep() }
  setHistCnt(hist_cnt)

  val rd_req = waitForHistRdReq(lp_id, hist_cnt)
  1 to 5 foreach { _ => takestep() }
  sendHistMsg(rd_req, msg1)
  sendHistMsg(rd_req, msg2)

  val gen_evt = waitForGenEvt(time)
  val evt_msg = HistMsg(time, gen_evt.time, gen_evt.lp, gen_evt.cancel)

  val cncl_evt1 = waitForCancelEvt(time = 97)
  val rb_evt1 = waitForRollbackEvt(time = 79, lp = lp_id)

  val cncl_evt2 = waitForCancelEvt(time = 73)
  val rb_evt2 = waitForRollbackEvt(time = 53, lp = lp_id)

  val w1 = waitForHistWrReq(evt_msg)

  takestep()
  takestep()

  sendHistMsg(w1)

  waitForEvtCompletion()
  takestep()
}

class CoreCancelTester(c: PDESACore) extends CoreTesterBase(c) {
  var gvt = 35
  var lp_id = 5
  var time = 51

  var hist_cnt = 2
  val msg1 = HistMsg(time = 39, target_time = 97, target_lp = 9, cancel = 0)
  /* This event is the pending anti-message */
  val msg2 = HistMsg(time = 51, target_time = 0, target_lp = 0, cancel = 1)

  setGvt(gvt)
  waitUntilEvtReq()
  issueEvent(lp_id, time)
  1 to 3 foreach { _ => takestep() }
  setHistCnt(hist_cnt)

  val rd_req = waitForHistRdReq(lp_id, hist_cnt)
  1 to 5 foreach { _ => takestep() }
  sendHistMsg(rd_req, msg1)
  sendHistMsg(rd_req, msg2)

  //  val gen_evt = waitForGenEvt(time)
  //  val evt_msg = HistMsg(time, gen_evt.time, gen_evt.lp, gen_evt.cancel)

  val w1 = waitForHistWrReq(msg1)

  takestep()
  takestep()

  sendHistMsg(w1)

  waitForEvtCompletion()
  takestep()
}

class CoreGVTTester(c: PDESACore) extends CoreTesterBase(c) {
  var gvt = 45
  var lp_id = 5
  var time = 55

  var hist_cnt = 2
  /* This event is expired and will be erased from history */
  val msg1 = HistMsg(time = 39, target_time = 67, target_lp = 9, cancel = 0)
  val msg2 = HistMsg(time = 53, target_time = 73, target_lp = 11, cancel = 0)

  setGvt(gvt)
  waitUntilEvtReq()
  issueEvent(lp_id, time)
  1 to 3 foreach { _ => takestep() }
  setHistCnt(hist_cnt)

  val rd_req = waitForHistRdReq(lp_id, hist_cnt)
  1 to 5 foreach { _ => takestep() }
  sendHistMsg(rd_req, msg1)
  sendHistMsg(rd_req, msg2)

  val gen_evt = waitForGenEvt(time)
  val evt_msg = HistMsg(time, gen_evt.time, gen_evt.lp, gen_evt.cancel)

  val w1 = waitForHistWrReq(msg2)
  val w2 = waitForHistWrReq(evt_msg)

  takestep()
  takestep()

  sendHistMsg(w1)
  sendHistMsg(w2)

  waitForEvtCompletion()
  takestep()
}


class CoreTest extends FreeSpec with Matchers {
  val options = Array("--backend-name", "verilator", "--fint-write-vcd")

  "Core should process events properly" - {
    "with simple event" in {
      chisel3.iotesters.Driver.execute(options,
        () => new PDESACore(3, 8, 16)) { c =>
        new CoreSimpleTester(c)
      } should be(true)
    }

    "with single rollback" in {
      chisel3.iotesters.Driver.execute(options,
        () => new PDESACore(3, 8, 16)) { c =>
        new CoreRollbackTester(c)
      } should be(true)
    }

    "with multiple rollbacks" in {
      chisel3.iotesters.Driver.execute(options,
        () => new PDESACore(3, 8, 16)) { c =>
        new CoreMultiRollbackTester(c)
      } should be(true)
    }

    "with event cancelled" in {
      chisel3.iotesters.Driver.execute(options,
        () => new PDESACore(3, 8, 16)) { c =>
        new CoreCancelTester(c)
      } should be(true)
    }

    "with history cleared by GVT" in {
      chisel3.iotesters.Driver.execute(options,
        () => new PDESACore(3, 8, 16)) { c =>
        new CoreGVTTester(c)
      } should be(true)
    }
  }
}