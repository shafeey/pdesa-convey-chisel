package pdesa

import chisel3._
import chisel3.util._
import chisel3.iotesters.{AdvTester, PeekPokeTester}
import org.scalatest._

import scala.collection.mutable

case class DispatchMsg(tag: BigInt, msg: EventData)

class EventManagerTesterBase(c: EventManager, num_q: Int) extends AdvTester(c) {
  // request driver
  val req_drivers = for (i <- 0 until num_q) yield {
    DecoupledSource(c.io.evt_req(i))
  }

  // Event Input driver
  val in_drivers = for (i <- 0 until num_q) yield {
    new DecoupledSource(c.io.in(i), (in: EventMsg, m: EventData) => {
      reg_poke(in.time, m.time)
      reg_poke(in.lp_id, m.lp)
      reg_poke(in.cancel_evt, m.cancel)
    })
  }

  // Event output handler
  val out_handler = for (i <- 0 until num_q) yield {
    new ValidSink(c.io.out(i), (out: EventDispatchBundle) => {
      val m = EventData(peek(out.msg.lp_id), peek(out.msg.time), peek(out.msg.cancel_evt))
      DispatchMsg(peek(out.tag), m)
    })
  }

  def sendEvent(qid: Int, m: EventData): Unit = {
    in_drivers(qid).inputs.enqueue(m)
  }

  def expectEvent(qid: Int, tag: BigInt, msg: EventData): DispatchMsg = {
    eventually(out_handler(qid).outputs.nonEmpty)
    val d: DispatchMsg = out_handler(qid).outputs.dequeue()
    expect(d.tag == tag, "Tag doesn't match.")
    expect(d.msg == msg, "Msg doesn't match.")
    d
  }

  def reqEvent(qid: Int, cid: BigInt): Unit = {
    req_drivers(qid).inputs.enqueue(cid)
  }

  val ilist = Seq.fill(num_q)(new mutable.ListBuffer[EventData])
  val olist = Seq.fill(num_q)(new mutable.ListBuffer[(Int, EventData)])

  0 until num_q foreach { i =>
    ilist(i) += EventData(rnd.nextInt(100), rnd.nextInt(100), 0)
    sendEvent(i, ilist(i).last)
  }

  0 until num_q foreach { i =>
    olist(i) += ((rnd.nextInt(Specs.num_cores), ilist(i).remove(0)))
    reqEvent(i, olist(i).last._1)
  }

  0 until num_q foreach { i =>
    expectEvent(i, olist(i).head._1, olist(i).head._2)
    olist(i).remove(0)
  }
}

class EventManagerTest extends FreeSpec with Matchers {
  val options = Array("--backend-name", "verilator", "--fint-write-vcd")

  "Event Manager should respond properly" - {
    "with simple input and output" in {
      chisel3.iotesters.Driver.execute(options,
        () => new EventManager(num_q = 4)) { c =>
        new EventManagerTesterBase(c, num_q = 4)
      } should be(true)
    }
  }
}
