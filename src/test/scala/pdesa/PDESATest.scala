package pdesa

import chisel3._
import chisel3.iotesters._
import org.scalatest.{FreeSpec, Matchers}

import scala.collection.mutable

class PDESATester(c: PDESA) extends PeekPokeTester(c){
  val max_cycle = 2000
  val target_gvt = 25

  case class EnqPacket(valid: Boolean, coreid: Int, time: Int, lp: Int, cancel: Int)
  def peekEnq(i: Int) = {
    EnqPacket(
      peek(c.io.dbg.enqueue(i).valid) != BigInt(0),
      peek(c.io.dbg.enqueue(i).bits.tag).toInt,
      peek(c.io.dbg.enqueue(i).bits.msg.time).toInt,
      peek(c.io.dbg.enqueue(i).bits.msg.lp_id).toInt,
      peek(c.io.dbg.enqueue(i).bits.msg.cancel_evt).toInt
    )
  }

  case class IssuePacket(valid: Boolean, coreid: Int, time: Int, lp: Int, cancel: Int, gvt: Int)
  def peekIssue(i: Int) = {
    IssuePacket(
      peek(c.io.dbg.issue(i).valid) != BigInt(0),
      peek(c.io.dbg.issue(i).bits.tag).toInt,
      peek(c.io.dbg.issue(i).bits.msg.time).toInt,
      peek(c.io.dbg.issue(i).bits.msg.lp_id).toInt,
      peek(c.io.dbg.issue(i).bits.msg.cancel_evt).toInt,
      peek(c.io.dbg.gvt).toInt
    )
  }

  case class StartPacket(valid: Boolean, coreid: Int, lp:Int, gvt:Int)
  def peekStart(i:Int)={
    StartPacket(
      peek(c.io.dbg.start(i).valid) != BigInt(0),
      peek(c.io.dbg.start(i).bits.core_id).toInt,
      peek(c.io.dbg.start(i).bits.lp_id).toInt,
      peek(c.io.dbg.gvt).toInt
    )
  }

  case class GenPacket(valid: Boolean, coreid: Int, time: Int, lp: Int, cancel: Int, gvt: Int)
  def peekGen(i: Int) = {
    GenPacket(
      peek(c.io.dbg.gen(i).valid) != BigInt(0),
      peek(c.io.dbg.gen(i).bits.tag).toInt,
      peek(c.io.dbg.gen(i).bits.msg.time).toInt,
      peek(c.io.dbg.gen(i).bits.msg.lp_id).toInt,
      peek(c.io.dbg.gen(i).bits.msg.cancel_evt).toInt,
      peek(c.io.dbg.gvt).toInt
    )
  }

  case class FinishPacket(valid: Boolean, coreid: Int, lp: Int, gvt: Int)
  def peekFinish(i: Int) = {
    FinishPacket(
      peek(c.io.dbg.finish(i).valid) != BigInt(0),
      peek(c.io.dbg.finish(i).bits.core_id).toInt,
      peek(c.io.dbg.finish(i).bits.last_lp).toInt,
      peek(c.io.dbg.gvt).toInt
    )
  }

  poke(c.io.start, 0)
  poke(c.io.target_gvt, target_gvt)
  step(1)
  poke(c.io.start, 1)

  case class Event(lp: Int, time: Int, cancel: Int)
  val core_event = mutable.Seq.fill(Specs.num_cores)(Event(0, -1, 0))
  val core_started = mutable.Seq.fill(Specs.num_cores)(-1)
  var gvt = 0

  val pq = Seq.fill(Specs.num_queues)(mutable.PriorityQueue.empty[Event](Ordering.by(x => -x.time)))
  /* replicate event initialization */
  for(cnt <- 0 until (Specs.num_events/Specs.num_queues)){
    for(q<- 0 until Specs.num_queues){
      pq(q)+= Event(q*Specs.num_lp/Specs.num_queues + cnt, cnt, 0)
    }
  }
  println(s"${pq(0)}")

  val rollback_q = mutable.Seq.fill(Specs.num_cores)(mutable.ArrayBuffer.empty[Int])
  val event_q = mutable.Seq.fill(Specs.num_lp)(mutable.ArrayBuffer.empty[Int])
  val cancel_q = mutable.Seq.fill(Specs.num_lp)(mutable.ArrayBuffer.empty[Int])

  var cycle = 1
  while(cycle < max_cycle && peek(c.io.done.valid) == BigInt(0)){
    (0 until Specs.num_queues).foreach { q =>
      val issue = peekIssue(q)
      if (issue.valid) {
        val issued_event = Event(issue.lp, issue.time, issue.cancel)
        assert(pq(q).nonEmpty, s"Deque from empty queue $q")
        expect(pq(q).head.time == issue.time, s"Issued non-smallest event $issued_event from queue $q -- Actual ${pq(q).head}")
        expect(pq(q).exists(_ == issued_event), "No matching event in queue")

        /* Remove the matching event from queue, the exact event may not be at the head of test queue,
         * so we remove until the first exact match, then reinsert non-exact matches */
        var deqs = mutable.ArrayBuffer.empty[Event]
        var d = pq(q).dequeue()
        while (d != issued_event) {
          deqs += d
          assert(pq(q).nonEmpty, s"trying to dequeue from empty queue $q")
          d = pq(q).dequeue()
        }
        pq(q) ++= deqs

        /* Core should be empty, assign event to core */
        expect(core_event(issue.coreid).time < 0, "Issueing event to occupied core")
        core_event(issue.coreid) = Event(issue.lp, issue.time, issue.cancel)

        expect(issue.gvt <= issue.time, "GVT bigger than event time")
        gvt = issue.gvt
      }
    }

    (0 until Specs.num_queues).foreach { q =>
      val start = peekStart(q)
      if (start.valid) {
        val this_evt = core_event(start.coreid)

        /* Check that no other conflicting events are being processed in active cores */
        expect(!core_started.contains(this_evt.lp), "Start signal may cause conflict")
        core_started(start.coreid) = this_evt.lp

        /* Discard events past GVT */
        event_q(this_evt.lp) = event_q(this_evt.lp).filter(_ >= gvt)
        if (this_evt.cancel > 0) { // anti-message
          /* check for anti-msg target in the event queue */
          if (event_q(this_evt.lp).contains(this_evt.time)) {
            /* Event has been processed, remove it from the processed list, to prevent generating rollback later */
            event_q(this_evt.lp) -= this_evt.time
          } else {
            /* keep a record of unmatched anti-messages */
            cancel_q(this_evt.lp) += this_evt.time
          }
        } else { // Regular event
          /* keep track of events that should later be rolled back, prune event_list */
          rollback_q(start.coreid) = event_q(this_evt.lp).filter(_ > this_evt.time)
          event_q(this_evt.lp) = event_q(this_evt.lp).filterNot(_ > this_evt.time)

          if (cancel_q(this_evt.lp).contains(this_evt.time)) {
            /* event has a cancellation message waiting, don't insert into processed list
             * remove cancellation message from cancellation list */
            cancel_q(this_evt.lp) -= this_evt.time
          } else {
            event_q(this_evt.lp) += this_evt.time // Insert to processed events list
          }
        }
      }
    }

    (0 until Specs.num_queues).foreach { q =>
      val enq = peekEnq(q)
      if(enq.valid){ // Update queue with new received events
        pq(q)+= Event(enq.lp, enq.time, enq.cancel)
      }
    }

    (0 until Specs.num_cores).foreach { cid =>
      val gen = peekGen(cid)
      if (gen.valid) {
        val this_evt = core_event(cid)
        if (gen.lp == this_evt.lp) { // These are rollback events
          if (rollback_q(cid).contains(gen.time)) {
            rollback_q(cid) -= gen.time // Remove the rollback event from list
          }
        }
      }
    }

    (0 until Specs.num_cores).foreach { cid =>
      val fin = peekFinish(cid)
      if (fin.valid) {
        /* All rollback events in the list should have been sent already */
        expect(rollback_q(cid).isEmpty, s"Events remains for rollback when core returns")
        core_event(cid) = Event(-1, -1, -1) // Mark core inactive
        core_started(cid) = -1
      }
    }

    step(1)
    cycle = cycle + 1
  }
  if(peek(c.io.done.bits) >= target_gvt) println("+++ Simulation reached target GVT +++")
  if(cycle > max_cycle) println("+++ Simulation ended due to reaching maximum number of cycles +++")
}


class PDESATest extends FreeSpec with Matchers {
  val options: Array[String] = Array(
//    "--backend-name", "firrtl",
    "--backend-name", "verilator",
//    "--is-verbose",
    "--split-modules",
    "--fint-write-vcd"
  )

  "Simulation should end with proper GVT" in {
    chisel3.iotesters.Driver.execute(
      options,
      () => new PDESA) { c =>
      new PDESATester(c)
    } should be(true)
  }
}
