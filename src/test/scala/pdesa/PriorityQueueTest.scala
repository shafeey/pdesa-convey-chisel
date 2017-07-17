package pdesa

import chisel3._
import chisel3.util._
import chisel3.iotesters._
import chisel3.testers.TesterDriver

class PriorityQueueTester extends SteppedHWIOTester{
  val number_of_stages = 5

  val device_under_test = Module(new PriorityQueue(0.U(16.W), 0.U(8.W), number_of_stages))
  val c = device_under_test
  enable_all_debug = true

  val pq = scala.collection.mutable.PriorityQueue.empty[Int](Ordering.by(-_))

  def enqueue(p: Int) : Unit = {
    poke(c.io.in.priority, p)
    poke(c.io.op, 1)
    pq += p
  }

  def dequeue() : Unit = {
    poke(c.io.op, 2)
    expect(c.io.out.priority, pq.dequeue())
  }

  def rest() : Unit = {
    poke(c.io.op, 0)
    expect(c.io.count, pq.size)
  }

  private def ins(i: Int) = {enqueue(i); step(1); rest(); step(1)}
  private def rem() = {dequeue(); step(1); rest(); step(1)}

//  (14 to 1 by -1).map(ins(_))
//  step(4)
//  (1 to 14).map(_ => rem())

  rnd.setSeed(14L)

  var last_op = 0
  for (i<- 0 until 1000){
    var op = rnd.nextInt(3)
    if(last_op > 0) op = 0

    if(op == 0) rest()

    if(op == 1 && pq.size < (1<<number_of_stages)-1){
      enqueue(rnd.nextInt(100))
    }
    if(op == 2 && pq.nonEmpty){
      dequeue()
    }

    last_op = op
    step(1)
  }
}

class PriorityQueueTest extends ChiselFlatSpec {
  "Priority Queue" should "keep elements in order" in {
      assertTesterPasses { new PriorityQueueTester }
  }
}