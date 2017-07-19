package pdesa

import chisel3._
import chisel3.iotesters._

class PriorityQueueTester extends SteppedHWIOTester {
  val number_of_stages = 5

  val device_under_test = Module(new PriorityQueue(0.U(16.W), 0.U(8.W), number_of_stages))
  val c = device_under_test
  enable_all_debug = false

  val pq = scala.collection.mutable.PriorityQueue.empty[Int](Ordering.by(-_))

  def enqueue(p: Int): Unit = {
    poke(c.io.in.priority, p)
    poke(c.io.op, 1)
    pq += p
  }

  def dequeue(): Unit = {
    poke(c.io.op, 2)
    expect(c.io.out.priority, pq.dequeue())
  }

  def replace(p: Int): Unit = {
    expect(c.io.out.priority, pq.dequeue())
    poke(c.io.op, 3)
    poke(c.io.in.priority, p)
    pq += p
  }

  def rest(): Unit = {
    poke(c.io.op, 0)
    expect(c.io.count, pq.size)
  }

  private def ins(i: Int) = {
    enqueue(i); step(1); rest(); step(1)
  }

  private def rem() = {
    dequeue(); step(1); rest(); step(1)
  }

  def randomTest(cycles: Int): Unit = {
    var last_op = 0
    for (i <- 0 until cycles) {
      var op = rnd.nextInt(4)
      if (last_op > 0) op = 0
      if (op == 0) rest()
      if (op == 1 && pq.size < (1 << number_of_stages) - 1) {
        enqueue(rnd.nextInt(100))
      }
      if (op == 2 && pq.nonEmpty) {
        dequeue()
      }
      if (op == 3 && pq.nonEmpty) replace(rnd.nextInt(100))

      last_op = op
      step(1)
    }
  }

  rnd.setSeed(17L)

  // Warm up Queue to half full
  for (i <- 0 until (1 << number_of_stages) by 2) {
    enqueue(i)
    step(1)
    rest()
    step(1)
  }

  // Start random test
  randomTest(100)

  // make queue empty
  while (pq.nonEmpty) {
    rest()
    step(1)
    dequeue()
    step(1)
    rest()
    step(1)
  }

  randomTest(100)
}

class PriorityQueueTest extends ChiselFlatSpec {
  "Priority Queue" should "keep elements in order" in {
    assertTesterPasses {
      new PriorityQueueTester
    }
  }
}