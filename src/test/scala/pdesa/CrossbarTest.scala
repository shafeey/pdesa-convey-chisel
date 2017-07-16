package pdesa

import chisel3._
import chisel3.util._
import chisel3.iotesters._
import org.scalatest.{FreeSpec, Matchers}

import scala.collection.mutable


class CrossbarWrapper extends Module{
  private val n_si = CrossbarTestSpecs.n_si
  private val n_mi = CrossbarTestSpecs.n_mi

  private val dtype = CrossbarTestSpecs.dtype
  private val cb_entry = Wire(Flipped(new CrossbarEntry(dtype, n_si)))

  val io = IO(new Bundle{
    val mi = Flipped(Vec(n_mi, Decoupled(UInt(cb_entry.getWidth.W))))
    val si = Vec(n_si, Decoupled(UInt(dtype.getWidth.W)))
  })

  val cb_module = Module(new Crossbar(dtype, n_mi, n_si))

  for(i<- 0 until n_mi){
    cb_module.io.mi(i).bits := io.mi(i).bits.asTypeOf(new CrossbarEntry(dtype, n_si))
    cb_module.io.mi(i).valid := io.mi(i).valid
    io.mi(i).ready := cb_module.io.mi(i).ready
  }
  for(i<- 0 until n_si){
    io.si(i) <> cb_module.io.si(i)
  }

}

class CrossbarTest(c: CrossbarWrapper) extends AdvTester(c) {

  private val num_of_samples = CrossbarTestSpecs.n_sample
  private val n_si = CrossbarTestSpecs.n_si
  private val n_mi = CrossbarTestSpecs.n_mi
  private val dtype = CrossbarTestSpecs.dtype

  def makeWrappedData(addr: Int, data: Int) : Int = {
    (addr << CrossbarTestSpecs.dtype.getWidth) + data
  }

  private val output_handlers = c.io.si.map(IrrevocableSink(_))
  private val input_drivers = c.io.mi.map(DecoupledSource(_))

  private val pushed_data = Seq.fill(n_si)(new mutable.ListBuffer[Int])

  def addInput(addr: Int, data: Int, driver: DecoupledSource[UInt, BigInt]): Unit = {
    val inputdata = makeWrappedData(addr, data)
    pushed_data(addr) += data
    driver.inputs.enqueue(inputdata)
  }

  def rndInput(driver: DecoupledSource[UInt, BigInt]) : Unit =
                      addInput(rnd.nextInt(n_si), rnd.nextInt((1 << dtype.getWidth) - 1), driver)
  // give each engine something to do
  for((driver, index) <- input_drivers.zipWithIndex) {
    rndInput(driver)
  }

  private var sampleCount = input_drivers.size
  private var resultCount = 0

  while(resultCount < num_of_samples) {
    output_handlers.zipWithIndex.foreach { case (handler, index) =>
      if(handler.outputs.nonEmpty) {
        resultCount += 1
        val result = handler.outputs.dequeue()
        val expected = pushed_data(index).contains(result.toInt)
        if(expected) pushed_data(index) -= result.toInt

        assert(expected, f"handler $index%d got event that isn't inserted: $result")

        if (sampleCount < num_of_samples) {
          rndInput(input_drivers(rnd.nextInt(n_mi)))
        }
      }
    }
    takestep()
  }

}

object CrossbarTestSpecs {
  val n_sample = 1000

  val n_mi = 8
  val n_si = 4
  val dtype = 0.U(16.W)
}

class CrossbarTester extends FreeSpec with Matchers {
  "Crossbar should deliver all inputs properly" - {
    "using interpreter" in {
      chisel3.iotesters.Driver.execute(
        Array("--backend-name", "firrtl", "--fint-write-vcd"),
        () => new CrossbarWrapper) { c =>
        new CrossbarTest(c)
      } should be(true)
    }
  }
}