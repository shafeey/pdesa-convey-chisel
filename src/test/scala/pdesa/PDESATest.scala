package pdesa

import chisel3._
import chisel3.iotesters._
import org.scalatest.{FreeSpec, Matchers}


class PDESATester(c: PDESA) extends PeekPokeTester(c){
  var cycle = 0
  val max_cycle = 1000
  val target_gvt = 100
  poke(c.io.target_gvt, target_gvt)
  poke(c.io.start, 1)

  while(cycle < max_cycle && peek(c.io.done.valid) == BigInt(0)){
    assert(peek(c.io.target_gvt) > target_gvt, "Simulation ended prematurely")
    cycle = cycle + 1
  }
  assert(cycle < max_cycle, "Exceeded maximum number of cycles")
}

class PDESATest extends FreeSpec with Matchers {
  "Simulation should end with proper GVT" - {
//    "using interpreter" in {
      chisel3.iotesters.Driver.execute(
        Array("--backend-name", "firrtl", "--fint-write-vcd"),
        () => new PDESA) { c =>
        new PDESATester(c)
      } should be(true)
    }
//  }
}