package pdesa

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util.Cat  // can't use chisel3_ version because of compile order

// scalastyle:off magic.number
object LFSR {
  val width = 16
  def apply(increment: Bool = true.B, seed: Int = 1): UInt = {
    val lfsr = RegInit(seed.U(width.W))
    when (increment) { lfsr := Cat(lfsr(0)^lfsr(2)^lfsr(3)^lfsr(5), lfsr(width-1,1)) }
    lfsr
  }

  def next_state_comb(seed: UInt = 1.U(width.W)) : UInt ={
    Cat(seed(0)^seed(2)^seed(3)^seed(5),seed(width-1,1))
  }
}
