package pdesa

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util.Cat  // can't use chisel3_ version because of compile order

// scalastyle:off magic.number
object LFSR {
  def apply(increment: Bool = true.B, seed: Int = 1): UInt = {
    val width = 16
    val lfsr = RegInit(seed.U(width.W))
    when (increment) { lfsr := Cat(lfsr(0)^lfsr(2)^lfsr(3)^lfsr(5), lfsr(width-1,1)) }
    lfsr
  }
}
