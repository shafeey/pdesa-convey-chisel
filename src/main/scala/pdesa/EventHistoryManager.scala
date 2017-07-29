package pdesa

import chisel3._
import chisel3.util._

class EventHistoryManager extends Module {
  val io = IO {
    new Bundle {
      val hist_req = Flipped(Decoupled(new EventHistoryReq(Specs.lp_bits, Specs.time_bits)))
      val hist_rsp = Valid(new EventHistoryRsp(Specs.lp_bits, Specs.time_bits))
    }
  }

  val req = Queue(io.hist_req, 1, pipe = true)

  val lp = Reg(req.bits.origin_lp.cloneType)
  val count = RegInit(req.bits.count.cloneType, init = 0.U)

  val mem = SyncReadMem(Specs.hist_size * Specs.num_lp, req.bits.msg)
  val mem_rd = Wire(req.bits.msg)
  val addr = Cat(req.bits.origin_lp, Mux(req.bits.op === EventHistroyCmnd.sOP_RD, count, req.bits.count))

  val sIDLE :: sREAD :: sWRITE :: Nil = Enum(3)
  val state = RegInit(sIDLE)


  val r_EP = RegNext(req.bits.EP_id)
  val r_op = RegNext(req.bits.op)
  val r_tag = RegNext(req.bits.count)
  val r_valid = RegInit(false.B)


  io.hist_rsp.bits.EP_id := r_EP
  io.hist_rsp.bits.op := r_op
  io.hist_rsp.bits.tag := Mux(req.bits.op === EventHistroyCmnd.sOP_RD, count, r_tag)
  io.hist_rsp.bits.msg := mem.read(addr, true.B)
  io.hist_rsp.valid := r_valid

  req.nodeq()

  r_valid := false.B
  switch(state) {
    is(sIDLE) {
      when(req.valid && req.bits.op === EventHistroyCmnd.sOP_RD) {
//        mem_rd := mem.read(addr)
        printf(p"Reading history from addr: $addr\n")
        r_valid := true.B
        when(req.bits.count > 1.U) {
          state := sREAD
          count := count + 1.U
        }.otherwise{
          req.deq()
        }
      }.elsewhen(req.valid && req.bits.op === EventHistroyCmnd.sOP_WR){
        r_valid := true.B
        mem.write(addr, req.bits.msg)
        printf(p"Writing history at addr: $addr, data: ${req.bits.msg}\n")
        req.deq()
      }
    }
    is(sREAD) {
//      mem_rd := mem.read(addr)
      count := count + 1.U
      r_valid := true.B
      when(count === req.bits.count - 1.U){
        state := sIDLE
        count := 0.U
        req.deq()
      }
    }
  }
}
