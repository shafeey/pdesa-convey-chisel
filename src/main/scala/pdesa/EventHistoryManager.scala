package pdesa

import chisel3._
import chisel3.util._

class EventHistoryManager extends Module {
  val io = IO {
    new Bundle {
      val hist_req = Flipped(Vec(Specs.num_queues, Decoupled(new EventHistoryReq(Specs.lp_bits, Specs.time_bits))))
      val hist_rsp = Vec(Specs.num_queues, Decoupled(new EventHistoryRsp(Specs.lp_bits, Specs.time_bits)))
    }
  }

  val num_lp_per_grp = Specs.num_lp / Specs.num_queues
  val lp_bits_for_addr = if (num_lp_per_grp > 1) log2Ceil(num_lp_per_grp) else 1

  val hist_rsp = Seq.fill(Specs.num_queues)(Wire(io.hist_rsp.head.cloneType))
  val rsp_q = hist_rsp.map(t=> Module(new Queue(t.bits.cloneType, entries = 4)))
  rsp_q.map(_.io.enq).zip(hist_rsp).foreach(x => x._1 <> x._2)
  io.hist_rsp.zip(rsp_q.map(_.io.deq)).foreach(x=> x._1 <> x._2)
  val rsp_afull = rsp_q.map(_.io.count === 3.U)

  for (q <- 0 until Specs.num_queues) {
    val req = Queue(io.hist_req(q), 1, pipe = true)

    val lp = Reg(req.bits.origin_lp.cloneType)
    val count = RegInit(req.bits.count.cloneType, init = 0.U)

    val mem = SyncReadMem(Specs.hist_size * num_lp_per_grp, req.bits.msg)
    val mem_rd = Wire(req.bits.msg)
    val addr = Cat(req.bits.origin_lp(lp_bits_for_addr - 1, 0), Mux(req.bits.op === EventHistroyCmnd.sOP_RD, count, req.bits.count))

    val sIDLE :: sREAD :: sWRITE :: Nil = Enum(3)
    val state = RegInit(sIDLE)

    val r_EP = RegNext(req.bits.EP_id)
    val r_op = RegNext(req.bits.op)
    val r_tag = RegNext(req.bits.count)
    val r_valid = RegInit(false.B)

    hist_rsp(q).bits.EP_id := r_EP
    hist_rsp(q).bits.op := r_op
    hist_rsp(q).bits.tag := Mux(req.bits.op === EventHistroyCmnd.sOP_RD, count, r_tag)
    hist_rsp(q).valid := r_valid

    req.nodeq()
    r_valid := false.B
    switch(state) {
      is(sIDLE) {
        when(req.valid && req.bits.op === EventHistroyCmnd.sOP_WR) {
          when(!rsp_afull(q)) {
            r_valid := true.B
            mem.write(addr, req.bits.msg)
            printf(p"+++ Writing history at addr: $addr, data: ${req.bits.msg}\n")
            req.deq()
          }
        }.elsewhen(req.valid && req.bits.op === EventHistroyCmnd.sOP_RD) {
          io.hist_rsp(q).bits.msg := mem.read(addr, true.B)
          when(!rsp_afull(q)) {
            r_valid := true.B
            when(req.bits.count > 1.U) {
              state := sREAD
              count := count + 1.U
            }.otherwise {
              req.deq()
            }
          }

        }
      }
      is(sREAD) {
        when(!rsp_afull(q)) {
          r_valid := true.B
          count := count + 1.U
          when(count === req.bits.count - 1.U) {
            state := sIDLE
            count := 0.U
            req.deq()
          }
        }
      }
    }
  }
}
