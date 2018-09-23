package pdesa

import chisel3._
import chisel3.util._

class EventMsg(lp_id_bits: Int, time_bits: Int) extends Bundle {
  val lp_id = UInt(lp_id_bits.W)
  val time = UInt(time_bits.W)
  val cancel_evt = Bool()
  val event_type = UInt(2.W)

  override def cloneType: EventMsg.this.type = new EventMsg(lp_id_bits, time_bits).asInstanceOf[this.type]

  def setValue(lp_id: UInt, time: UInt, event_type: UInt, cancel: Bool): Unit = {
    this.lp_id := lp_id
    this.time := time
    this.event_type := event_type
    this.cancel_evt := cancel
  }
}

class EventHistoryMsg(lp_bits: Int, time_bits: Int) extends Bundle {
  val origin_time = UInt(time_bits.W)
//  val target_time = UInt(time_bits.W)
//  val target_lp = UInt(lp_bits.W)
  val cancel_evt = Bool()
  val rng_seed = UInt(16.W) // LFSR used has 16 bits seed
  val event_type = UInt(2.W)
  val branch_taken = new EventHistoryBranchPath

//  def hist_wid: Int = time_bits + time_bits + lp_bits + 1 + 16 + 2
  def hist_wid: Int = time_bits + 1 + 16 + 2 + branch_taken.getWidth

  override def cloneType: EventHistoryMsg.this.type =
    new EventHistoryMsg(lp_bits, time_bits).asInstanceOf[this.type]
}

class EventHistoryBranchPath extends Bundle{
  val landing_slot_open = Bool()
}

object EventHistroyCmnd {
  val sOP_RD :: sOP_WR :: Nil = Enum(2)
}

class EventHistoryReq(lp_bits: Int, time_bits: Int) extends Bundle {
  val origin_lp = UInt(Specs.lp_bits.W)
  val EP_id = UInt(Specs.core_bits.W)
  val count = UInt(log2Ceil(Specs.hist_size).W)
  val op = UInt(1.W)
  val msg = new EventHistoryMsg(lp_bits, time_bits)

  def get_target_queue_addr: UInt = {
    val lower_bits_rev = Reverse(origin_lp)(log2Ceil(Specs.num_queues)-1, 0)
    Reverse(lower_bits_rev)
  }

  override def cloneType: EventHistoryReq.this.type =
    new EventHistoryReq(lp_bits, time_bits).asInstanceOf[this.type]

  def setRead(EP: UInt, LP: UInt, cnt: UInt): Unit = {
    origin_lp := LP
    EP_id := EP
    count := cnt
    op := EventHistroyCmnd.sOP_RD
  }

  def setWrite(EP: UInt, LP: UInt, pos: UInt, msg: EventHistoryMsg): Unit = {
    origin_lp := LP
    EP_id := EP
    count := pos
    this.msg := msg
    op := EventHistroyCmnd.sOP_WR
  }
}

class EventHistoryRsp(lp_bits: Int, time_bits: Int) extends Bundle {
  val msg = new EventHistoryMsg(lp_bits, time_bits)
  val EP_id = UInt(Specs.core_bits.W)
  val op = UInt(1.W)
  val tag = UInt(log2Ceil(Specs.hist_size).W)

  override def cloneType: EventHistoryRsp.this.type =
    new EventHistoryRsp(lp_bits, time_bits).asInstanceOf[this.type]
}