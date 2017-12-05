package pdesa

import chisel3._
import chisel3.util._

class GVTResolver extends Module{
  val io = IO(new Bundle{
    val compute = Input(Bool())
    val last_processed_ts = Input(Vec(Specs.num_cores, UInt(Specs.time_bits.W)))
    val queue_min = Flipped(Valid(UInt(Specs.time_bits.W)))
    val gvt = Valid(UInt(Specs.time_bits.W))
  })

  val active_cores = (~0.U(Specs.num_cores.W)).asUInt()
  val r_ts = RegInit(Vec(Seq.fill(Specs.num_cores)(0.U(Specs.time_bits.W))))

  val gvt = RegInit(0.U(Specs.time_bits.W))
  val gvt_valid = RegInit(false.B)
  val compute = RegInit(false.B)
  val core_min = RegInit(0.U(Specs.time_bits.W))
  val queue_min = RegInit(0.U(Specs.time_bits.W))
  val last_vld_queue_min = RegInit(0.U(Specs.time_bits.W))
  when(io.queue_min.valid){last_vld_queue_min := io.queue_min.bits}

  val min_res = Module(new MinResolverScan)
  min_res.io.core_times.zip(io.last_processed_ts).foreach{case(m, c) => m := c}
  min_res.io.start := compute

  val sIDLE :: sCOMPUTE :: sVALID :: Nil = Enum(3)
  val state = RegInit(sIDLE)
  switch(state){
    is(sIDLE){
      when(io.compute){
        state := sCOMPUTE
        r_ts.zip(io.last_processed_ts).foreach{case(r,c) => r := c}
        queue_min := last_vld_queue_min
        compute := true.B
        gvt_valid := false.B
      }
    }
    is(sCOMPUTE){
      compute := false.B
      when(min_res.io.vld){
        state := sVALID
        core_min := min_res.io.min
      }
    }
    is(sVALID){
      gvt := Mux(core_min < queue_min, core_min, queue_min)
      state := sIDLE
      gvt_valid := true.B
    }
  }

  io.gvt.valid := gvt_valid
  io.gvt.bits := gvt
}

protected class MinResolverScan extends Module {
  val io = IO(new Bundle {
    val core_times = Input(Vec(Specs.num_cores, UInt(Specs.time_bits.W)))
    val start = Input(Bool())
    val min = Output(UInt(Specs.time_bits.W))
    val vld = Output(Bool())
  })

  val num_seg = 4
  val seg_min: Seq[UInt] = Seq.fill(num_seg)(Reg(UInt(Specs.time_bits.W)))
  val elem_per_seg = Specs.num_cores/num_seg
  val seg_elems = for (s <- 0 until num_seg) yield{
    val w = Wire(Vec(elem_per_seg, UInt(Specs.time_bits.W)))
    for(i<- 0 until elem_per_seg){
      w(i) := io.core_times(s*elem_per_seg + i)
    }
    w
  }

  val counter = RegInit(0.U(log2Up(elem_per_seg).W))

  val sIDLE :: sSCAN :: sREDUCE :: Nil = Enum(3)
  val state = RegInit(sIDLE)

  val r_vld = RegInit(false.B)
  val r_min = Reg(UInt(Specs.time_bits.W))

  for(s <- 0 until num_seg){
    val elem = seg_elems(s)(counter)
    seg_min(s) := Mux(seg_min(s) > elem && state === sSCAN, elem, seg_min(s))
  }

  switch(state){
    is(sIDLE){
      r_vld := false.B
      when(io.start){
        seg_min.foreach(_ := (~0.U(Specs.time_bits.W)).asUInt())
        counter := 0.U
        state := sSCAN
      }
    }
    is(sSCAN){
      counter := counter + 1.U
      when(counter === elem_per_seg.U){
        state := sREDUCE
      }
    }
    is(sREDUCE){
      r_min := seg_min.reduce((a,b) => Mux(a < b, a, b))
      r_vld := true.B
      state := sIDLE
    }
  }

  io.min := r_min
  io.vld := r_vld
}
