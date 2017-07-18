package pdesa

import chisel3._
import chisel3.util._

class PriorityQueue[T <: Data, P <: UInt](dtype: T, ptype : P, num_stages : Int) extends Module{
  val sNOP :: sENQUEUE :: sDEQUEUE :: sREPLACE :: Nil = Enum(4)

  val io = IO(new PriorityQueueIO)

  val last_op = RegInit(init = sNOP)
  val ready_for_op = last_op === sNOP
  last_op := Mux(ready_for_op, io.op, sNOP)

  val levels = for(i<- 1 until num_stages) yield{
    Module(new PriorityQueueHeapLevel(stage_id = i))
  }

  val head = Module(new PriorityQueueHeapLevel(stage_id = 1))
  head.io.top.index := 0.U
  head.io.top.op := Mux(ready_for_op, io.op, sNOP)
  head.io.top.data := io.in
  io.out := head.io.top.l_data

  head.io.bot <> levels(0).io.top
  for(i<- 1 until num_stages - 1){
    levels(i-1).io.bot <> levels(i).io.top
  }

  val count = RegInit(0.U(num_stages.W))
  switch(io.op){
    is(sENQUEUE){
      count := count + 1.U
    }
    is(sDEQUEUE){
      count := count - 1.U
    }
  }
  io.count := count

  // PQ internal
  class PriorityQueueIO extends Bundle{
    val in = Input(new PriorityQueueBundle)
    val out = Output(new PriorityQueueBundle)
    val count = Output(UInt(num_stages.W))
    val op = Input(UInt(2.W))
  }

  class PriorityQueueBundle  extends Bundle{
    val data = dtype.cloneType
    val priority = ptype.cloneType

    override def cloneType: PriorityQueueBundle.this.type = (new PriorityQueueBundle).asInstanceOf[this.type]
  }

  class PriorityQueueHeapIO extends Bundle{
    val data = Input(new PriorityQueueBundle)
    val op = Input(UInt(2.W))
    val index = Input(UInt(num_stages.W))
    val ch_index = Input(UInt(num_stages.W))

    val capacity = Output(UInt(num_stages.W))
    val l_data = Output(new PriorityQueueBundle)
    val r_data = Output(new PriorityQueueBundle)
    val l_occupied = Output(Bool())
    val r_occupied = Output(Bool())

    override def cloneType: PriorityQueueHeapIO.this.type = (new PriorityQueueHeapIO).asInstanceOf[this.type]
  }

  class PriorityQueueHeapLevel(stage_id: Int) extends Module{
    val io = IO(new Bundle{
      val top = new PriorityQueueHeapIO
      val bot = Flipped(new PriorityQueueHeapIO)
    })

    val sLEFT :: sRIGHT :: Nil = Enum(2)

    val mem = Seq.fill(2)(Mem(1 << (stage_id - 1) , new PriorityQueueBundle))
    val occupied = Seq.fill(2)(RegInit(init = Vec(Seq.fill(1 << (stage_id-1))(false.B))))
    val capacity = Seq.fill(2)(RegInit(init = Vec(Seq.fill(1 << (stage_id-1))(((1<< (num_stages - stage_id))  - 1).U))))
    val trans_op = RegInit(sNOP)
    val trans_val = Reg(new PriorityQueueBundle)
    val trans_idx = RegInit(0.U(num_stages.W))

    io.bot.index := trans_idx
    io.bot.data := trans_val
    io.bot.op := trans_op
    io.bot.ch_index := io.top.index

    val pos = Wire(UInt((num_stages-1).W))
    val side = Wire(Bool())
    side := io.top.index(0)
    pos := io.top.index >> 1.U

    val par_idx = io.top.ch_index

    private val mem_rd_val_tmp = Seq.fill(2)(Wire(new PriorityQueueBundle))
    private val trans_val_tmp = Seq.fill(2)(Wire(new PriorityQueueBundle))
    private val nxt_val_tmp = Seq.fill(2)(Wire(new PriorityQueueBundle))
    private val trans_dest_tmp = Seq.fill(2)(Wire(Bits(1.W)))

    trans_op := sNOP

    /* Dequeue and replace operations read two values from the child level. These values
     * are read from two consecutive address. Therefore the memory and relevant signals
     * have been split into two groups odd(left) and even(right). The logic for each group
     * has been explicitly stated so that the synthesizer doesn't confuse them as a single
     * memory with two read port.
     */

    switch(io.top.op){
      is(sENQUEUE){
        (0 to 1).map(s => {
          when(s.U === side){
            occupied(s)(pos) := true.B
            capacity(s)(pos) := capacity(s)(pos) - 1.U
            trans_val_tmp(s) := io.top.data
            when(occupied(s)(pos)){
              trans_op := sENQUEUE
              mem_rd_val_tmp(s) := mem(s).read(pos)
              when(io.top.data.priority < mem_rd_val_tmp(s).priority){
                mem(s).write(pos, io.top.data)
                trans_val_tmp(s) := mem_rd_val_tmp(s)
              }
            }.otherwise{
              mem(s).write(pos, io.top.data)
            }
          }
        })

        trans_idx := Cat(io.top.index, Mux(io.bot.capacity > 0.U, sLEFT, sRIGHT))
        trans_val := Mux(side === 0.U, trans_val_tmp(0), trans_val_tmp(1))
      }
      is(sDEQUEUE){
        trans_op := sDEQUEUE
        (0 to 1).map(s => {
          when(s.U === side) {
            capacity(s)(pos) := capacity(s)(pos) + 1.U
            when(io.bot.l_occupied && io.bot.r_occupied){
              when(io.bot.l_data.priority < io.bot.r_data.priority){
                nxt_val_tmp(s) := io.bot.l_data
                trans_dest_tmp(s) := sLEFT
              }.otherwise{
                nxt_val_tmp(s) := io.bot.r_data
                trans_dest_tmp(s) := sRIGHT
              }
            }.elsewhen(io.bot.l_occupied){
              nxt_val_tmp(s) := io.bot.l_data
              trans_dest_tmp(s) := sLEFT
            }.elsewhen(io.bot.r_occupied){
              nxt_val_tmp(s) := io.bot.r_data
              trans_dest_tmp(s) := sRIGHT
            }.otherwise{
              occupied(s)(pos) := false.B
              nxt_val_tmp(s) := 0.U.asTypeOf(new PriorityQueueBundle)
              trans_op := sNOP
            }

            mem(s).write(pos, nxt_val_tmp(s))
          }
        })
        trans_idx := Cat(io.top.index, Mux(side === 0.U, trans_dest_tmp(0), trans_dest_tmp(1)))
      }
      is(sREPLACE){
        trans_op := sREPLACE
        (0 to 1).map(s =>{
          when(s.U === side){
            when(io.bot.l_occupied && io.bot.r_occupied){
              when(io.bot.l_data.priority < io.bot.r_data.priority){
                when(io.bot.l_data.priority < io.top.data.priority){
                  nxt_val_tmp(s) := io.bot.l_data
                  trans_dest_tmp(s) := sLEFT
                  trans_val_tmp(s) := io.top.data
                }.otherwise{
                  nxt_val_tmp(s) := io.top.data
                  trans_op := sNOP
                }
              }.otherwise{
                when(io.bot.r_data.priority < io.top.data.priority){
                  nxt_val_tmp(s) := io.bot.r_data
                  trans_dest_tmp(s) := sRIGHT
                  trans_val_tmp(s) := io.top.data
                }.otherwise{
                  nxt_val_tmp(s) := io.top.data
                  trans_op := sNOP
                }
              }
            }.elsewhen(io.bot.l_occupied){
              when(io.bot.l_data.priority < io.top.data.priority){
                nxt_val_tmp(s) := io.bot.l_data
                trans_dest_tmp(s) := sLEFT
                trans_val_tmp(s) := io.top.data
              }.otherwise{
                nxt_val_tmp(s) := io.top.data
                trans_op := sNOP
              }
            }.elsewhen(io.bot.r_occupied){
              when(io.bot.r_data.priority < io.top.data.priority){
                nxt_val_tmp(s) := io.bot.r_data
                trans_dest_tmp(s) := sRIGHT
                trans_val_tmp(s) := io.top.data
              }.otherwise{
                nxt_val_tmp(s) := io.top.data
                trans_op := sNOP
              }
            }

            mem(s).write(pos, nxt_val_tmp(s))
          }
        })

        trans_val := Mux(side === 0.U, trans_val_tmp(0), trans_val_tmp(1))
        trans_idx := Cat(io.top.index, Mux(side === 0.U, trans_dest_tmp(0), trans_dest_tmp(1)))
      }
      is(sNOP){
        io.top.capacity := capacity(0)(par_idx)
        io.top.l_occupied:= occupied(0)(par_idx)
        io.top.r_occupied:= occupied(1)(par_idx)
        io.top.l_data := mem(0).read(par_idx)
        io.top.r_data := mem(1).read(par_idx)
      }
    }
  }
}

object PriorityQueue extends App{
  chisel3.Driver.execute(args, () => new PriorityQueue(0.U(8.W), 0.U(8.W), 5))
}