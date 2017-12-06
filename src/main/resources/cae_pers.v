/*****************************************************************************/
//
// Module	   : cae_pers.vpp
//
//-----------------------------------------------------------------------------
//
// Original Author : gedwards
// Created On      : Wed Oct 10 09:26:08 2007
// Modified to work with Wrapper generated in Chisel3
//
//-----------------------------------------------------------------------------
//
// Description     : Top level personality file that instantiates the wrapper
//                   for accelerator implemented in Chisel3
//
//-----------------------------------------------------------------------------
//
// Copyright (c) 2007-2013 : created by Convey Computer Corp. This model is the
// confidential and proprietary property of Convey Computer Corp.
//
/*****************************************************************************/

`timescale 1 ns / 1 ps

`include "pdk_fpga_defines.vh"

(* keep_hierarchy = "true" *)
module cae_pers #(
   parameter    NUM_MC_PORTS = 8,
   parameter    RTNCTL_WIDTH = 32
) (
   //
   // Clocks and Resets
   //
   input		clk,		// Personalitycore clock
   input		clkhx,		// half-rate clock
   input		clk2x,		// 2x rate clock
   input		i_reset,	// global reset synchronized to clk

   //
   // Dispatch Interface
   //
   input                disp_inst_vld,
   input  [4:0]         disp_inst,
   input  [17:0]        disp_aeg_idx,
   input                disp_aeg_rd,
   input                disp_aeg_wr,
   input  [63:0]        disp_aeg_wr_data,

   output [17:0]        disp_aeg_cnt,
   output [15:0]        disp_exception,
   output               disp_idle,
   output               disp_rtn_data_vld,
   output [63:0]        disp_rtn_data,
   output               disp_stall,

   //
   // MC Interface(s)
   //
   output [NUM_MC_PORTS*1-1 :0]         mc_rq_vld,
   output [NUM_MC_PORTS*RTNCTL_WIDTH-1:0]         mc_rq_rtnctl,
   output [NUM_MC_PORTS*64-1:0]         mc_rq_data,
   output [NUM_MC_PORTS*48-1:0]         mc_rq_vadr,
   output [NUM_MC_PORTS*2-1 :0]         mc_rq_size,
   output [NUM_MC_PORTS*3-1 :0]         mc_rq_cmd,
   output [NUM_MC_PORTS*4-1 :0]         mc_rq_scmd,
   input  [NUM_MC_PORTS*1-1 :0]         mc_rq_stall,
   
   input  [NUM_MC_PORTS*1-1 :0]         mc_rs_vld,
   input  [NUM_MC_PORTS*3-1 :0]         mc_rs_cmd,
   input  [NUM_MC_PORTS*4-1 :0]         mc_rs_scmd,
   input  [NUM_MC_PORTS*64-1:0]         mc_rs_data,
   input  [NUM_MC_PORTS*RTNCTL_WIDTH-1:0]         mc_rs_rtnctl,
   output [NUM_MC_PORTS*1-1 :0]         mc_rs_stall,

   // Write flush 
   output [NUM_MC_PORTS*1-1 :0]         mc_rq_flush,
   input  [NUM_MC_PORTS*1-1 :0]         mc_rs_flush_cmplt,

   //
   // AE-to-AE Interface not used
   //

   //
   // Management/Debug Interface
   //
   input				csr_wr_vld,
   input				csr_rd_vld,
   input  [15:0]			csr_address,
   input  [63:0]			csr_wr_data,
   output				csr_rd_ack,
   output [63:0]			csr_rd_data,

   //
   // Miscellaneous
   //
   input  [3:0]		i_aeid
);



ConveyWrapper wrapper
(
  .clock            (clk),

  .io_dispInstValid	(disp_inst_vld),
  .io_dispInstData	(disp_inst),
  .io_dispRegID		(disp_aeg_idx),
  .io_dispRegRead	(disp_aeg_rd	),
  .io_dispRegWrite	(disp_aeg_wr	),
  .io_dispRegWrData	(disp_aeg_wr_data	),
  .io_dispAegCnt	(disp_aeg_cnt	),
  .io_dispException	(disp_exception	),
  .io_dispIdle		(disp_idle		),
  .io_dispRtnValid	(disp_rtn_data_vld	),
  .io_dispRtnData	(disp_rtn_data	),
  .io_dispStall		(disp_stall		),
  
  .io_mcReqValid	(mc_rq_vld	),
  .io_mcReqRtnCtl	(mc_rq_rtnctl	),
  .io_mcReqData		(mc_rq_data		),
  .io_mcReqAddr		(mc_rq_vadr		),
  .io_mcReqSize		(mc_rq_size		),
  .io_mcReqCmd		(mc_rq_cmd		),
  .io_mcReqSCmd		(mc_rq_scmd		),
  .io_mcReqStall	(mc_rq_stall	),
  .io_mcResValid	(mc_rs_vld	),
  .io_mcResCmd		(mc_rs_cmd		),
  .io_mcResSCmd		(mc_rs_scmd		),
  .io_mcResData		(mc_rs_data		),
  .io_mcResRtnCtl	(mc_rs_rtnctl	),
  .io_mcResStall	(mc_rs_stall	),
  .io_mcReqFlush	(mc_rq_flush	),
  .io_mcResFlushOK	(mc_rs_flush_cmplt	),
  .io_csrWrValid	(csr_wr_vld	),
  .io_csrRdValid	(csr_rd_vld	),
  .io_csrAddr		(csr_address		),
  .io_csrWrData		(csr_wr_data		),
  .io_csrReadAck	(csr_rd_ack	),
  .io_csrReadData	(csr_rd_data	),
  .io_aeid		    (i_aeid		),
  
  .reset		(i_reset)
);

   /* ---------- debug & synopsys off blocks  ---------- */

   // synopsys translate_off

   // Parameters: 1-Severity: Don't Stop, 2-start check only after negedge of reset
   //assert_never #(1, 2, "***ERROR ASSERT: unimplemented instruction cracked") a0 (.clk(clk), .reset_n(!i_reset), .test_expr(r_unimplemented_inst));

    // synopsys translate_on

endmodule // cae_pers
