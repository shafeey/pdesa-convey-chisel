`timescale 1 ns / 1 ps

module tb_user();

  initial begin
    // Insert user code here, such as signal dumping
    // set CNY_PDK_TB_USER_VLOG variable in sim/makefile
    //$wlfdumpvars(1,testbench.ae0.same_pers.cae_pers);
    //$wlfdumpvars(1,testbench.ae1.same_pers.cae_pers);
    //$wlfdumpvars(1,testbench.ae2.same_pers.cae_pers);
    //$wlfdumpvars(1,testbench.ae3.same_pers.cae_pers);
   $dumpfile("dump.vcd");
   $dumpvars(5,testbench.ae0.same_pers.cae_pers);
 //   $dumpvars(1,testbench.ae1.same_pers.cae_pers);
 //   $dumpvars(1,testbench.ae2.same_pers.cae_pers);
 //   $dumpvars(1,testbench.ae3.same_pers.cae_pers);
$dumpon;
//#30000
//$dumpoff;
  end

endmodule

