//
// White Rabbit Core Hands-On Course
//
// Lesson 04a: Trivial streamer demo
//
// Objectives:
// - Demonstrate pulse distribution example on a simulation
//
// Brief description:
// Testbench instantiates two SPEC cards connected to each other via a Gigabit
// Ethernet link. SPEC A sends input trigger pulses to SPEC B, which reproduces them with
// fixed, 20us delay.

`timescale 10fs/10fs // need very fine timestep to correctly simulate the GTP.

module main;

   // Parameters
   
   // Reference clock period.
   parameter g_ref_clock_period = 8ns;

   reg clk_20m = 0, clk_ref = 0;
   wire uart_txd;
   wire [7:0] uart_data;
   wire uart_data_valid;

   // Generate the reference clock
   always #(g_ref_clock_period / 2) clk_ref <= ~clk_ref;

   // Generate the 20 MHz VCXO clock
   always #(50ns / 2) clk_20m <= ~clk_20m;

   reg  pulse_in = 0;
   wire pulse_out;
   wire [4:0] dio_out_b;
   
   
   
   // This time we have two SPECs talking to each other in the same testbench
   spec_top
     #(
       .g_simulation (1)
    ) SPEC_A (
           .clk_125m_pllref_p_i (clk_ref),
           .clk_125m_pllref_n_i (~clk_ref),

           .fpga_pll_ref_clk_101_p_i (clk_ref),
           .fpga_pll_ref_clk_101_n_i (~clk_ref),

           .clk_20m_vcxo_i(clk_20m),

           // Connect the gigabit output of one SPEC with the RX input of the other,
           // and vice-versa.
           .sfp_txp_o(a_to_b_p),
           .sfp_txn_o(a_to_b_n),

           .sfp_rxp_i(b_to_a_p),
           .sfp_rxn_i(b_to_a_n),

           .dio_p_i( {3'b0, pulse_in, 1'b0} ),
           .dio_n_i( {3'b1, ~pulse_in, 1'b1} )
          );

   spec_top
     #(
       .g_simulation (1)
    ) SPEC_B (
           .clk_125m_pllref_p_i (clk_ref),
           .clk_125m_pllref_n_i (~clk_ref),

           .fpga_pll_ref_clk_101_p_i (clk_ref),
           .fpga_pll_ref_clk_101_n_i (~clk_ref),

           .clk_20m_vcxo_i(clk_20m),

           // Connect the gigabit output of one SPEC with the RX input of the other,
           // and vice-versa.
           .sfp_txp_o(b_to_a_p),
           .sfp_txn_o(b_to_a_n),

           .sfp_rxp_i(a_to_b_p),
           .sfp_rxn_i(a_to_b_n),

           .dio_p_o ( dio_out_b )
    );

   assign pulse_out = dio_out_b[2];
   

   // observe the link LEDs on both sides, and tell us when the link is ready.
   wire link_up_a = SPEC_A.U_The_WR_Core.led_link_o;
   wire link_up_b = SPEC_B.U_The_WR_Core.led_link_o;

  initial begin
      // wait until both SPECs see the Ethernet link. Otherwise the packet we're going 
      // to send might end up in void...
      $display("Start very looooong wait until link is OK (over 600us)");
      #520us
      wait(link_up_a == 1'b1 && link_up_b == 1'b1);
      #10us
      $display("Stop very looooong wait until link is OK");
     forever begin // send a pulse every 30 us;
        pulse_in = 1;
        #1us;
        pulse_in = 0;
        #30us;
     end

  end
   
endmodule // main

