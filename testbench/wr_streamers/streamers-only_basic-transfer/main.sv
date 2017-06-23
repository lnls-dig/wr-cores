//
// White Rabbit Core Hands-On Course
//
// Lesson 04: Simulating the streamers
//
// Objectives:
// - demonstrate packet transfers in WR Core MAC interface
// - demonstrate the user interface of tx_streamer/rx_streamer modules.
// - demonstrate latency measurement feature of the streamers
//
// Brief description:
// A continuous sequence of 64-bit numbers (counting up from 0) is streamed
// via the TX streamer, sent as Ethernet packets over WR MAC interface and decoded
// in the RX streamer module.
//

`include "if_wb_link.svh"

`timescale 1ns/1ns

module main;

   // Parameters
   
   // Size of data record to be used by the streamers - in our case, a 64-bit
   // word.
   parameter g_record_size = 64;

   // MAC address of the TX side
   parameter bit [47:0] g_mac_tx = 48'h112233445566;

   // MAC address of the RX side
   parameter bit [47:0] g_mac_rx = 48'hcafebabedead;

   // Ethertype for distinguishing our streamer frames
   parameter bit [15:0] g_ethertype = 16'hdbff;

   // 16-bit data, 2-bit address Wishbone bus that connects the WR MAC interfaces
   // of both streamers
   IWishboneLink #(16, 2) mac();

   // Clock & reset
   reg clk = 0;
   reg rst = 0;

   // TX Streamer signals
   reg tx_streamer_dvalid = 0;
   reg [g_record_size-1:0] tx_streamer_data = 0;
   wire                    tx_streamer_dreq;

   // RX Streamer signals
   reg                     rx_streamer_dreq = 0;
   wire [g_record_size-1:0] rx_streamer_data;
   wire                     rx_streamer_dvalid;
   wire                     rx_streamer_lost;
   wire [27:0]              rx_latency;
   wire                     rx_latency_valid;


   // Fake White Rabbit reference clock (125 MHz) and cycle counte (we don't use 
   // TAI counter as the latency never exceeds 1 second...)

   reg                      clk_ref = 0;
   reg [27:0]               tm_cycle_counter = 0;

   // Currently transmitted counter value
   integer                  tx_counter = 0;

   initial #100 rst = 1;
   always #10 clk <= ~clk;

   // transfer queue. Used to pass sent data to the verification process.
   logic [g_record_size-1:0] queue[$];
   
   //generate 125 MHz WR Clock
   always #4ns clk_ref <= ~clk_ref; 

   // WR clock cycle counter
   always@(posedge clk_ref)
     tm_cycle_counter <= tm_cycle_counter + 1;
   
   
   // TX data stream generation. 
   always@(posedge clk) 
     if(!rst)
       begin
          tx_streamer_dvalid <= 0;
          tx_counter <= 0;
       end else  begin
          // TX streamer is fed with a subsequent data word at random intervals (you can
          // change the probability in the condition below). New value is sent only when
          // the streamer can accept it (i.e. its tx_dreq_o output is active)
          if({$random} % 100 < 50 && tx_streamer_dreq) begin
             queue.push_back(tx_counter);
             
             tx_streamer_data <= tx_counter;
             tx_streamer_dvalid <= 1;
             
             tx_counter++;

          end else
            tx_streamer_dvalid <= 0;
       end // if (rst)

   // Instantiation of the streamers. The TX streamer will assemble packets
   // containing max. 8 records, or flush the buffer after 512 clk cycles if
   // it contains less than 8 records to prevent latency buildup.
   tx_streamer
     #( 
        .g_data_width   (g_record_size),
        .g_tx_threshold  (8),
        .g_tx_buffer_size(16),
        .g_tx_max_words_per_frame(16),
        .g_tx_timeout    (512),
        .g_simulation(1)
     ) 
   U_TX_Streamer
     (
      .clk_sys_i(clk),
      .rst_n_i  (rst),
 
      .src_dat_o  (mac.dat_i),
      .src_adr_o  (mac.adr),
      .src_sel_o  (mac.sel),
      .src_cyc_o  (mac.cyc),
      .src_stb_o  (mac.stb),
      .src_we_o   (mac.we),
      .src_stall_i(mac.stall),
      .src_err_i  (mac.err),
      .src_ack_i  (mac.ack),

      .clk_ref_i(clk_ref), // fake WR time
      .tm_time_valid_i(1'b1),
      .tm_cycles_i(tm_cycle_counter),

      .tx_data_i      (tx_streamer_data),
      .tx_valid_i     (tx_streamer_dvalid),
      .tx_dreq_o      (tx_streamer_dreq),
      
      .cfg_mac_local_i  (g_mac_tx),
      .cfg_mac_target_i (g_mac_rx),
      .cfg_ethertype_i  (g_ethertype)
      );

   rx_streamer
     #(
       .g_data_width        (g_record_size),
       .g_simulation(1)
       ) 
   U_RX_Streamer 
     (
      .clk_sys_i (clk),
      .rst_n_i   (rst),

      .snk_dat_i (mac.dat_i),
      .snk_adr_i (mac.adr),
      .snk_sel_i (mac.sel),
      .snk_cyc_i (mac.cyc),
      .snk_stb_i (mac.stb),
      .snk_we_i  (mac.we),
      .snk_stall_o (mac.stall),
      .snk_ack_o  (mac.ack),
      .snk_err_o (mac.err),
      .snk_rty_o (mac.rty),

      .clk_ref_i(clk_ref), // fake WR time
      .tm_time_valid_i(1'b1),
      .tm_cycles_i(tm_cycle_counter),
      
      .rx_data_o  (rx_streamer_data),
      .rx_valid_o (rx_streamer_dvalid),
      .rx_dreq_i  (rx_streamer_dreq),
      .rx_latency_o (rx_latency),
      .rx_latency_valid_o(rx_latency_valid),
   
      .cfg_mac_local_i  (g_mac_rx),
      .cfg_mac_remote_i (g_mac_tx),
      .cfg_ethertype_i  (g_ethertype)
      );

   // Client-side reception logic. Compares the received records with their copies
   // stored in the queue.
   
   always@(posedge clk)
     if(!rst)
       begin
          rx_streamer_dreq <= 0;
       end else begin
          // throttle the RX path a little bit
          rx_streamer_dreq  <= {$random}%100 < 80;

          if(rx_streamer_dvalid)
            begin
               // Got a record? Compare it against the copy stored in queue.
               automatic logic [g_record_size-1:0] d = queue.pop_front();
               
               $display("Received value: %d", rx_streamer_data);
               
               if(rx_streamer_data != d)
                 begin
                    $display("Failure: Got %d, should be: %d\n", rx_streamer_data, d);
                    $stop;
              end
            end // if (rx_streamer_dvalid)
       end // else: !if(!rst)

   // Show the latency value when a new frame arrives
   always@(posedge clk)
     if(rst && rx_latency_valid)
          $display("This frame's latency: %.3f microseconds\n", real'(rx_latency) * 0.008);
   
   
endmodule // main

