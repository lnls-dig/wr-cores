//
// White Rabbit Core Hands-On Course
//
// Lesson 04b: Multiword transfers
//
// Objectives:
// - demonstrate multiword transfers using tx_last, rx_first and rx_last lines.
//
//

`include "../../../sim/if_wb_link.svh"

`timescale 1ns/1ns

module main;

   // Parameters
   
   // Size of data record to be used by the streamers - in our case, a 64-bit
   // word.
   parameter g_word_width = 64;

   parameter g_block_size_min = 1;
   parameter g_block_size_max = 3;

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
   reg rst_n = 0;

   // TX Streamer signals
   reg tx_streamer_dvalid = 0;
   reg [g_word_width-1:0] tx_streamer_data = 0;
   reg                     tx_streamer_last = 0;
   wire                    tx_streamer_dreq;

   // RX Streamer signals
   reg                     rx_streamer_dreq = 0;
   wire [g_word_width-1:0] rx_streamer_data;
   wire                     rx_streamer_dvalid;
   wire                     rx_streamer_lost;
   wire                     rx_streamer_first;
   wire                     rx_streamer_last;
   wire [27:0]              rx_latency;
   wire                     rx_latency_valid;


   // Fake White Rabbit reference clock (125 MHz) and cycle counter (we don't use 
   // TAI counter as the latency never exceeds 1 second...)

   reg                      clk_ref = 0;
   reg [27:0]               tm_cycle_counter = 0;

   // Currently transmitted counter value
   integer                  tx_counter = 0;

   initial #100 rst_n = 1;
   always #10 clk <= ~clk;

   typedef struct 
                  {
                     bit[g_word_width-1:0] data[$];
                  } block_t;

   // transfer queue. Used to pass sent data to the verification process.
   block_t queue[$];

   always #4ns clk_ref <= ~clk_ref; //generate 125 MHz WR Clock

   always@(posedge clk_ref)
     tm_cycle_counter <= tm_cycle_counter + 1;
   

   int                       count = 0;
   int                       seed = 0;
   
   // generate a block of data words of random size, containing subsequent numbers
   task automatic generate_block(ref block_t blk);
      int size = $dist_uniform(seed, g_block_size_min, g_block_size_max);
      int i;

      
      for(i = 0; i<size; i++)
        blk.data.push_back(count++);
   endtask // generate_block
   

   // sends out a data block (blk) by driving TX_(DVALID, DATA, LAST) lines of the TX streamer
   task automatic send_block(ref block_t blk);
      int i = 0;
      $display("Sending block of %d words...", blk.data.size());
      
      while(i < blk.data.size())
        begin
           if(tx_streamer_dreq) begin
              // assert the TX_LAST line on the last word in the current block
              tx_streamer_last <= (i == (blk.data.size() - 1)) ? 1 : 0;
              tx_streamer_data <= blk.data[i];
              tx_streamer_dvalid <= 1;
              i++;
           end else
             tx_streamer_dvalid <= 0;
              
           @(posedge clk);
        end // while (i < blk.data.size())
      tx_streamer_dvalid <= 0;
      tx_streamer_last <= 0;
      @(posedge clk);
      
   endtask // send_block

   // receives a data block from the RX streamer and puts it in (blk). Returns non-zero done value when
   // blk contains a complete block
   task automatic receive_block(ref block_t blk, ref int done);

      // drive dreq line permanently to 1 to make the testbench a bit clearer
      rx_streamer_dreq <= 1;
      
      if(rx_streamer_dvalid)
        begin
           
           if(rx_streamer_first)
             blk.data = {};

           blk.data.push_back(rx_streamer_data);
           
           if(rx_streamer_last)
             begin
                done = 1;
             end
           else
             done = 0;
        end
   endtask // receive_block
   
   
   // TX block stream generation
   initial forever begin
      block_t blk;

      blk.data={};

      generate_block(blk);
      queue.push_back(blk);
      send_block(blk);
   end

   
   // Instantiation of the streamers. The TX streamer will assemble packets
   // containing max. 8 records, or flush the buffer after 512 clk cycles if
   // it contains less than 8 records to prevent latency buildup.
   tx_streamer
     #( 
        .g_data_width   (g_word_width),
        .g_tx_threshold  (8),
        .g_tx_buffer_size(16),
        .g_tx_max_words_per_frame(16),
        .g_tx_timeout    (128),
        .g_simulation(1)
     ) 
   U_TX_Streamer
     (
      .clk_sys_i(clk),
      .rst_n_i  (rst_n),

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
      .tx_last_p1_i   (tx_streamer_last),
      
      .cfg_mac_local_i  (g_mac_tx),
      .cfg_mac_target_i (g_mac_rx),
      .cfg_ethertype_i  (g_ethertype)
      );

   rx_streamer
     #(
       .g_data_width        (g_word_width),
       .g_simulation(1)
       ) 
   U_RX_Streamer 
     (
      .clk_sys_i (clk),
      .rst_n_i   (rst_n),

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
      .rx_first_p1_o (rx_streamer_first),
      .rx_last_p1_o (rx_streamer_last),
      
      .rx_latency_o (rx_latency),
      .rx_latency_valid_o(rx_latency_valid),
   
      .cfg_mac_local_i  (g_mac_rx),
      .cfg_mac_remote_i (g_mac_tx),
      .cfg_ethertype_i  (g_ethertype)
      );

   // Client-side reception logic. Compares the received records with their copies
   // stored in the queue.
   
   always@(posedge clk)
     if(rst_n)
       begin
          block_t rblk;
          automatic int done = 0;
          
          receive_block(rblk, done);

          if(done)
            begin
               automatic block_t tblk = queue.pop_front();
               
               $display("Received block of %d words.\n", rblk.data.size());

               if(tblk.data != rblk.data)
                 begin
                    $error("Sent block does not match received block\n");
                    $stop;
                 end
               
                    
               

               
            end
       end // else: !if(!rst_n)

//   always@(posedge clk)
  //   if(rst && rx_latency_valid)
    //      $display("This frame's latency: %.3f microseconds\n", real'(rx_latency) * 0.008);
   
endmodule // main

