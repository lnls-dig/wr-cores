//-----------------------------------------------------------------------------
// Title      : WR streamers testbench
// Project    : White Rabbit Cores
// URL        : http://www.ohwr.org/projects/wr-cores/wiki/WR_Streamers
//-----------------------------------------------------------------------------
// File       : main.sv
// Author(s)  : Tomasz Wlostosky,
//              Extended by Denia Bouhired <denia.bouhired@cern.ch>
// Company    : CERN (BE-CO-HT)
// Created    : 2017-04-28
//-----------------------------------------------------------------------------
// Description:
// 

//
//-----------------------------------------------------------------------------
//
// Copyright (c) 2017 CERN
//
// This source file is free software; you can redistribute it
// and/or modify it under the terms of the GNU Lesser General
// Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any
// later version.
//
// This source is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied
// warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE.  See the GNU Lesser General Public License for more
// details.
//
// You should have received a copy of the GNU Lesser General
// Public License along with this source; if not, download it
// from http://www.gnu.org/licenses/lgpl-2.1.html
//

//-----------------------------------------------------------------------------

`include "../../../sim/if_wb_link.svh"

// `include "./tb_wr_fabric_link_control.sv"

`timescale 1ns/1ns

module main;

   /////////////////////////////////////////////////////////////////////////////
   // Parameters
   /////////////////////////////////////////////////////////////////////////////  
   
   // Size of data record to be used by the streamers.
   // In this case, a 64-bit word.
   parameter g_word_width = 64;     
   parameter g_tx_thr = 16;     
   parameter g_tx_tm_out = 128;   
   parameter g_max_wrds_pr_frm = 16;
   
   // Min and max block size
   parameter g_block_size_min = 1; 
   parameter g_block_size_max = 5;
   
   // Min and max frame size
   parameter g_frame_size_min = 1; 
   parameter g_frame_size_max = 3;

   // MAC address of the TX side
   parameter bit [47:0] g_mac_tx = 48'h112233445566;


   // MAC address of the RX side
   parameter bit [47:0] g_mac_rx = 48'hcafebabedead;

   // Ethertype for distinguishing streamer frames. Default accepted by WRPC core.
   parameter bit [15:0] g_ethertype = 16'hdbff;

   // 16-bit data, 2-bit address Wishbone bus that connects the WR MAC interfaces
   // of both streamers
   IWishboneLink #(16, 2) mac();

  

   // Clock & reset
   reg clk = 0;
   reg rst_n = 0;
   reg [27:0]               clk_cycle_counter = 0;           
   int clk_cycle_counter_before = 0;
   int clk_cycle_counter_after = 0;

   // TX Streamer signals
   reg                    tx_streamer_dvalid = 0;
   reg [g_word_width-1:0] tx_streamer_data = 0;
   //wire[15:0] data_from_tx= 0;
   reg                    tx_streamer_last = 0;
   reg                    tx_flush = 0;
   wire                   tx_streamer_dreq;
   wire                   tx_frame_sent;

   // RX Streamer signals
   reg                     rx_streamer_dreq = 0;
   wire [g_word_width-1:0] rx_streamer_data;
   wire                    rx_streamer_dvalid;
   wire                    rx_streamer_lost_blks;
   wire                    rx_streamer_lost_frm;
   wire [14:0]             rx_streamer_lost_frm_cnt;
   wire                    rx_streamer_first;
   wire                    rx_streamer_last;
   wire [27:0]             rx_latency;
   wire                    rx_latency_valid;
   wire                    rx_frame_received;


   // Fake White Rabbit reference clock (125 MHz) and cycle counter (we don't use 
   // TAI counter as the latency never exceeds 1 second...)

   reg                      clk_ref = 0;
   reg [27:0]               tm_cycle_counter = 0;

   // Currently transmitted counter value
   integer                  tx_counter_val = 0;
   
   //Seed for random generator
   int                      seed = 0;
   
   // Wishbone link interface 
    bit [15 : 0] data_from_tx;
    logic [15 : 0] data_to_rx ;
    logic tx_wb_cyc, rx_wb_cyc;
    logic tx_wb_stb, rx_wb_stb;
    logic tx_wb_ack;
    logic tx_wb_stall;
    wire rx_wb_stall;
    wire rx_wb_ack;
    
    
    //tests
    
    logic        flush_test = 0;
    logic       timeout_test = 0;
    logic        thr_test = 0;
   /////////////////////////////////////////////////////////////////////////////
   // Initialise and set, reset, clocks and clk counter
   /////////////////////////////////////////////////////////////////////////////
   initial
    begin 
        #100 rst_n = 1;
    end;

    
   always #10   clk     <= ~clk;
   always #4ns  clk_ref <= ~clk_ref; //generate 125 MHz WR Clock
  
   always@(posedge clk_ref) tm_cycle_counter <= tm_cycle_counter + 1;   
   always@(posedge clk) clk_cycle_counter <= clk_cycle_counter + 1;
   always@(posedge tx_streamer_last) clk_cycle_counter_before = clk_cycle_counter;
   always@(posedge tx_wb_cyc) clk_cycle_counter_after = clk_cycle_counter;
   
   
   tx_streamer
     #( 
        .g_data_width   (g_word_width),
        .g_tx_buffer_size(2*g_tx_thr),
        .g_tx_threshold  (g_tx_thr),
        .g_tx_timeout    (g_tx_tm_out),
        .g_tx_max_words_per_frame (g_max_wrds_pr_frm),
        .g_simulation(1),
        .g_sim_startup_cnt(0)
     ) 
   U_TX_Streamer
     (
      .clk_sys_i(clk),
      .rst_n_i  (rst_n),

      .src_dat_o  (data_from_tx),// (mac.dat_i),
      .src_adr_o  (mac.adr),
      .src_sel_o  (mac.sel),
      .src_cyc_o  (tx_wb_cyc),
      .src_stb_o  (tx_wb_stb),
      .src_we_o   (mac.we),
      .src_stall_i(tx_wb_stall),
      .src_err_i  (mac.err),
      .src_ack_i  (tx_wb_ack),

      .clk_ref_i(clk_ref), // fake WR time
      .tm_time_valid_i(1'b1),
      .tm_cycles_i(tm_cycle_counter),

      .tx_data_i      (tx_streamer_data),
      .tx_valid_i     (tx_streamer_dvalid),
      .tx_dreq_o      (tx_streamer_dreq),
      .tx_last_p1_i   (tx_streamer_last),
      .tx_flush_p1_i   (tx_flush),
      .tx_reset_seq_i (),
      .tx_frame_p1_o  (tx_frame_sent),
      
      .cfg_mac_local_i  (g_mac_tx),
      .cfg_mac_target_i (g_mac_rx),
      .cfg_ethertype_i  (g_ethertype)
      );

  

   rx_streamer
     #(
       .g_data_width        (g_word_width)
       ) 
   U_RX_Streamer 
     (
      .clk_sys_i (clk),
      .rst_n_i   (rst_n),

      .snk_dat_i (data_to_rx),
      .snk_adr_i (mac.adr),
      .snk_sel_i (mac.sel),
      .snk_cyc_i (rx_wb_cyc),
      .snk_stb_i (rx_wb_stb),
      .snk_we_i  (mac.we),
      .snk_stall_o (rx_wb_stall),
      .snk_ack_o  (rx_wb_ack),
      .snk_err_o (mac.err),
      .snk_rty_o (mac.rty),

      .clk_ref_i(clk_ref), // fake WR time
      .tm_time_valid_i(1'b1),
      .tm_cycles_i(tm_cycle_counter),
      
      .rx_first_p1_o (rx_streamer_first),
      .rx_last_p1_o (rx_streamer_last),
      .rx_data_o  (rx_streamer_data),
      .rx_valid_o (rx_streamer_dvalid),
      .rx_dreq_i  (rx_streamer_dreq),
      
      //.rx_lost_p1_o (rx_streamer_lost),
      .rx_lost_blocks_p1_o (rx_streamer_lost_blks),
      .rx_lost_frames_p1_o (rx_streamer_lost_frm), 
      .rx_lost_frames_cnt_o (rx_streamer_lost_frm_cnt),

      .rx_latency_o (rx_latency),
      .rx_latency_valid_o (rx_latency_valid),
      
      .rx_frame_p1_o (rx_frame_received),
   
      .cfg_mac_local_i  (g_mac_rx),
      .cfg_mac_remote_i (g_mac_tx),
      .cfg_ethertype_i  (g_ethertype)
      );
   
   
   
   
   
   /////////////////////////////////////////////////////////////////////////////
   // Struct definition
   /////////////////////////////////////////////////////////////////////////////

   typedef struct{  bit[g_word_width-1:0] words[$];
                    int wrd_cnt[$];
                    bit[g_word_width-1:0] first_wrd;
                    bit[g_word_width-1:0] last_wrd;
                    bit dropped;
                  } block_t;   //block is a number of words with info about first 
                               // and last
                
   typedef struct{  block_t blocks[$];
                  } streamer_frame_t; //frame contains a collection of blocks


// Transfer queue. Used to pass sent data to the verification process.
   block_t tx_blk_queue[$];
   streamer_frame_t tx_frm_queue[$]; /////////////////////////////////////////////////////////////////////////////
   // Task definitions
   /////////////////////////////////////////////////////////////////////////////
   
   
   // Generate a block of data words of random size, containing subsequent 
   // numbers
   /////////////////////////////////////////////////////////////////////////////
   
   task automatic generate_block(ref block_t blk, int size);
   

      int i;

      
      for(i = 0; i<size; i++) 
        begin
            if (i == 0) 
                blk.first_wrd = tx_counter_val; // Copy first word
            if (i == size-1) 
                blk.last_wrd = tx_counter_val; // Copy last word
            
            blk.words.push_back(tx_counter_val++); //
            
            if (i == 0 || i==size-1) 
                blk.wrd_cnt.push_back(i+1);// first or last words
            else 
                blk.wrd_cnt.push_back(0); // All other words
        end //for loop      
        
   endtask // generate_block
 ////////////////////////////////////////////////////////  
    task automatic generate_frame(ref streamer_frame_t frm, int size);
   
      int i;
      block_t blk;
      for(i = 0; i<size; i++) 
        begin
            blk.words = {};
            blk.wrd_cnt = {};
            generate_block(blk, size);
            frm.blocks.push_back(blk);            
        end       

    endtask  
    
               
   // Sends out a data block (blk) by driving TX_(DVALID, DATA, LAST) lines 
   // of the TX streamer
   /////////////////////////////////////////////////////////////////////////////
   
   task automatic send_block(ref block_t blk);
      int i = 0;
      ////$display("Sending block of %d words...", blk.words.data.size());
      while(i < blk.words.size())
        begin
           if(tx_streamer_dreq) begin
              // assert the TX_LAST line on the last word in the current block
              tx_streamer_last <= (i == (blk.words.size() - 1)) ? 1 : 0;
              tx_streamer_data <= blk.words[i];
              $display("Data to be sent is %d*****\n", tx_streamer_data);
              tx_streamer_dvalid <= 1;
              //clk_cycle_counter_before = clk_cycle_counter;
              i++;
           end else
             tx_streamer_dvalid <= 0;
              
           @(posedge clk);
        end // while (i < blk.words.data.size())
        tx_streamer_dvalid <= 0;
        tx_streamer_last <= 0;
      
   endtask // send_block

    


    assign data_to_rx = data_from_tx;
    assign rx_wb_stb = tx_wb_stb;
    assign rx_wb_cyc = tx_wb_cyc;
    assign tx_wb_stall = rx_wb_stall;
    assign tx_wb_ack = rx_wb_ack;   
  
///////////////////////////////////////////////////////////////////
   //RECEPTION LOGIC//
   // Receives a data block from the RX streamer and puts it in (blk).
   // Returns non-zero done value when blk contains a complete block
   /////////////////////////////////////////////////////////////////////////////
   
   task automatic receive_block(ref block_t blk, ref int new_block, ref int done);
           
    bit[g_word_width-1:0] wrd[$];
    bit[g_word_width-1:0] word1;
    bit[g_word_width-1:0] wordn;
    
    wrd= blk.words;
    word1=blk.first_wrd;
    wordn=blk.last_wrd;
   
    if(rx_streamer_dvalid)
        begin
            if(rx_streamer_first && new_block == 1) 
                begin
                     new_block = 0;
                     wrd = {};
                     blk.wrd_cnt = {};
                     blk.wrd_cnt.push_back(1);
                     word1 = rx_streamer_data;
                end 
            else if (!rx_streamer_last && !rx_streamer_first) 
                begin
                 blk.wrd_cnt.push_back(0);
                end
            
            wrd.push_back(rx_streamer_data);
            if (rx_streamer_last && new_block == 0)
                begin
                    wordn = rx_streamer_data;
                    if (wrd.size() > 1) 
                        blk.wrd_cnt.push_back(wrd.size());  //Last word in block           
                    done = 1;
                end 
            else
                begin
                    done = 0;
                end
            blk.words=wrd; 
            blk.first_wrd = word1;
            blk.last_wrd = wordn;
        end
   endtask // receive_block
   
   //Check transmission has been initiated auomatically
   

   
      // TX block stream generation

    initial forever 
        begin
            int blk_size;
            
            streamer_frame_t frm;
            block_t blk;
            
            blk.words = {};
            blk.wrd_cnt = {};
            
            wait(rst_n == 1'b1);
            

            rx_streamer_dreq  <= 1;//({$random} % 100 < 90) ? 1 : 
            thr_test = 0;  
            timeout_test = 0;        
            flush_test = 0;
            //Tx TEST 1: Check that maximum number of words/frame triggers transmission
            //-------------------------------------------------------------------------
            blk_size = g_max_wrds_pr_frm + 10; 
            generate_block(blk, blk_size); 
            $display ("Send block: %p \n", blk);          
            send_block(blk);
            tx_blk_queue.push_back(blk);   
            wait (tx_frame_sent) thr_test = 1;
            $display ("Frame transmitted after limit of words/frame is reached\n");
            
            //Tx TEST 2: Check that when timeout is reached, frame is transmitted
            //-------------------------------------------------------------------------
            blk.words = {};
            blk.wrd_cnt = {};
            blk_size = 2; //just 2 words
            generate_block(blk, blk_size); 
            $display ("Send block: %p \n", blk);          
            send_block(blk);
            tx_blk_queue.push_back(blk);   
            wait(tx_frame_sent) 
            $display ("Frame transmitted after timeout: %d\n", clk_cycle_counter_after-clk_cycle_counter_before);
            
            if (g_tx_tm_out == clk_cycle_counter_after-clk_cycle_counter_before - 5) 
            begin
                $display ("Tx timeout test PASSED \n");
                timeout_test = 1;
            end
            else
                $error ("Failed timeout test");
            
            //Tx TEST 3: Check that when tx_flush_i is asserted, current frame is txed
            //-------------------------------------------------------------------------
            blk.words = {};
            blk.wrd_cnt = {};
            blk_size = 4; //just 2 words
            generate_block(blk, blk_size); 
            $display ("Send block: %p \n", blk);          
            send_block(blk);
            @(posedge clk) tx_flush = 1;
            @(posedge clk) tx_flush = 0;
            
            tx_blk_queue.push_back(blk);   
            wait(tx_frame_sent) flush_test = 1;
            $display ("Frame transmitted after flush asserted");

     assert (flush_test == 1 && timeout_test == 1 && thr_test == 1) 
     else begin
	      $error(1, "Transmitter implementation contains errors", $time);
	 end
     end
 // Instantiation of the streamers. The TX streamer will assemble packets
   // containing max. 8 records, or flush the buffer after 128 clk cycles if
   // it contains less than 8 records to prevent latency buildup.
   
   
   


   
   // TESTBENCH VERIFICATION
   // Client-side reception logic. Compares the received records with their copies
   // stored in the transfer queue.  
   int new_block = 1;
   int no_curr_req = 0;
   
           
   always@(posedge clk)
 
     if(rst_n)
       begin
          
          
          block_t rblk;
          streamer_frame_t tfrm;
          automatic int done = 0;            
          receive_block(rblk, new_block, done);  
          if(done)
            begin
               automatic block_t tblk;                 
               tblk = tx_blk_queue.pop_front();               
               $display(" OLD TBLK is %p \n********",  tblk);        
               $display(" FRAME AFTER POP is  %p \n%%%%%%%%%%%%",  tfrm);
               $display(" QUEUE is  %p \n%%%%%%%%%%%%",  tx_blk_queue);
               $display(" block OUT is %p \n********",  tblk);
               new_block = 1;                
              // ===============================================================
              // TEST 1: Check Txed and Rxed blocks match
               if(tblk.words != rblk.words)
                 begin
                    $error("TEST 1 ---> FAILED\n####Sent block does not match received block\n");
                    $display("Txed is %p, Rxed equals %p", tblk, rblk);
                    $stop;
                 end else begin 
                 $display("****\nTEST 1 ---> PASSED\n****Correct words received\n");
                 end
               
            end
       end // else: !if(!rst_n)

  always@(posedge clk)
    if(rst_n && rx_latency_valid)
         $display("*************This frame's latency: %.3f microseconds*************************************************\n", real'(rx_latency) * 0.008);
   
endmodule // main

