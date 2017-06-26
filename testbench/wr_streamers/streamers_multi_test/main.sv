//-----------------------------------------------------------------------------
// Title      : WR streamers testbench
// Project    : White Rabbit Cores
// URL        : http://www.ohwr.org/projects/wr-cores/wiki/WR_Streamers
//-----------------------------------------------------------------------------
// File       : main.sv
// Author(s)  : Tomasz Wlostosky ,
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
   parameter g_max_wrds_pr_frm = 24;
   parameter g_fixed_latency = 28'd1024;
   
   // Min and max block size
   parameter block_size_min = 1; 
   parameter block_size_max = 5;
   
   // Min and max frame size
   parameter frame_size_min = 1; 
   parameter frame_size_max = 3;

   // Maximum number of frames
   parameter max_num_frames = 5; 
   
   // MAC address of the TX side
   parameter bit [47:0] g_mac_tx = 48'h112233445566;


   // MAC address of the RX side
   parameter bit [47:0] g_mac_rx = 48'hcafebabedead;

   // Ethertype for distinguishing streamer frames. Default accepted by WRPC core.
   parameter bit [15:0] g_ethertype = 16'hdbff;

   // 16-bit data, 2-bit address Wishbone bus that connects the WR MAC interfaces
   // of both streamers
   IWishboneLink #(16, 2) mac();

  

   // Fake White Rabbit reference clock (125 MHz) and cycle counter (we don't use 
   // TAI counter as the latency never exceeds 1 second...)

   reg                      clk_ref = 0;
   reg [27:0]               tm_cycle_counter = 0;
   
   // System clock & reset
   reg clk = 0;
   reg rst_n = 0;
   reg [27:0]               clk_cycle_counter = 0;           
   int clk_cycle_tmout_ctr_before = 0;
   int clk_cycle_tmout_ctr_after = 0;
   int clk_cycle_frm_txed = 0;
   int clk_cycle_frm_valid = 0;

   // TX Streamer signals
   reg                    tx_streamer_dvalid = 0;
   reg [g_word_width-1:0] tx_streamer_data = 0;
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
   
   //Fixed latency
   reg  [27:0] fixed_latency = 28'h0;


   // Currently transmitted counter value
   int                     tx_counter_val = 0;
   
   //Seed for random generator
   int                     seed = 0;
   
   // Wishbone link interface 
    bit [15 : 0]           fab_data_from_tx;
    logic [15 : 0]         fab_data_to_rx ;
    logic tx_wb_cyc, rx_wb_cyc;
    logic tx_wb_stb, rx_wb_stb;
    logic tx_wb_ack;
    logic tx_wb_stall;
    wire rx_wb_stall;
    wire rx_wb_ack;
    
    
    //tests
    
    logic        flush_test = 0;
    logic        timeout_test = 0;
    logic        max_words_test = 0;
    logic        min_words_test = 0;
    logic        flatency_test = 0;
    logic        frm_drop_test = 0;
    logic        comparator_test = 1;
    
    int          link_tests = 0;
    string       current_test = "IDLE";

   // Initialise and set, reset, clocks and clk counter
    initial
        begin 
            #100 rst_n = 1;
        end;
    
   // --------------------------------------------------------------------------
   //WARNING: As of now, the fixed latency implementation in xrx_streamer assumes 
   //that ref_clk period is equal to sys_clk period/2. 
   //This is a bug and should be fixed in future releases
                                        
   always #8ns    clk     <= ~clk;      
   always #4ns  clk_ref <= ~clk_ref; //generate 125 MHz WR Clock
  
  
   // --------------------------------------------------------------------------
   //Set counter values for time measurements
   
   always@(posedge clk_ref) tm_cycle_counter <= tm_cycle_counter + 1;   
   always@(posedge clk) clk_cycle_counter <= clk_cycle_counter + 1;
   always@(posedge tx_streamer_last) clk_cycle_tmout_ctr_before = clk_cycle_counter;
   always@(posedge rx_frame_received) clk_cycle_tmout_ctr_after = clk_cycle_counter;
   
   // Instantiation of the streamers. The TX streamer will assemble packets
   // containing max. 8 records, or flush the buffer after 128 clk cycles if
   // it contains less than 8 records to prevent latency buildup.
 
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

      .src_dat_o  (fab_data_from_tx),
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

      .snk_dat_i (fab_data_to_rx),
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
      .rx_lost_blocks_p1_o (rx_streamer_lost_blks),
      .rx_lost_frames_p1_o (rx_streamer_lost_frm), 
      .rx_lost_frames_cnt_o (rx_streamer_lost_frm_cnt),

      .rx_latency_o (rx_latency),
      .rx_latency_valid_o (rx_latency_valid),
      
      .rx_frame_p1_o (rx_frame_received),
   
      .cfg_mac_local_i  (g_mac_rx),
      .cfg_mac_remote_i (g_mac_tx),
      .cfg_ethertype_i  (g_ethertype),
      .cfg_accept_broadcasts_i (),
      .cfg_filter_remote_i     (),
      .cfg_fixed_latency_i     (fixed_latency) //(28'd2000)
      );
   
   // --------------------------------------------------------------------------
   // Struct definition
   // --------------------------------------------------------------------------

   typedef struct{  bit[g_word_width-1:0] words[$];
                  } block_t;   //block is a number of words with info about first 
                               // and last
                
   typedef struct{  block_t blocks[$];
                  } streamer_frame_t; //frame contains a collection of blocks


   // Transfer queue. Used to pass sent data to the verification process.
   block_t tx_blk_queue[$];             //queue of trasmitted blocks
   streamer_frame_t tx_frm_queue[$];    //queue of trasmitted frames
   
   
   
   // --------------------------------------------------------------------------   
   // --------------------------------------------------------------------------
   // Task definitions
   // --------------------------------------------------------------------------
   
   
   // Generate a block of data words of random size, containing subsequent 
   // numbers
   
   task automatic generate_block(ref block_t blk, int size);
        int i;
        for(i = 0; i<size; i++) 
            begin            
                blk.words.push_back(tx_counter_val++); //
            end //for loop      
   endtask // generate_block
   
   // --------------------------------------------------------------------------
   // generate a number of blocks each with a number of words
   //store blks sent in frame queue
   
    task automatic generate_frame(ref streamer_frame_t frm, int frm_size, int blk_size);
        int i;
        block_t blk;
        for(i = 0; i<frm_size; i++) 
            begin
                blk.words = {};
                generate_block(blk, blk_size);
                frm.blocks.push_back(blk);            
            end //for loop     
    endtask // generate_frame
    
               
   // -------------------------------------------------------------------------
   // Sends out a data block (blk) by driving TX_(DVALID, DATA, LAST) lines 
   // of the TX streamer

   
   task automatic send_block(ref block_t blk);
      int i = 0;
      while(i < blk.words.size())
        begin
           if(tx_streamer_dreq) begin
              // assert the TX_LAST line on the last word in the current block
              tx_streamer_last <= (i == (blk.words.size() - 1)) ? 1 : 0;
              tx_streamer_data <= blk.words[i];
              tx_streamer_dvalid <= 1;
              i++;
           end else
             tx_streamer_dvalid <= 0;
              
           @(posedge clk);
        end // while (i < blk.words.data.size())
        tx_streamer_dvalid <= 0;
        tx_streamer_last <= 0;
      
   endtask // send_block
   
    // -------------------------------------------------------------------------
  // send frame of multiple blocks
  
    task automatic send_frame(ref streamer_frame_t frm);
        int i = 0;
        
        while (i < frm.blocks.size()) begin
            send_block(frm.blocks[i]);
            i++;
        end
    endtask  // send_block

    // -------------------------------------------------------------------------
    //routine to generate and send a frame
      
    task automatic gen_send_frm(ref streamer_frame_t frm, int frm_size, int blk_size);
        frm.blocks = {};
        generate_frame(frm, frm_size, blk_size);  
        send_frame(frm);         
    endtask //gen_send_frm

    // -------------------------------------------------------------------------
    // WISHBONE LINK CONTROL
    // -------------------------------------------------------------------------

    
    logic [ 15 : 0] corrupt_mask = 16'h0000; //to use for corrupted blocks
    logic drop_frm = 0;
    logic delay_link= 0;
      
    //Continuous assignements to WR fabric signals
    assign fab_data_to_rx = fab_data_from_tx ^ corrupt_mask;
    assign rx_wb_stb = tx_wb_stb & (~drop_frm);// | ~delay_link);
    assign rx_wb_cyc = tx_wb_cyc & (~drop_frm);// | ~delay_link);
    assign tx_wb_stall = rx_wb_stall | delay_link;
    assign tx_wb_ack = rx_wb_ack | delay_link;   

    task automatic drop_frame (int n);
       //n={$random} % 5; //number of frames to be dropped
       for (int i=0; i<n; i++) 
           begin
                    @(posedge tx_wb_cyc) drop_frm = 1; 
           end            
           @(negedge tx_wb_cyc) drop_frm = 0;
       
    endtask //drop_frame    
    
    //##########################################################################
    //Is not being used currently as not fully working
    //The latency output is incorrect
    //could be that if this delay is triggered at a different point the output 
    //would be correct
    
    task automatic delay_frame ();
        delay_link = 0;        
        //wait (rx_wb_stall == 1)  //to avoid changes at startup
        // @(negedge rx_wb_stall)
        @(posedge rx_wb_stall) delay_link = 1;
        #10000;
        @(posedge clk) delay_link = 0;
        //link_good ();
    endtask //delay_frame
    //##########################################################################
    
    
  
   // -------------------------------------------------------------------------
   // RECEIVER LOGIC//
   // Receives a data block from the RX streamer and puts it in (blk).
   // Returns non-zero done value when blk contains a complete block
   // -------------------------------------------------------------------------
   
   task automatic receive_block(ref block_t blk, ref int new_block, ref int done);
           
    bit[g_word_width-1:0] wrd[$];
    wrd= blk.words;
    if(rx_streamer_dvalid)
        begin
            if(rx_streamer_first && new_block == 1) 
                begin
                     new_block = 0;
                     wrd = {};
                end 
            wrd.push_back(rx_streamer_data);
            if (rx_streamer_last && new_block == 0)
                done = 1;
            else
                done = 0;
                
            blk.words=wrd; 
        end
   endtask // receive_block
   
   //Check transmission has been initiated auomatically
   

   // -------------------------------------------------------------------------
   // SIMULATION TESTBENCH: Currently setup for 6 tests/use-cases   
   // -------------------------------------------------------------------------


    int rand_blk_size, blk_size, rand_frm_size, frm_size;
    int num_frm_dropped;
    int test_num;
    int test_tm_out;
            
            
    initial forever 
        begin
            streamer_frame_t frm;
            block_t blk;
            
            rand_frm_size = $urandom_range(block_size_max,block_size_min);
            rand_blk_size = $urandom_range(frame_size_max,frame_size_min); 
            
            
            fixed_latency = 28'd0;
            wait(rst_n == 1'b1); //make sure reset is not asserted
            
            rx_streamer_dreq  <= 1;     //({$random} % 100 < 90) ? 1 : 
            
            //list of tests 
            test_tm_out = 10000; //10us
            test_num   = 1;
            flush_test = 0;     
            timeout_test = 0;   
            max_words_test = 0; 
            min_words_test = 0; 
            flatency_test = 0;
            frm_drop_test = 0;
            
            
            //Tx TEST 1: Check that when tx_flush_i is asserted, current frame is txed
            //-------------------------------------------------------------------------
            current_test = "Tx FLUSH";
            blk_size = rand_blk_size; 
            frm_size = rand_frm_size;
            
            gen_send_frm(frm, frm_size, blk_size);
            @(posedge clk) tx_flush = 1;
            @(posedge clk) tx_flush = 0;
            tx_frm_queue.push_back(frm);  

            fork : wait_or_timeout_t1
              begin
                #test_tm_out; 
                $display ("[%t ns]: >>> FAILED - TEST %d - %s   \n", $time, test_num, current_test );
                disable wait_or_timeout_t1;
              end
              begin
                @(posedge rx_frame_received) flush_test = 1;
                    $display ("[%t ns]: PASSED - TEST %d - %s   \n", $time, test_num, current_test );
                disable wait_or_timeout_t1;
              end
            join
                
            wait (rx_frame_received); //make sure frame is sent
            wait (tx_frame_sent);
            test_num ++;
            
            //Tx TEST 2: Check that when timeout is reached, frame is transmitted
            //-------------------------------------------------------------------------
            current_test = "Tx TIMEOUT";
            blk_size = $urandom_range(g_tx_thr - 1, 1); //send less words than minimum threshold
            frm_size = 1 ;           //For timeout test no need for multiple blocks
            gen_send_frm(frm, frm_size, blk_size);
            tx_frm_queue.push_back(frm); 
            fork : wait_or_timeout_t2
              begin
                #((g_tx_tm_out* 2) * 16); //time before test fails. wait long enough
                $display ("[%t ns]: >>> FAILED - TEST %d - %s   \n", $time, test_num, current_test );
                disable wait_or_timeout_t2;
              end
              begin
                @(posedge rx_frame_received) timeout_test = 1;
                    $display ("[%t ns]: PASSED - TEST %d - %s   \n", $time, test_num, current_test );
                disable wait_or_timeout_t2;
              end
            join
            wait (rx_frame_received);
            wait (tx_frame_sent);
            test_num ++;
            
            //Tx TEST 3: Check that minimum number of words/frame triggers transmission
            //-------------------------------------------------------------------------                  
            current_test = "Tx MIN WORDS";
            blk_size = g_tx_thr ;     //Tx 1 more words that the limit
            frm_size = 1;
            
            gen_send_frm(frm, frm_size, blk_size); //generate and send a frame
            tx_frm_queue.push_back(frm);           //push txed frame into Tx Q
            
            fork : wait_or_timeout_t3
              begin
                #test_tm_out; 
                $display ("[%t ns]: >>> FAILED - TEST %d - %s   \n", $time, test_num, current_test );
                disable wait_or_timeout_t3;
              end
              begin
                @(posedge rx_frame_received) min_words_test = 1;
                    $display ("[%t ns]: PASSED - TEST %d - %s   \n", $time, test_num, current_test );
                disable wait_or_timeout_t3;
              end
            join
            
            wait (rx_frame_received);
            wait (tx_frame_sent);
            test_num ++;

            //Tx TEST 4: Check that minimum number of words/frame triggers transmission
            //-------------------------------------------------------------------------                  
            current_test = "Tx MAX WORDS";
            blk_size = g_max_wrds_pr_frm;     //blk_size *frm_size must be int multiple
            frm_size = 2;                     // of g_max_wrds_pr_frm
            fork
                begin
                    gen_send_frm(frm, frm_size, blk_size); //generate and send a frame
                    tx_frm_queue.push_back(frm);           //push txed frame into Tx Q
                end
                
                fork : wait_or_timeout_t4
                  begin
                    #test_tm_out;
                $display ("[%t ns]: >>> FAILED - TEST %d - %s   \n", $time, test_num, current_test );
                    disable wait_or_timeout_t4;
                  end
                  begin
                    @(posedge rx_frame_received) max_words_test = 1;
                    $display ("[%t ns]: PASSED - TEST %d - %s   \n", $time, test_num, current_test );
                     wait (rx_frame_received);
                     wait (tx_frame_sent);
                    disable wait_or_timeout_t4;
                  end
                join
                begin
                    for (int i=0; i< (frm_size*blk_size)/g_max_wrds_pr_frm; i++) begin 
                        wait (rx_frame_received);
                        wait (tx_frame_sent);
                    end
                end
            join
            
            test_num ++;
            
            // Rx Test 5: Check the fixed latency is correct
            //-----------------------------------------------
            #1us // make sure that the previous frame has been received, otherwise 
                 // the testbench might wrongly measure the latency
            current_test = "Rx FIXED-LATENCY";
            blk_size = rand_blk_size;
            frm_size = rand_frm_size;
            fixed_latency = g_fixed_latency;
            gen_send_frm(frm, frm_size, blk_size);
            @(posedge clk) tx_flush = 1;
            @(posedge clk) tx_flush = 0; 
            tx_frm_queue.push_back(frm);  
            
            @(posedge U_TX_Streamer.U_Wrapped_Streamer.U_Fab_Source.sof_i) clk_cycle_frm_txed = tm_cycle_counter;
            //$display("frame received @ %d, time %t\n", clk_cycle_frm_txed, $time);
            @(posedge rx_streamer_first) clk_cycle_frm_valid = tm_cycle_counter;
            //$display("frame out valid @ %d, time %t\n", clk_cycle_frm_valid, $time);
            
            //fixed latency value is checked against range since i/o interface 
            //of streamers does not allow for exact latency measurement without 
            //probing an internal signal
            
            if ((fixed_latency <= clk_cycle_frm_valid - clk_cycle_frm_txed+24) &&
               (fixed_latency >= clk_cycle_frm_valid - clk_cycle_frm_txed-24) )
               begin
                    $display ("[%t ns]: PASSED - TEST %d - %s   \n", $time, test_num, current_test );
                //$display ("Fixed latency set to %.3f us, Rx output valid @ %.3f us",
                //real'(fixed_latency) * 0.008, real'(clk_cycle_frm_valid-
               //clk_cycle_frm_txed) * 0.008);
                flatency_test = 1;
               end
            else
               begin
                $display ("[%t ns]: >>> FAILED - TEST %d - %s   \n", $time, test_num, current_test );
                $display ("Fixed latency set to %.3f us, Rx output latency valid @ %.3f us",
                real'(fixed_latency) * 0.008, real'(clk_cycle_frm_valid-
                clk_cycle_frm_txed) * 0.008);
               // $stop;
               end

            test_num ++;
            // Rx Test 6: Check frames dropped are signalled correctly
            // ----------------------------------------------- 
            current_test = "Rx DROP_FRAMES";
            
            num_frm_dropped = $urandom_range(max_num_frames - 1,1);
            fork
            begin
                for (int i=0;  i < max_num_frames; i++) begin
                    gen_send_frm(frm, frm_size, blk_size);
                    tx_frm_queue.push_back(frm); 
                    @(posedge clk) tx_flush = 1;
                    @(posedge clk) tx_flush = 0;  
                    wait (tx_frame_sent);
                end
            end
            drop_frame(num_frm_dropped);
            @(posedge rx_streamer_lost_frm) 
            begin
            if (num_frm_dropped == rx_streamer_lost_frm_cnt)
                begin
                    $display ("[%t ns]: PASSED - TEST %d - %s   \n", $time, test_num, current_test );
                    frm_drop_test = 1;
                end
            else
                $display ("[%t ns]: >>> FAILED - TEST %d - %s   \n", $time, test_num, current_test );
            end
            join;
            test_num ++;
            
            
            //###################
            //NOT working
            //Test tries to introduce latency on the fabric link
            //in order to see change in the output latency
            
            // Rx Test 6: Check frames dropped are signalled correctly
            // -----------------------------------------------   
            // current_test = "Tx DELAY FRAMES";
            // blk_size = 4;

            // fork
            // begin
                // gen_send_frm(frm, frm_size, blk_size);
                // tx_frm_queue.push_back(frm);   
                // @(posedge clk) tx_flush = 1;
                // @(posedge clk) tx_flush = 0; 
            // end      
            // delay_frame();
            // join
            // wait (tx_frame_sent);
            // test_num ++;
            
     assert (flush_test == 1 && timeout_test == 1 && max_words_test == 1 && 
             max_words_test == 1 && flatency_test == 1 && frm_drop_test == 1 && 
             comparator_test == 1) 
     else begin
	      $error("Streamers implementation contains errors", $time);
          $fatal;
	 end   



     
end

   //---------------------------------------------------------------------------
   // DATA MONITOR
   // Client-side reception logic. Compares the received records with their copies
   // stored in the transfer queue.  
   int new_block = 1;
   
   always@(posedge clk)
 
     if(rst_n)
       begin
          block_t rblk;
          streamer_frame_t tfrm, l_tfrm;
          automatic int done = 0;        
          
          if (rx_streamer_lost_frm == 1) 
              begin 
                int i,  n_lost_frames;
                n_lost_frames = rx_streamer_lost_frm_cnt;
                for (i = 0; i < n_lost_frames; i++) 
                    begin
                        l_tfrm = tx_frm_queue.pop_front();
                    end
              end           
          receive_block(rblk, new_block, done); 
          if(done)
            begin
               automatic block_t tblk;                       
              if (tfrm.blocks.size() == 0) 
                   tfrm = tx_frm_queue.pop_front();
              tblk = tfrm.blocks.pop_front();                   
              new_block = 1;                
              if(tblk.words != rblk.words)
                begin
                    $error("[%t ns]: >> FAILED - TEST - DATA MONITOR \n", $time );
                    //$display("Txed is %p, Rxed equals %p", tblk, rblk);
                    comparator_test = 0;
                end 
              else  $display ("[%t ns]: PASSED - TEST - DATA MONITOR \n", $time );
                    
              // end //while (tfrm.blocks.size() > 0)
            end // if (done)
       end // else: !if(!rst_n)

endmodule // main

