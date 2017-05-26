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

   logic [1 : 0]   adr_d1;
   logic [1 : 0] sel_d1 ;     
   logic cyc_d1;
   logic stb_d1;
   logic we_d1 ;
   logic [1 : 0]   adr;
   logic [1 : 0] sel ;     
   logic cyc;
   logic stb;
   logic we;

   // Clock & reset
   reg clk = 0;
   reg rst_n = 0;
   reg [27:0] clk_cycle_counter = 0;           
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
   
   //Fixed latency
   reg  [27:0] fixed_latency = 28'h0;


   // Fake White Rabbit reference clock (125 MHz) and cycle counter (we don't use 
   // TAI counter as the latency never exceeds 1 second...)

   reg                      clk_ref = 0;
   reg [27:0]               tm_cycle_counter = 0;

   // Currently transmitted counter value
   integer                  tx_counter_val = 10;
   
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
   always@(posedge tx_frame_sent) clk_cycle_counter_before = clk_cycle_counter;
   always@(posedge rx_frame_received) clk_cycle_counter_after = clk_cycle_counter;
     
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
   streamer_frame_t tx_frm_queue[$];    

    
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
     //
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
      .cfg_ethertype_i  (g_ethertype),
      .cfg_accept_broadcasts_i (),
      .cfg_filter_remote_i     (),
      .cfg_fixed_latency_i     (fixed_latency) //(28'd2000)
      
      );

   /////////////////////////////////////////////////////////////////////////////
   // Task definitions
   /////////////////////////////////////////////////////////////////////////////
   
   
   // Generate a block of data words of random size, containing subsequent 
   // numbers
   /////////////////////////////////////////////////////////////////////////////
   
   task automatic generate_block(ref block_t blk);
   
      int size = $dist_uniform(seed, g_block_size_min, g_block_size_max);
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
    task automatic generate_frame(ref streamer_frame_t frm);
   
      int size = $dist_uniform(seed, g_frame_size_min, g_frame_size_max);
      int i;
      block_t blk;
      for(i = 0; i<size; i++) 
        begin
            blk.words = {};
            blk.wrd_cnt = {};
            generate_block(blk);
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
              //$display("Data to be sent is %d*****\n", tx_streamer_data);
              tx_streamer_dvalid <= 1;
              i++;
           end else
             tx_streamer_dvalid <= 0;
              
           @(posedge clk);
        end // while (i < blk.words.data.size())
        tx_streamer_dvalid <= 0;
        tx_streamer_last <= 0;
      
   endtask // send_block
///////////////////////////////////////////////////////////////////

      
    task automatic send_frame(ref streamer_frame_t frm);
        int i = 0;
        
        while (i < frm.blocks.size()) begin
            send_block(frm.blocks[i]);
            i++;
        end
    endtask   
    
///////////////////////////////////////////////////////////////////
// WISHBONE LINK CONTROL
///////////////////////////////////////////////////////////////////

//Continuous assignements to WR fabric signals
  

      logic [ 15 : 0] corrupt_mask = 16'h0000;
      logic drop_frm = 0;
      logic link_ok= 1;
      logic delay_link= 0;
      

    task automatic link_good ();
        int frm_counter = 0;
        //$display ("LINKOK---------");
        @(posedge tx_frame_sent) link_ok = 1;
        corrupt_mask = 16'h0000;
        drop_frm = 0;
        delay_link= 0;
        frm_counter=0;
        while (frm_counter < 3 ) begin
        @(posedge tx_wb_cyc) //at every new frame
            frm_counter ++;
        end
    endtask; 
            

    task automatic corrupt_data ();
        int i, j, n;
        
        int frm_counter = 0;
        corrupt_mask = 16'h0000;
        link_ok= 0;
        n={$random} % 2; //number of frames to be corrupted
        j = {$random} % 15;
        $display("=====BIT FLIP================");
        @(negedge rx_frame_received) 
        corrupt_mask [j] = ~corrupt_mask [j];
        @(negedge rx_wb_ack) link_good ();

    endtask //corrupt_data
      
task automatic drop_frame ();
    int n, i;
    link_ok= 0;
    n={$random} % 5; //number of frames to be dropped
       for (i=0; i<n; i++) 
         begin
                @(negedge tx_wb_cyc)
                drop_frm = 1;  
         end           
        @(negedge tx_wb_cyc)
        drop_frm = 0;
       
    endtask //drop_frame     

     
    
    task automatic delay_frame ();
        link_ok= 0;
        delay_link = 0;        
        wait (rx_wb_stall == 1)  //to avoid changes at startup
        // @(negedge rx_wb_stall)
        @(posedge tx_frame_sent)
            delay_link = 1;
            
        #10000;
        delay_link = 0;
        link_good ();
    
endtask //delay_frame


    assign data_to_rx = data_from_tx ^ corrupt_mask;
    assign rx_wb_stb = tx_wb_stb & ~drop_frm;
    assign rx_wb_cyc = tx_wb_cyc & ~drop_frm;
    assign tx_wb_stall = rx_wb_stall | delay_link; //extend pulse
    assign tx_wb_ack = rx_wb_ack;// & ~delay_link;   
    
    // assign tx_wb_stall = delay_link; //extend pulse
   // assign tx_wb_stall = ~rx_wb_cyc ? (1'b0 | delay_link)  : (~rx_wb_ack | delay_link);     
  

      
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
   
            //$display("BEFORE valid streamer block--------");
    if(rx_streamer_dvalid)
        begin
            //$display("valid streamer block--------");
            if(rx_streamer_first && new_block == 1) 
                begin
                    $display("streamer first---------");
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
            //$display(" streamer last-------------");
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
   
   
      // TX block stream generation

    initial forever 
        begin
            //int i;
            streamer_frame_t frm;
            frm.blocks = {};
            
            wait(rst_n == 1'b1);
            
            rx_streamer_dreq  <= 1;//({$random} % 100 < 90) ? 1 : 0;
            
            //Rx Test1 fixed latency test
            
            //fixed_latency <= 28'h200;
            generate_frame(frm);
            for (int i=0; i < frm.blocks.size(); i++) begin
                frm.blocks[i].dropped = drop_frm;
                tx_blk_queue.push_back(frm.blocks[i]);
               
            end             
            send_frame(frm);
            @(posedge clk) tx_flush = 1;
            @(posedge clk) tx_flush = 0;
            tx_frm_queue.push_back(frm); 
            
            wait(tx_frame_sent);
            //wait (rx_latency_valid) $display("Latency received is %d---\n", rx_latency);
            

        end
        
/*     initial forever 
        begin 
            //randcase
               //10 : drop_frame ();
               //10 : corrupt_data ();
               // link_good ();
               //10 : delay_frame ();
             //endcase;
        end */

   


   
   // TESTBENCH VERIFICATION
   // Client-side reception logic. Compares the received records with their copies
   // stored in the transfer queue.  
   int new_block = 1;
   int no_curr_req = 0;
   
           
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
                    for (i = 0; i < n_lost_frames; i++) begin
                        l_tfrm = tx_frm_queue.pop_front();
                    //$display ("%d have been lost, frame %p is POPPED\n=====", n_lost_frames, l_tfrm );
                    end
                    //$display ("%d have been lost, the new Tx queue is %p\n=====", n_lost_frames, tx_frm_queue );
                  end
                
                receive_block(rblk, new_block, done);  
          
          if(done)
            begin
               automatic block_t tblk;                  
               //$display(" frame OUT is %p \n********",  tx_blk_queue);
               //if (tx_blk_queue.size() != 0) 
               //else begin
                tblk = tx_blk_queue.pop_front();              // $display(" OLD TBLK is %p \n********",  tblk);
                //$display(" FRAME size is  %d \n%%%%%%%%%%%%",  tfrm.blocks.size());
                if (tfrm.blocks.size() == 0) begin
                    tfrm = tx_frm_queue.pop_front();
                    //$display(" FRAME is  %p \n%%%%%%%%%%%%",  tfrm); end
                tblk = tfrm.blocks.pop_front();               //$display(" NEW TBLK is %p \n********",  tblk);
                //$display(" FRAME AFTER POP is  %p \n%%%%%%%%%%%%",  tfrm);
              // $display(" QUEUE is  %p \n%%%%%%%%%%%%",  tx_blk_queue);
                 
               //end
               
               
              // $display(" block OUT is %p \n********",  tblk);
               
               //$display("Received block of %d words....\n", tblk_size);
               new_block = 1;
                
                
              // ===============================================================
              // TEST 1: Check Txed and Rxed blocks match, first and last word are signalled correctly
               if(tblk.words != rblk.words)
                 begin
                    $error("TEST 1 ---> FAILED\n####Sent block does not match received block\n");
                    $display("Txed is %p, Rxed equals %p", tblk, rblk);
                    $stop;
                 end else begin 
                 $display("****\nTEST 1 ---> PASSED\n****Correct words received\n");
                 end


              // TEST 5: Check that when rx_dreq is not asserted data stops at the next clk cycle
               if (rx_streamer_dreq == 1) 
                 no_curr_req = 0;
               else if (rx_streamer_dreq == 0 && no_curr_req == 0)
                 no_curr_req = 1;
               else if (no_curr_req == 1 && rx_streamer_dvalid == 1)
                 begin
                    $error("TEST 5 ---> FAILED\n####Pulse rx_request is not asserted should not receive data.\n");
                    $stop;
                 end
               else begin
                 $display("****\nTEST 5 ---> PASSED\n****No data output when Rx_request is not asserted\n");
               end
                 
                                   
            end
       end // else: !if(!rst_n)
end


  always@(posedge clk)

    if (delay_link == 0) 
        begin
         
            if(rst_n && rx_latency_valid) 
                begin
                wait (rx_frame_received)
                $display ("Latency calculated %d\n", clk_cycle_counter_before-clk_cycle_counter_after);
                $display("*************This frame's latency: %.3f microseconds*************************************************\n", (rx_latency));
             
         end
   
   end
endmodule // main

