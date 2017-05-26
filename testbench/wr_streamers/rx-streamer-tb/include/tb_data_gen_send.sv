class CGENERATE_AND_SEND


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
        @(posedge clk);
      
   endtask // send_block
///////////////////////////////////////////////////////////////////

      
    task automatic send_frame(ref streamer_frame_t frm);
        int i = 0;
        
        while (i < frm.blocks.size()) begin
            send_block(frm.blocks[i]);
            i++;
        end
    endtask   




endclass