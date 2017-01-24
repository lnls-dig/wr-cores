`include "if_wb_master.svh"
`include "if_wb_slave.svh"
`include "wb_packet_source.svh"
`include "wb_packet_sink.svh"

`include "drivers/simdrv_minic.svh"
//`include "ep2ep_wrapper.svh"

`define TEST_TX 1
`define TEST_RX 1
`define NFRAMES 3000

module main;

  //-------------------------------------------
  // Functional coverage analysis
  //-------------------------------------------
  reg txp_cg_newframe, rxp_cg_newframe;
  int txp_cg_size, rxp_cg_size;
  covergroup cg @(txp_cg_newframe == 1 or rxp_cg_newframe == 1);
    coverpoint txp_cg_size iff (txp_cg_newframe == 1){
      bins pkt_sizes[1440] = {[60:1500]};
    }
    coverpoint rxp_cg_size iff (rxp_cg_newframe == 1){
      bins pkt_sizes[1440] = {[60:1500]};
    }
  endgroup
  cg cover_group = new;

  reg clk_sys = 1'b0;
  reg rst_n = 1'b0;
  
  always #5ns clk_sys <= ~clk_sys;
  initial begin
    repeat(3) @(posedge clk_sys);
    rst_n <= 1'b1;
  end

  IWishboneMaster #(
    .g_data_width(16),
    .g_addr_width(2))
  U_wrf_source (
    .clk_i(clk_sys),
    .rst_n_i(rst_n)
  );

  IWishboneSlave #(
    .g_data_width(16),
    .g_addr_width(2))
  U_wrf_sink (
    .clk_i(clk_sys),
    .rst_n_i(rst_n)
  );

  IWishboneMaster #(
    .g_data_width(32),
    .g_addr_width(32))
  U_sys_bus_master (
    .clk_i(clk_sys),
    .rst_n_i(rst_n)
  );

  wr_mini_nic #(
    .g_interface_mode(PIPELINED),
    .g_address_granularity(BYTE))
  DUT (
    .clk_sys_i (clk_sys),
    .rst_n_i   (rst_n),

    .src_dat_o  (U_wrf_sink.dat_i),
    .src_adr_o  (U_wrf_sink.adr),
    .src_sel_o  (U_wrf_sink.sel),
    .src_cyc_o  (U_wrf_sink.cyc),
    .src_stb_o  (U_wrf_sink.stb),
    .src_we_o   (U_wrf_sink.we),
    .src_stall_i(U_wrf_sink.stall),
    .src_err_i  (U_wrf_sink.err),
    .src_ack_i  (U_wrf_sink.ack),

    .snk_dat_i  (U_wrf_source.dat_o), 
    .snk_adr_i  (U_wrf_source.adr),
    .snk_sel_i  (U_wrf_source.sel),
    .snk_cyc_i  (U_wrf_source.cyc),
    .snk_stb_i  (U_wrf_source.stb), 
    .snk_we_i    (U_wrf_source.we),
    .snk_stall_o (U_wrf_source.stall),
    .snk_err_o   (U_wrf_source.err),
    .snk_ack_o   (U_wrf_source.ack),

    .txtsu_port_id_i  (5'b0),
    .txtsu_frame_id_i (16'b0),
    .txtsu_tsval_i    (32'b0),
    .txtsu_stb_i      (1'b0),
    .txtsu_ack_o      (),

    .wb_cyc_i   (U_sys_bus_master.cyc),
    .wb_stb_i   (U_sys_bus_master.stb),
    .wb_we_i    (U_sys_bus_master.we),
    .wb_sel_i   (U_sys_bus_master.sel),
    .wb_adr_i   (U_sys_bus_master.adr),
    .wb_dat_i   (U_sys_bus_master.dat_o),
    .wb_dat_o   (U_sys_bus_master.dat_i),
    .wb_ack_o   (U_sys_bus_master.ack),
    .wb_stall_o (U_sys_bus_master.stall)
  );

  CSimDrv_Minic minic;

  function int fid(EthPacket pkt);
    return ((pkt.payload[1] << 8) & 'hff00) | pkt.payload[0];
  endfunction

  task prep_rx_test(output EthPacketGenerator gen);
    EthPacket tmpl;
    gen = new;

    tmpl = new;
    tmpl.src       = '{10,11,12,13,14,15};
    tmpl.dst       = '{1,2,3,4,5,6};
    tmpl.has_smac  = 1;
    tmpl.is_q      = 0;

    gen.set_randomization(EthPacketGenerator::SEQ_PAYLOAD | EthPacketGenerator::RX_OOB);
    gen.set_template(tmpl);
    gen.set_size(60,1500);
  endtask

  task prep_tx_test(output EthPacketGenerator gen);
    EthPacket tmpl;
    gen = new;

    tmpl           = new;
    tmpl.src       = '{1,2,3,4,5,6};
    tmpl.dst       = '{10,11,12,13,14,15};
    tmpl.has_smac  = 1;
    tmpl.is_q      = 0;

    gen.set_randomization(EthPacketGenerator::SEQ_PAYLOAD | EthPacketGenerator::TX_OOB);
    gen.set_template(tmpl);
    gen.set_size(60,1500);
  endtask

   
   initial begin
      CWishboneAccessor sys_bus;     
      WBPacketSource wrf_src  = new(U_wrf_source.get_accessor());
      WBPacketSink   wrf_sink = new(U_wrf_sink.get_accessor()); 
      EthPacket rx_pkt, tx_pkt;
      EthPacket rx_frames[$], tx_frames[$];
      int txp_minic_frames, txp_snk_frames, txp_lost_frames;
      int rxp_minic_frames, rxp_src_frames, rxp_lost_frames;

      EthPacketGenerator gen_rx, gen_tx;
      int i;

      
      @(posedge rst_n);
      @(posedge clk_sys);
 
      // Configure wishbone/fabric interfaces
      sys_bus  = U_sys_bus_master.get_accessor();
      sys_bus.set_mode(PIPELINED);
      U_sys_bus_master.settings.cyc_on_stall = 1;
      U_sys_bus_master.settings.addr_gran = WORD;
      U_wrf_sink.settings.gen_random_stalls = 1;
      U_wrf_source.settings.cyc_on_stall = 1;
      U_wrf_source.settings.gen_random_throttling = 1;

      // Configure Minic
      minic  = new(sys_bus, 0);
      minic.init();

      // Configure frame generators
      prep_rx_test(gen_rx);
      prep_tx_test(gen_tx);

      #40us;
      @(posedge clk_sys);

      force DUT.irq_tx_mask = 1;
      force DUT.irq_tx_ack  = 1;
      force DUT.irq_rx_ack = 1;
      txp_minic_frames = 0;
      txp_snk_frames   = 0;
      txp_lost_frames  = 0;
      rxp_minic_frames = 0;
      rxp_src_frames   = 0;
      rxp_lost_frames  = 0;

      fork
        // 1st process, Minic operations, sending and receiving frames
        forever begin
          minic.run();
          txp_cg_newframe = 0;

          // TEST_RX Minic: receive frame
          if(`TEST_RX && minic.poll()) begin
            EthPacket rxp, orig;

            minic.recv(rxp);
            orig    = rx_frames.pop_front();
            while ( fid(orig) < fid(rxp)) begin
              $warning("RXP: Lost frame %x", fid(orig));
              orig    = rx_frames.pop_front();
              rxp_lost_frames += 1;
            end
            //if(!sent.equal(rxp, EthPacket::CMP_OOB)) begin
            if(!orig.equal(rxp)) begin
              //orig.dump();
              //rxp.dump();
              $warning("RXP: Wrong frame received");
              rxp_lost_frames += 1;
            end
            else
              rxp_minic_frames += 1;
          end //minic.poll

          // TEST_TX Minic: send frame
          if (`TEST_TX && txp_minic_frames < `NFRAMES) begin
            tx_pkt = gen_tx.gen();
            minic.send(tx_pkt);
            tx_frames.push_back(tx_pkt);
            txp_minic_frames += 1;
            txp_cg_newframe = 1;
            txp_cg_size = tx_pkt.size;
          end
          else
            txp_cg_newframe = 0;

          #1;
        end

        // TEST_TX Receive frames sent by Minic
        forever begin
          if(`TEST_TX && wrf_sink.poll()) begin
            EthPacket rxp, orig;
            wrf_sink.recv(rxp);
            orig  = tx_frames.pop_front();
            assert (orig.equal(rxp, EthPacket::CMP_OOB)) begin // compare original frame, with received frame
              txp_snk_frames += 1;
            end
            else begin
              $warning("TXP: Wrong frame received or frame lost");
              orig.dump();
              rxp.dump();
              txp_lost_frames += 1;
            end
          end
          #1;
        end

        // 3rd: Send frames to Minic
        if (`TEST_RX) begin
           #40ns;
           rxp_cg_newframe = 1;
           for(i=0;i<`NFRAMES;i++) begin
              rx_pkt  = gen_rx.gen();
              rx_pkt.payload[0] = i & 'hff;
              rx_pkt.payload[1] = (i & 'hff00) >> 8;
              rxp_cg_size = rx_pkt.size;
              wrf_src.send(rx_pkt);
              rx_frames.push_back(rx_pkt);
              rxp_src_frames += 1;
              #45us;
           end
           rxp_cg_newframe = 0;
        end

      join      

   end // initial begin

   
   
endmodule // main



