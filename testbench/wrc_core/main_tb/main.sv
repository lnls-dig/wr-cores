`timescale 1ns/1ps

   
`include "tbi_utils.sv"

`include "simdrv_defs.svh"
`include "if_wb_master.svh"
`include "wrc_syscon_regs.vh"
`include "if_wb_slave.svh"
`include "wb_packet_source.svh"
`include "wb_packet_sink.svh"
`include "if_wb_link.svh"

`define BASE_SYSCON 'h20400


module main;

  wire clk_ref;
  wire clk_sys;
  wire rst_n;

  IWishboneMaster WB (
    .clk_i(clk_sys),
    .rst_n_i(rst_n));

  IWishboneMaster #(2,16) U_ep_src (
    .clk_i(clk_sys),
    .rst_n_i(rst_n));

  IWishboneSlave  #(2,16) U_ep_snk (
    .clk_i(clk_sys),
    .rst_n_i(rst_n));

  WBPacketSource ep_src;
  WBPacketSink ep_snk;
	 
  tbi_clock_rst_gen
  #(
    .g_rbclk_period(8000))
  clkgen(
    .clk_ref_o(clk_ref),
    .clk_sys_o(clk_sys),
    .phy_rbclk_o(phy_rbclk),
    .rst_n_o(rst_n)
  );

  wire   [7:0]phy_tx_data      ;
  wire   phy_tx_k         ;
  wire   phy_tx_disparity ;
  wire   phy_tx_enc_err   ;
  wire   [7:0]phy_rx_data      ;
  wire   phy_rx_rbclk     ;
  wire   phy_rx_k         ;
  wire   phy_rx_enc_err   ;
  wire   [3:0]phy_rx_bitslide  ;
  wire   phy_rst          ;
  wire   phy_loopen;

  wr_core #(
    .g_simulation             (1),
    .g_interface_mode(PIPELINED),
    .g_address_granularity(BYTE),
    .g_tx_runt_padding(1),
    .g_with_external_clock_input(1),
    .g_dpram_initf            ("sw/wrc.ram"),
    .g_dpram_size             (131072/4))
    DUT (
    .clk_sys_i      (clk_sys),
    .clk_dmtd_i     (clk_ref),
    .clk_ref_i      (clk_ref),
    .clk_aux_i      (clk_ref),
    .rst_n_i        (rst_n),

    .pps_p_o        (),
    .dac_hpll_load_p1_o (),
    .dac_hpll_data_o (),

    .dac_dpll_load_p1_o (),
    .dac_dpll_data_o (),

    .uart_rxd_i       (1'b0),
    .uart_txd_o       (),

    .scl_i(scl_loop),
    .scl_o(scl_loop),
    .sda_i(sda_loop),
    .sda_o(sda_loop),

    .btn1_i(1'b0),
    .btn2_i(1'b0),

    .ext_snk_adr_i	(U_ep_src.adr[1:0]),
    .ext_snk_dat_i	(U_ep_src.dat_o),
    .ext_snk_sel_i	(U_ep_src.sel),
    .ext_snk_cyc_i	(U_ep_src.cyc),
    .ext_snk_we_i 	(1'b1),
    .ext_snk_stb_i	(U_ep_src.stb),
    .ext_snk_ack_o	(U_ep_src.ack),
    .ext_snk_err_o	(U_ep_src.err),
    .ext_snk_stall_o(U_ep_src.stall),
    .ext_src_adr_o	(U_ep_snk.adr),
    .ext_src_dat_o	(U_ep_snk.dat_i),
    .ext_src_sel_o	(U_ep_snk.sel),
    .ext_src_cyc_o  (U_ep_snk.cyc),
    .ext_src_stb_o  (U_ep_snk.stb),
    .ext_src_we_o   (U_ep_snk.we),
    .ext_src_ack_i  (U_ep_snk.ack),
    .ext_src_err_i  (U_ep_snk.err),
    .ext_src_stall_i(U_ep_snk.stall),

    .wb_adr_i      (WB.master.adr[31:0]),
    .wb_dat_i      (WB.master.dat_o),
    .wb_dat_o      (WB.master.dat_i),
    .wb_sel_i       (4'b1111),
    .wb_we_i        (WB.master.we),
    .wb_cyc_i       (WB.master.cyc),
    .wb_stb_i       (WB.master.stb),
    .wb_ack_o       (WB.master.ack),
    .wb_stall_o     (WB.master.stall),

    .phy_ref_clk_i(clk_ref),
    .phy_tx_data_o(phy_tx_data),
    .phy_tx_k_o(phy_tx_k),
    .phy_tx_disparity_i(phy_tx_disparity),
    .phy_tx_enc_err_i(phy_tx_enc_err),
    .phy_rx_data_i(phy_rx_data),
    .phy_rx_rbclk_i(clk_ref),
    .phy_rx_k_i(phy_rx_k),
    .phy_rx_enc_err_i(phy_rx_enc_err),
    .phy_rx_bitslide_i(phy_rx_bitslide),
    .phy_rst_o(phy_rst),
    .phy_loopen_o(phy_lo)
  );

  assign phy_rx_data       = phy_tx_data;
  assign phy_rx_k          = phy_tx_k;
  assign phy_tx_disparity  = 0;
  assign phy_tx_enc_err    = 0;
  assign phy_rx_enc_err    = 0;

	int tx_sizes[$], tx_padded[$];

  //////////////////////////////////////
  task send_frames(WBPacketSource src, int n_packets);
    int i, seed = 0,n1=0,n2=0;
    int cur_size, dir;
    EthPacket pkt, tmpl;
    EthPacket to_ext[$], to_minic[$];
    EthPacketGenerator gen  = new;
    
    tmpl                = new;
    tmpl.src                = '{1,2,3,4,5,6};
    tmpl.dst                = '{'hff,'hff,'hff,'hff,'hff,'hff};
    //tmpl.dst                = '{'h01,'h1b,'h19,'h00,'h00,'h00}; // PTP dst MAC
    tmpl.has_smac           = 1;
    tmpl.is_q               = 0;
    //tmpl.ethertype	=	{'h0800};
    tmpl.ethertype	=	{'hdbff};
    //tmpl.ethertype	=	{'h88f7};
    
    //gen.set_randomization(EthPacketGenerator::SEQ_PAYLOAD | EthPacketGenerator::ETHERTYPE /*| EthPacketGenerator::RX_OOB*/) ;
    gen.set_randomization(EthPacketGenerator::SEQ_PAYLOAD ) ;
    gen.set_template(tmpl);
    gen.set_size(1, 1500);

    cur_size = 0;
    dir = 1;
    for(i=0;i<n_packets;i++) begin
      /* switch between incrementing/decrementing */
      if(cur_size == 1495)
        dir = 0;
      if(cur_size == 1)
        dir = 1;
      /* increment/decrement frame size, based on dir */
      if(dir == 1)
        cur_size += 1;
      else
        cur_size -= 1;

      pkt         = gen.gen(cur_size);
      //pkt         = gen.gen();
      tx_sizes = {tx_sizes, pkt.size};
      tx_padded = {tx_padded, padded_size(pkt)};
      src.send(pkt);
    end
  endtask

  function int nopad_size(EthPacket pkt);
    int i;
    if(pkt.size > 64)
      nopad_size = pkt.size;
    else begin
      nopad_size = 1;
      for(i=1; i<pkt.size; i++) begin
        if(pkt.payload[i]==0) break;
        nopad_size = nopad_size + 1;
      end
      nopad_size = nopad_size + 14; //+header
    end
  endfunction;

  function int padded_size(EthPacket pkt);
    if(pkt.size < 60) padded_size = 60;
    else padded_size = pkt.size;
  endfunction;

  function int find_pkt_size(EthPacket pkt, int start, int limit);
    int i;
    for(i=start; i<start+limit; i++) begin
      if(pkt.size == tx_sizes[i])
        return i;
    end
    return -1;
  endfunction;
  //////////////////////////////////////

  initial begin
    CWishboneAccessor acc;

    @(posedge rst_n);
    repeat(3) @(posedge clk_sys);

    #1us;

    acc  = WB.get_accessor();
    acc.set_mode(PIPELINED);
    WB.settings.cyc_on_stall = 1;

    ep_src = new(U_ep_src.get_accessor());
    U_ep_src.settings.cyc_on_stall = 1;

    #1us
    acc.write(`BASE_SYSCON + `ADDR_SYSC_RSTR, 'hdeadbee | `SYSC_RSTR_RST);
    #1us;
    acc.write(`BASE_SYSCON + `ADDR_SYSC_RSTR, 'hdeadbee );

    #400us;
    tx_sizes = {};
    //NOW LET'S SEND SOME FRAMES
    send_frames(ep_src, 3000);

  end

  initial begin
    EthPacket pkt;
    int i = 0, correct = 0, j;
    int drop_first = 1;
    int size_pos;

    U_ep_snk.settings.gen_random_stalls = 1;
    ep_snk = new(U_ep_snk.get_accessor());
    while(1) begin
      ep_snk.recv(pkt);
      size_pos = find_pkt_size(pkt, i, 5);

      assert (pkt.size == tx_padded[i] && nopad_size(pkt) == tx_sizes[i]) begin
        correct = correct + 1;
        $display("--> recv: size=%4d, nopad=%4d", pkt.size, nopad_size(pkt));
      end
      else assert (pkt.error == 1'b1 || (pkt.size == tx_padded[size_pos] && nopad_size(pkt) == tx_sizes[size_pos])) begin
        if(size_pos == -1)
          $warning("(%1d) Lost frame with size: %4d", pkt.error, tx_sizes[i]);
        for(j=i; j<size_pos; j++) 
          $warning("(%1d) Lost frame with size: %4d", pkt.error, tx_sizes[j]);
        if(pkt.error == 1'b0) i = size_pos;
        correct = correct + 1;
        $display("--> recv: size=%4d, nopad=%4d", pkt.size, nopad_size(pkt));
      end  
      else begin
        $display("Frame dump:");
        for(j=0; j<pkt.size-14; j++) begin
          $write("0x%02X ", pkt.payload[j]);
        end
        $fatal("(%1d) Size does not match: pkt.size=%4d, nopad.size=%4d, padded=%4d,
        sizes=%4d", pkt.error, pkt.size, nopad_size(pkt), tx_padded[i], tx_sizes[i]); 
      end
      i = i+1;
    end
  end

endmodule // main
