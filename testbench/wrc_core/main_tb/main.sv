`timescale 1ns/1ps

   
`include "tbi_utils.sv"

`include "simdrv_defs.svh"
`include "if_wb_master.svh"
`include "wrc_syscon_regs.vh"
`include "if_wb_slave.svh"
`include "wb_packet_source.svh"
`include "wb_packet_sink.svh"
`include "if_wb_link.svh"
`include "functions.svh"
`include "drivers/simdrv_minic.svh"
`include "drivers/simdrv_wr_endpoint.svh"
`include "lbk_regs.v"

//import wishbone_pkg::*;

`define BASE_SYSCON 'h20400
`define BASE_MINIC  'h20000


module main;

  wire clk_ref;
  wire clk_sys;
  wire rst_n;

	/* WB masters */
  IWishboneMaster WB_wrc (clk_sys, rst_n);
  IWishboneMaster WB_ep  (clk_sys, rst_n);
  IWishboneMaster WB_lbk (clk_sys, rst_n);

	/* WB accessors */
  CWishboneAccessor acc_wrc, acc_ep, acc_lbk;

	/* Fabrics */
  IWishboneMaster #(2,16) WB_wrc_src (clk_sys, rst_n);
  IWishboneSlave  #(2,16) WB_wrc_snk (clk_sys, rst_n);
  IWishboneMaster #(2,16) WB_ep_src  (clk_sys, rst_n);
  IWishboneSlave  #(2,16) WB_ep_snk  (clk_sys, rst_n);

	/* Fabrics accessors */
  WBPacketSource wrc_src, ep_src;
  WBPacketSink   wrc_snk, ep_snk;

	/* VHDL package types */
	//wire t_wishbone_master_out vhd_epwbm_o;
	//wire t_wishbone_master_in	vhd_epwbm_i;
	//wire t_wrf_source_out	vhd_epsrc_o;
	//wire t_wrf_source_in		vhd_epsrc_i;
	//wire t_wrf_sink_out		vhd_epsnk_o;
	//wire t_wrf_sink_in			vhd_epsnk_i;
	 
  tbi_clock_rst_gen
  #(
    .g_rbclk_period(8000))
  clkgen(
    .clk_ref_o(clk_ref),
    .clk_sys_o(clk_sys),
    .phy_rbclk_o(phy_rbclk),
    .rst_n_o(rst_n)
  );

  wire   [7:0]phy_tx_data;
  wire   phy_tx_k;
  wire   phy_tx_disparity;
  wire   phy_tx_enc_err;
  wire   [7:0]phy_rx_data;
  wire   phy_rx_rbclk;
  wire   phy_rx_k;
  wire   phy_rx_enc_err;
  wire   [3:0]phy_rx_bitslide;
  wire   phy_rst;
  wire   phy_loopen;
	// set of wires between WRC and WRF_LBK
	wire wrc_src_cyc;
	wire wrc_src_stb;
	wire [1:0] wrc_src_sel;
	wire [1:0] wrc_src_adr;
	wire [15:0] wrc_src_dat;
	wire wrc_src_ack;
	wire wrc_src_stall;
	wire wrc_src_err;
	wire wrc_snk_cyc;
	wire wrc_snk_stb;
	wire [1:0] wrc_snk_sel;
	wire [1:0] wrc_snk_adr;
	wire [15:0] wrc_snk_dat;
	wire wrc_snk_ack;
	wire wrc_snk_stall;
	wire wrc_snk_err;

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

    //.ext_snk_adr_i	(WB_wrc_src.adr[1:0]),
    //.ext_snk_dat_i	(WB_wrc_src.dat_o),
    //.ext_snk_sel_i	(WB_wrc_src.sel),
    //.ext_snk_cyc_i	(WB_wrc_src.cyc),
    //.ext_snk_we_i 	(1'b1),
    //.ext_snk_stb_i	(WB_wrc_src.stb),
    //.ext_snk_ack_o	(WB_wrc_src.ack),
    //.ext_snk_err_o	(WB_wrc_src.err),
    //.ext_snk_stall_o(WB_wrc_src.stall),
    //.ext_src_adr_o	(WB_wrc_snk.adr),
    //.ext_src_dat_o	(WB_wrc_snk.dat_i),
    //.ext_src_sel_o	(WB_wrc_snk.sel),
    //.ext_src_cyc_o  (WB_wrc_snk.cyc),
    //.ext_src_stb_o  (WB_wrc_snk.stb),
    //.ext_src_we_o   (WB_wrc_snk.we),
    //.ext_src_ack_i  (WB_wrc_snk.ack),
    //.ext_src_err_i  (WB_wrc_snk.err),
    //.ext_src_stall_i(WB_wrc_snk.stall),

    .ext_snk_adr_i	(wrc_snk_adr),
    .ext_snk_dat_i	(wrc_snk_dat),
    .ext_snk_sel_i	(wrc_snk_sel),
    .ext_snk_cyc_i	(wrc_snk_cyc),
    .ext_snk_we_i 	(1'b1),
    .ext_snk_stb_i	(wrc_snk_stb),
    .ext_snk_ack_o	(wrc_snk_ack),
    .ext_snk_err_o	(wrc_snk_err),
    .ext_snk_stall_o(wrc_snk_stall),
    .ext_src_adr_o	(wrc_src_adr),
    .ext_src_dat_o	(wrc_src_dat),
    .ext_src_sel_o	(wrc_src_sel),
    .ext_src_cyc_o  (wrc_src_cyc),
    .ext_src_stb_o  (wrc_src_stb),
    .ext_src_we_o   (),
    .ext_src_ack_i  (wrc_src_ack),
    .ext_src_err_i  (wrc_src_err),
    .ext_src_stall_i(wrc_src_stall),

    .wb_adr_i      (WB_wrc.master.adr[31:0]),
    .wb_dat_i      (WB_wrc.master.dat_o),
    .wb_dat_o      (WB_wrc.master.dat_i),
    .wb_sel_i      (4'b1111),
    .wb_we_i       (WB_wrc.master.we),
    .wb_cyc_i      (WB_wrc.master.cyc),
    .wb_stb_i      (WB_wrc.master.stb),
    .wb_ack_o      (WB_wrc.master.ack),
    .wb_stall_o    (WB_wrc.master.stall),

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

	wr_endpoint #(
		.g_interface_mode(PIPELINED),
		.g_address_granularity(BYTE),
		.g_simulation 		(1),
		.g_pcs_16bit      (0),
		.g_rx_buffer_size (1024),
		.g_with_rx_buffer (1),
		.g_with_timestamper (0),
		.g_with_dpi_classifier (0),
		.g_with_vlans (0),
		.g_with_rtu   (0))
	EP (
		.clk_ref_i(clk_ref),
		.clk_sys_i(clk_sys),
		.clk_dmtd_i(clk_ref),
		.rst_n_i(rst_n),
		.pps_csync_p1_i(1'b0),
		
		.phy_sfp_tx_fault_i(1'b0),
		.phy_sfp_los_i(1'b0),
		.phy_rdy_i(1'b1),
		.phy_ref_clk_i(clk_ref),
		.phy_tx_data_o(phy_rx_data),
		.phy_tx_k_o(phy_rx_k),
		.phy_tx_disparity_i(phy_tx_disparity),
		.phy_tx_enc_err_i(phy_tx_enc_err),
		.phy_rx_data_i(phy_tx_data),
		.phy_rx_clk_i(clk_ref),
		.phy_rx_k_i(phy_tx_k),
		.phy_rx_enc_err_i(phy_rx_enc_err),
		.phy_rx_bitslide_i(phy_rx_bitslide),

		.src_dat_o  (WB_ep_snk.slave.dat_i),
		.src_adr_o  (WB_ep_snk.slave.adr),
		.src_sel_o  (WB_ep_snk.slave.sel),
		.src_cyc_o  (WB_ep_snk.slave.cyc),
		.src_stb_o  (WB_ep_snk.slave.stb),
		.src_we_o   (WB_ep_snk.slave.we),
		.src_stall_i(WB_ep_snk.slave.stall),
		.src_ack_i  (WB_ep_snk.slave.ack),
		.src_err_i  (1'b0),
			
		.snk_dat_i  (WB_ep_src.master.dat_o),
		.snk_adr_i  (WB_ep_src.master.adr),
		.snk_sel_i  (WB_ep_src.master.sel),
		.snk_cyc_i  (WB_ep_src.master.cyc),
		.snk_stb_i  (WB_ep_src.master.stb),
		.snk_we_i   (WB_ep_src.master.we),
		.snk_stall_o(WB_ep_src.master.stall),
		.snk_ack_o  (WB_ep_src.master.ack),
		.snk_err_o  (WB_ep_src.master.err),

		.wb_cyc_i  (WB_ep.master.cyc),
		.wb_stb_i  (WB_ep.master.stb),
		.wb_we_i   (WB_ep.master.we),
		.wb_sel_i  (WB_ep.master.sel),
		.wb_adr_i  (WB_ep.master.adr[7:0]),
		.wb_dat_i  (WB_ep.master.dat_o),
		.wb_dat_o  (WB_ep.master.dat_i),
		.wb_ack_o  (WB_ep.master.ack),
		.wb_stall_o(WB_ep.master.stall));

		wrf_loopback #(
			.g_interface_mode(PIPELINED),
			.g_address_granularity(BYTE))
		WRF_LBK (
			.clk_sys_i(clk_sys),
			.rst_n_i(rst_n),
			.snk_cyc_i(wrc_src_cyc),
			.snk_stb_i(wrc_src_stb),
			.snk_we_i (1'b1),
			.snk_sel_i(wrc_src_sel),
			.snk_adr_i(wrc_src_adr),
			.snk_dat_i(wrc_src_dat),
			.snk_ack_o(wrc_src_ack),
			.snk_stall_o(wrc_src_stall),

			.src_cyc_o(wrc_snk_cyc),
			.src_stb_o(wrc_snk_stb),
			.src_we_o (),
			.src_sel_o(wrc_snk_sel),
			.src_adr_o(wrc_snk_adr),
			.src_dat_o(wrc_snk_dat),
			.src_ack_i(wrc_snk_ack),
			.src_stall_i(wrc_snk_stall),

			.wb_cyc_i(WB_lbk.master.cyc),
			.wb_stb_i(WB_lbk.master.stb),
			.wb_we_i (WB_lbk.master.we),
			.wb_sel_i(4'b1111),
			.wb_adr_i(WB_lbk.master.adr),
			.wb_dat_i(WB_lbk.master.dat_o),
			.wb_dat_o(WB_lbk.master.dat_i),
			.wb_ack_o(WB_lbk.master.ack),
			.wb_stall_o(WB_lbk.master.stall));





	/* Wire together VHD and SV types */
	//`WIRE_VHD_SV_WBM(vhd_epwbm, WB_ep);
	//`WIRE_VHD_SV_WRFSNK(WB_ep_snk, vhd_epsrc);
	//`WIRE_VHD_SV_WRFSRC(vhd_epsnk, WB_ep_src);

  assign phy_tx_disparity  = 0;
  assign phy_tx_enc_err    = 0;
  assign phy_rx_enc_err    = 0;

  //////////////////////////////////////
  //////////////////////////////////////
  // DPI import exports
  //////////////////////////////////////
	//export "DPI-C" task SV_minic_write;
	//export "DPI-C" task SV_minic_read;
	//import "DPI-C" context task C_minic_rx_frame();
	//import "DPI-C" context task C_minic_init();

	//task SV_minic_write(uint32_t addr, uint32_t val);
	//	acc.write(`BASE_MINIC + addr, val);
	//endtask

	//task SV_minic_read(uint32_t addr, output uint32_t val);
	//	uint64_t val64;
	//	acc.read(`BASE_MINIC + addr, val64);
	//	val = val64;
	//	//return val64;
	//endtask
  //////////////////////////////////////

  initial begin
    //CWishboneAccessor acc_wrc;
		CSimDrv_WR_Endpoint ep_drv;

    @(posedge rst_n);
    repeat(3) @(posedge clk_sys);

    #1us;

    acc_wrc = WB_wrc.get_accessor();
    acc_wrc.set_mode(PIPELINED);
    WB_wrc.settings.cyc_on_stall = 1;

		acc_ep = WB_ep.get_accessor();
    WB_ep.settings.cyc_on_stall = 1;
		acc_ep.set_mode(PIPELINED);
		ep_drv = new(acc_ep, 0);
		ep_drv.init(0);

    wrc_src = new(WB_wrc_src.get_accessor());
    WB_wrc_src.settings.cyc_on_stall = 1;

		ep_src = new(WB_ep_src.get_accessor());
		WB_ep_src.settings.cyc_on_stall = 1;

    #1us
    acc_wrc.write(`BASE_SYSCON + `ADDR_SYSC_RSTR, 'hdeadbee | `SYSC_RSTR_RST);
    #1us;
    acc_wrc.write(`BASE_SYSCON + `ADDR_SYSC_RSTR, 'hdeadbee );

    #1400us;
    tx_sizes = {};
    //NOW LET'S SEND SOME FRAMES
    //send_frames(wrc_src, 3000);
    send_frames(ep_src, 20);

  end

  initial begin
    EthPacket pkt;
    int i = 0, correct = 0, j;
    int drop_first = 1;
    int size_pos;
		CSimDrv_Minic minic;
		EthPacket rxp;
		int prev_size=0;
		uint64_t val64;

    WB_wrc_snk.settings.gen_random_stalls = 1;
    wrc_snk = new(WB_wrc_snk.get_accessor());
		WB_ep_snk.settings.gen_random_stalls = 0;
		ep_snk = new(WB_ep_snk.get_accessor());

		acc_lbk = WB_lbk.get_accessor();
		acc_lbk.set_mode(PIPELINED);
		WB_lbk.settings.cyc_on_stall = 1;
		#1us;
		acc_lbk.write(`ADDR_LBK_MCR, `LBK_MCR_ENA);

		#1200us;
		//minic = new('h1000, acc_wrc, `BASE_MINIC, acc_wrc, 'h4d98);
		//minic.init();
    while(1) begin
			#1us;
			ep_snk.recv(pkt);
			$display("--> recv: size=%4d", pkt.size);
			acc_lbk.read(`ADDR_LBK_RCV_CNT, val64);
			$display("rcv_cnt: %d", val64);
			acc_lbk.read(`ADDR_LBK_DRP_CNT, val64);
			$display("drp_cnt: %d", val64);
			acc_lbk.read(`ADDR_LBK_FWD_CNT, val64);
			$display("fwd_cnt: %d", val64);
			//acc_lbk.write(`ADDR_LBK_MCR, `LBK_MCR_CLR);
			//acc_lbk.write(`ADDR_LBK_MCR, 0);

			//minic.run();
			//if(minic.poll()) begin
			//	minic.recv(rxp);
			//	$display("got frame size: %d; diff: %d", rxp.size, rxp.size - prev_size);
			//	prev_size = rxp.size;
			//end
      //wrc_snk.recv(pkt);
      //size_pos = find_pkt_size(pkt, i, 5);

      //assert (pkt.size == tx_padded[i] && nopad_size(pkt) == tx_sizes[i]) begin
      //  correct = correct + 1;
      //  $display("--> recv: size=%4d, nopad=%4d", pkt.size, nopad_size(pkt));
      //end
      //else assert (pkt.error == 1'b1 || (pkt.size == tx_padded[size_pos] && nopad_size(pkt) == tx_sizes[size_pos])) begin
      //  if(size_pos == -1)
      //    $warning("(%1d) Lost frame with size: %4d", pkt.error, tx_sizes[i]);
      //  for(j=i; j<size_pos; j++) 
      //    $warning("(%1d) Lost frame with size: %4d", pkt.error, tx_sizes[j]);
      //  if(pkt.error == 1'b0) i = size_pos;
      //  correct = correct + 1;
      //  $display("--> recv: size=%4d, nopad=%4d", pkt.size, nopad_size(pkt));
      //end  
      //else begin
      //  $display("Frame dump:");
      //  for(j=0; j<pkt.size-14; j++) begin
      //    $write("0x%02X ", pkt.payload[j]);
      //  end
      //  $fatal("(%1d) Size does not match: pkt.size=%4d, nopad.size=%4d, padded=%4d,
      //  sizes=%4d", pkt.error, pkt.size, nopad_size(pkt), tx_padded[i], tx_sizes[i]); 
      //end
      //i = i+1;
    end
  end

	/////////////////// DPI
	//always @posedge(wb_read_req)
	//begin
	//	wb_read_done = 1'b0;
	//	acc.read(`BASE_MINIC + wb_read_adr, wb_read_val);
	//	wb_read_done = 1'b1;
	//end
	//////////////////////////

endmodule // main
