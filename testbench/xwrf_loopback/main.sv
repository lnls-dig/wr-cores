`timescale 1ns/1ps

   
`include "tbi_utils.sv"

`include "simdrv_defs.svh"
`include "if_wb_master.svh"
`include "if_wb_slave.svh"
`include "wb_packet_source.svh"
`include "wb_packet_sink.svh"
`include "if_wb_link.svh"
`include "functions.svh"
`include "lbk_regs.v"

module main;

  wire clk_ref;
  wire clk_sys;
  wire rst_n;

	/* WB masters */
  IWishboneMaster WB_lbk (clk_sys, rst_n);

	/* WB accessors */
  CWishboneAccessor acc_lbk;

	/* Fabrics */
  IWishboneMaster #(2,16) WB_lbk_src (clk_sys, rst_n);
  IWishboneSlave  #(2,16) WB_lbk_snk (clk_sys, rst_n);

	/* Fabrics accessors */
  WBPacketSource lbk_src;
  WBPacketSink   lbk_snk;

  tbi_clock_rst_gen
  #(
    .g_rbclk_period(8000))
  clkgen(
    .clk_ref_o(clk_ref),
    .clk_sys_o(clk_sys),
    .phy_rbclk_o(phy_rbclk),
    .rst_n_o(rst_n)
  );

	wrf_loopback #(
		.g_interface_mode(PIPELINED),
		.g_address_granularity(BYTE))
	WRF_LBK (
		.clk_sys_i(clk_sys),
		.rst_n_i(rst_n),
		.snk_cyc_i(WB_lbk_src.master.cyc),
		.snk_stb_i(WB_lbk_src.master.stb),
		.snk_we_i (WB_lbk_src.master.we),
		.snk_sel_i(WB_lbk_src.master.sel),
		.snk_adr_i(WB_lbk_src.master.adr),
		.snk_dat_i(WB_lbk_src.master.dat_o),
		.snk_ack_o(WB_lbk_src.master.ack),
		.snk_stall_o(WB_lbk_src.master.stall),

		.src_cyc_o(WB_lbk_snk.slave.cyc),
		.src_stb_o(WB_lbk_snk.slave.stb),
		.src_we_o (WB_lbk_snk.slave.we),
		.src_sel_o(WB_lbk_snk.slave.sel),
		.src_adr_o(WB_lbk_snk.slave.adr),
		.src_dat_o(WB_lbk_snk.slave.dat_i),
		.src_ack_i(WB_lbk_snk.slave.ack),
		.src_stall_i(WB_lbk_snk.slave.stall),

		.wb_cyc_i(WB_lbk.master.cyc),
		.wb_stb_i(WB_lbk.master.stb),
		.wb_we_i (WB_lbk.master.we),
		.wb_sel_i(4'b1111),
		.wb_adr_i(WB_lbk.master.adr),
		.wb_dat_i(WB_lbk.master.dat_o),
		.wb_dat_o(WB_lbk.master.dat_i),
		.wb_ack_o(WB_lbk.master.ack),
		.wb_stall_o(WB_lbk.master.stall));

  initial begin

    @(posedge rst_n);
    repeat(3) @(posedge clk_sys);

    #1us;

    acc_lbk = WB_lbk.get_accessor();
    acc_lbk.set_mode(PIPELINED);
    WB_lbk.settings.cyc_on_stall = 1;

		lbk_src = new(WB_lbk_src.get_accessor());
		WB_lbk_src.settings.cyc_on_stall = 1;

    #1us;
		acc_lbk.write(`ADDR_LBK_DMAC_H, 32'h00001122);
		#1us;
		acc_lbk.write(`ADDR_LBK_DMAC_L, 32'h33445566);
		#1us;
		acc_lbk.write(`ADDR_LBK_MCR, `LBK_MCR_ENA | `LBK_MCR_FDMAC);
		//acc_lbk.write(`ADDR_LBK_MCR, `LBK_MCR_ENA);

    #1500ns;
    tx_sizes = {};
    //NOW LET'S SEND SOME FRAMES
    send_frames(lbk_src, 1500);
  end

  initial begin
    EthPacket pkt;
		int prev_size=0;
		uint64_t val64;

    WB_lbk_snk.settings.gen_random_stalls = 1;
    lbk_snk = new(WB_lbk_snk.get_accessor());

		#5us;
    while(1) begin
			#1us;
			lbk_snk.recv(pkt);
			//if(pkt.size-prev_size!=1)
			//	$warning("--> recv: size=%4d, %4d", pkt.size, pkt.size-prev_size);
			if(pkt.dst[0]!=8'h11 || pkt.dst[1]!=8'h22 || pkt.dst[2]!=8'h33 || 
				 pkt.dst[3]!=8'h44 || pkt.dst[4]!=8'h55 || pkt.dst[5]!=8'h66)
			//if(pkt.dst[0]!=8'h16 || pkt.dst[1]!=8'h21 || pkt.dst[2]!=8'h2c || 
			//	 pkt.dst[3]!=8'h2c || pkt.dst[4]!=8'h37 || pkt.dst[5]!=8'h42)
			begin
				$write("%02X:", pkt.dst[0]);
				$write("%02X:", pkt.dst[1]);
				$write("%02X:", pkt.dst[2]);
				$write("%02X:", pkt.dst[3]);
				$write("%02X:", pkt.dst[4]);
				$write("%02X",  pkt.dst[5]);
				$warning("--> recv: size=%4d, %4d", pkt.size, pkt.size-prev_size);
			end;
			prev_size = pkt.size;
			//acc_lbk.read(`ADDR_LBK_RCV_CNT, val64);
			//$display("rcv_cnt: %d", val64);
			//acc_lbk.read(`ADDR_LBK_DRP_CNT, val64);
			//$display("drp_cnt: %d", val64);
			//acc_lbk.read(`ADDR_LBK_FWD_CNT, val64);
			//$display("fwd_cnt: %d", val64);
			//acc_lbk.write(`ADDR_LBK_MCR, `LBK_MCR_CLR);
			//acc_lbk.write(`ADDR_LBK_MCR, 0);
    end
  end

endmodule // main
