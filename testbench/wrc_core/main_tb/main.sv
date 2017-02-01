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

  /// ////////////////////// WR PTP CORE /////////////////////////////////////
  wr_core #(
    .g_simulation               (1),
    .g_interface_mode           (PIPELINED),
    .g_address_granularity      (BYTE),
    .g_tx_runt_padding          (1),
    .g_with_external_clock_input(1),
    .g_dpram_initf              ("sw/wrc.ram"),
    .g_dpram_size               (131072/4),
    .g_diag_id                  (1),
    .g_diag_ver                 (2),
    .g_diag_ro_size             (5),
    .g_diag_rw_size             (1))
  DUT (
    .clk_sys_i                  (clk_sys),
    .clk_dmtd_i                 (clk_ref),
    .clk_ref_i                  (clk_ref),
    .rst_n_i                    (rst_n),

    .pps_p_o                    (),
    .dac_hpll_load_p1_o         (),
    .dac_hpll_data_o            (),
    .dac_dpll_load_p1_o         (),
    .dac_dpll_data_o            (),

    .uart_rxd_i                 (1'b0),
    .uart_txd_o                 (),

    .scl_i                      (scl_loop),
    .scl_o                      (scl_loop),
    .sda_i                      (sda_loop),
    .sda_o                      (sda_loop),

    .btn1_i                     (1'b0),
    .btn2_i                     (1'b0),

     // connected to wrf_loopback
    .ext_snk_adr_i              (wrc_snk_adr),
    .ext_snk_dat_i              (wrc_snk_dat),
    .ext_snk_sel_i              (wrc_snk_sel),
    .ext_snk_cyc_i              (wrc_snk_cyc),
    .ext_snk_we_i               (1'b1),
    .ext_snk_stb_i              (wrc_snk_stb),
    .ext_snk_ack_o              (wrc_snk_ack),
    .ext_snk_err_o              (wrc_snk_err),
    .ext_snk_stall_o            (wrc_snk_stall),
    .ext_src_adr_o              (wrc_src_adr),
    .ext_src_dat_o              (wrc_src_dat),
    .ext_src_sel_o              (wrc_src_sel),
    .ext_src_cyc_o              (wrc_src_cyc),
    .ext_src_stb_o              (wrc_src_stb),
    .ext_src_we_o               (),
    .ext_src_ack_i              (wrc_src_ack),
    .ext_src_err_i              (wrc_src_err),
    .ext_src_stall_i            (wrc_src_stall),

    .wb_adr_i                   (WB_wrc.master.adr[31:0]),
    .wb_dat_i                   (WB_wrc.master.dat_o),
    .wb_dat_o                   (WB_wrc.master.dat_i),
    .wb_sel_i                   (4'b1111),
    .wb_we_i                    (WB_wrc.master.we),
    .wb_cyc_i                   (WB_wrc.master.cyc),
    .wb_stb_i                   (WB_wrc.master.stb),
    .wb_ack_o                   (WB_wrc.master.ack),
    .wb_stall_o                 (WB_wrc.master.stall),

    .phy_ref_clk_i              (clk_ref),
    .phy_tx_data_o              (phy_tx_data),
    .phy_tx_k_o                 (phy_tx_k),
    .phy_tx_disparity_i         (phy_tx_disparity),
    .phy_tx_enc_err_i           (phy_tx_enc_err),
    .phy_rx_rbclk_i             (clk_ref),
    .phy_rx_data_i              (phy_tx_data),
    .phy_rx_k_i                 (phy_tx_k),
    .phy_rx_enc_err_i           (phy_rx_enc_err),
    .phy_rx_bitslide_i          (phy_rx_bitslide),
    .phy_rst_o                  (phy_rst),
    .phy_loopen_o               (phy_lo)
  );

  /// ////////////////////// ENDPOINT /////////////////////////////////////
  wr_endpoint #(
    .g_interface_mode           (PIPELINED),
    .g_address_granularity      (BYTE),
    .g_simulation               (1),
    .g_pcs_16bit                (0),
    .g_rx_buffer_size           (1024),
    .g_with_rx_buffer           (1),
    .g_with_timestamper         (0),
    .g_with_dpi_classifier      (0),
    .g_with_vlans               (0),
    .g_with_rtu                 (0))
  EP (
    .clk_ref_i                  (clk_ref),
    .clk_sys_i                  (clk_sys),
    .clk_dmtd_i                 (clk_ref),
    .rst_n_i                    (rst_n),
    .pps_csync_p1_i             (1'b0),

    .phy_sfp_tx_fault_i         (1'b0),
    .phy_sfp_los_i              (1'b0),
    .phy_rdy_i                  (1'b1),
    .phy_ref_clk_i              (clk_ref),
    .phy_tx_data_o              (phy_rx_data),
    .phy_tx_k_o                 (phy_rx_k),
    .phy_tx_disparity_i         (phy_tx_disparity),
    .phy_tx_enc_err_i           (phy_tx_enc_err),
    .phy_rx_data_i              (phy_tx_data),
    .phy_rx_clk_i               (clk_ref),
    .phy_rx_k_i                 (phy_tx_k),
    .phy_rx_enc_err_i           (phy_rx_enc_err),
    .phy_rx_bitslide_i          (phy_rx_bitslide),

    // connected to simulation
    .src_dat_o                  (WB_ep_snk.slave.dat_i),
    .src_adr_o                  (WB_ep_snk.slave.adr),
    .src_sel_o                  (WB_ep_snk.slave.sel),
    .src_cyc_o                  (WB_ep_snk.slave.cyc),
    .src_stb_o                  (WB_ep_snk.slave.stb),
    .src_we_o                   (WB_ep_snk.slave.we),
    .src_stall_i                (WB_ep_snk.slave.stall),
    .src_ack_i                  (WB_ep_snk.slave.ack),
    .src_err_i                  (1'b0),

    .snk_dat_i                  (WB_ep_src.master.dat_o),
    .snk_adr_i                  (WB_ep_src.master.adr),
    .snk_sel_i                  (WB_ep_src.master.sel),
    .snk_cyc_i                  (WB_ep_src.master.cyc),
    .snk_stb_i                  (WB_ep_src.master.stb),
    .snk_we_i                   (WB_ep_src.master.we),
    .snk_stall_o                (WB_ep_src.master.stall),
    .snk_ack_o                  (WB_ep_src.master.ack),
    .snk_err_o                  (WB_ep_src.master.err),

    .wb_cyc_i                   (WB_ep.master.cyc),
    .wb_stb_i                   (WB_ep.master.stb),
    .wb_we_i                    (WB_ep.master.we),
    .wb_sel_i                   (WB_ep.master.sel),
    .wb_adr_i                   (WB_ep.master.adr[7:0]),
    .wb_dat_i                   (WB_ep.master.dat_o),
    .wb_dat_o                   (WB_ep.master.dat_i),
    .wb_ack_o                   (WB_ep.master.ack),
    .wb_stall_o                 (WB_ep.master.stall));

  /// ////////////////////// Loopback /////////////////////////////////////
  wrf_loopback #(
    .g_interface_mode           (PIPELINED),
    .g_address_granularity      (BYTE))
  WRF_LBK (
    .clk_sys_i                  (clk_sys),
    .rst_n_i                    (rst_n),
    .snk_cyc_i                  (wrc_src_cyc),
    .snk_stb_i                  (wrc_src_stb),
    .snk_we_i                   (1'b1),
    .snk_sel_i                  (wrc_src_sel),
    .snk_adr_i                  (wrc_src_adr),
    .snk_dat_i                  (wrc_src_dat),
    .snk_ack_o                  (wrc_src_ack),
    .snk_stall_o                (wrc_src_stall),

    .src_cyc_o                  (wrc_snk_cyc),
    .src_stb_o                  (wrc_snk_stb),
    .src_we_o                   (),
    .src_sel_o                  (wrc_snk_sel),
    .src_adr_o                  (wrc_snk_adr),
    .src_dat_o                  (wrc_snk_dat),
    .src_ack_i                  (wrc_snk_ack),
    .src_stall_i                (wrc_snk_stall),

    .wb_cyc_i                   (WB_lbk.master.cyc),
    .wb_stb_i                   (WB_lbk.master.stb),
    .wb_we_i                    (WB_lbk.master.we),
    .wb_sel_i                   (4'b1111),
    .wb_adr_i                   (WB_lbk.master.adr),
    .wb_dat_i                   (WB_lbk.master.dat_o),
    .wb_dat_o                   (WB_lbk.master.dat_i),
    .wb_ack_o                   (WB_lbk.master.ack),
    .wb_stall_o                 (WB_lbk.master.stall));

  /// ////////////////////////////////////////////////////////////////////////
  /// ////////////////////// Simulation  /////////////////////////////////////
  /// ////////////////////////////////////////////////////////////////////////
  assign phy_tx_disparity  = 0;
  assign phy_tx_enc_err    = 0;
  assign phy_rx_enc_err    = 0;

  initial begin

    CSimDrv_WR_Endpoint ep_drv;
    uint64_t val;

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

    end
  end

endmodule // main
