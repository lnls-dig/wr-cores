-------------------------------------------------------------------------------
-- Title      : WhiteRabbit PTP Core
-- Project    : WhiteRabbit
-------------------------------------------------------------------------------
-- File       : wr_core.vhd
-- Author     : Grzegorz Daniluk
-- Company    : Elproma
-- Created    : 2011-02-02
-- Last update: 2012-02-09
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
-- WR PTP Core is a HDL module implementing a complete gigabit Ethernet 
-- interface (MAC + PCS + PHY) with integrated PTP slave ordinary clock 
-- compatible with White Rabbit protocol. It performs subnanosecond clock 
-- synchronization via WR protocol and also acts as an Ethernet "gateway", 
-- providing access to TX/RX interfaces of the built-in WR MAC.
--
-- Starting from version 2.0 all modules are interconnected with pipelined
-- wishbone interface (using wb crossbar and bus fanout). Separate pipelined
-- wishbone bus is used for passing packets between Endpoint, Mini-NIC
-- and External MAC interface.
-------------------------------------------------------------------------------
-- Copyright (c) 2011 Grzegorz Daniluk
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2011-02-02  1.0      greg.d          Created
-- 2011-10-25  2.0      greg.d          Redesigned and wishbonized
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library work;
use work.wrcore_pkg.all;
use work.genram_pkg.all;
use work.wishbone_pkg.all;
use work.endpoint_pkg.all;
use work.wr_fabric_pkg.all;
use work.sysc_wbgen2_pkg.all;


entity xwr_core is
  generic(
    --if set to 1, then blocks in PCS use smaller calibration counter to speed 
    --up simulation
    g_simulation          : integer                        := 0;
    g_phys_uart           : boolean                        := true;
    g_virtual_uart        : boolean                        := false;
    g_ep_rxbuf_size       : integer                        := 1024;
    g_dpram_initf         : string                         := "";
    g_dpram_size          : integer                        := 16384;  --in 32-bit words
    g_interface_mode      : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity : t_wishbone_address_granularity := WORD
    );
  port(
    clk_sys_i : in std_logic;

    -- DDMTD offset lcock (125.x MHz)
    clk_dmtd_i : in std_logic;

    -- Timing reference (125 MHz)
    clk_ref_i : in std_logic;

    -- Aux clock (i.e. the FMC clock)
    clk_aux_i : in std_logic;

    rst_n_i : in std_logic;

    -----------------------------------------
    --Timing system
    -----------------------------------------
    dac_hpll_load_p1_o : out std_logic;
    dac_hpll_data_o    : out std_logic_vector(15 downto 0);

    dac_dpll_load_p1_o : out std_logic;
    dac_dpll_data_o    : out std_logic_vector(15 downto 0);

    -- PHY I/f
    phy_ref_clk_i : in std_logic;

    phy_tx_data_o      : out std_logic_vector(7 downto 0);
    phy_tx_k_o         : out std_logic;
    phy_tx_disparity_i : in  std_logic;
    phy_tx_enc_err_i   : in  std_logic;

    phy_rx_data_i     : in std_logic_vector(7 downto 0);
    phy_rx_rbclk_i    : in std_logic;
    phy_rx_k_i        : in std_logic;
    phy_rx_enc_err_i  : in std_logic;
    phy_rx_bitslide_i : in std_logic_vector(3 downto 0);

    phy_rst_o    : out std_logic;
    phy_loopen_o : out std_logic;

    -----------------------------------------
    --GPIO
    -----------------------------------------
    led_red_o   : out std_logic;
    led_green_o : out std_logic;
    scl_o       : out std_logic;
    scl_i       : in  std_logic;
    sda_o       : out std_logic;
    sda_i       : in  std_logic;
    btn1_i      : in  std_logic;
    btn2_i      : in  std_logic;

    -----------------------------------------
    --UART
    -----------------------------------------
    uart_rxd_i : in  std_logic;
    uart_txd_o : out std_logic;

    -----------------------------------------
    -- 1-wire
    -----------------------------------------
    owr_en_o : out std_logic;
    owr_i    : in  std_logic;

    -----------------------------------------
    --External WB interface
    -----------------------------------------
    slave_i : in  t_wishbone_slave_in;
    slave_o : out t_wishbone_slave_out;

    -----------------------------------------
    -- External Fabric I/F
    -----------------------------------------
    wrf_src_o : out t_wrf_source_out;
    wrf_src_i : in  t_wrf_source_in := c_dummy_src_in;
    wrf_snk_o : out t_wrf_sink_out;
    wrf_snk_i : in  t_wrf_sink_in   := c_dummy_snk_in;

    -----------------------------------------
    -- External Tx Timestamping I/F
    -----------------------------------------
    timestamps_o     : out t_txtsu_timestamp;
    timestamps_ack_i : in  std_logic := '1';

    -----------------------------------------
    -- Timecode/Servo Control
    -----------------------------------------

    -- DAC Control
    tm_dac_value_o       : out std_logic_vector(23 downto 0);
    tm_dac_wr_o          : out std_logic;
    -- Aux clock lock enable
    tm_clk_aux_lock_en_i : in  std_logic;
    -- Aux clock locked flag
    tm_clk_aux_locked_o  : out std_logic;
    -- Timecode output
    tm_time_valid_o      : out std_logic;
    tm_utc_o             : out std_logic_vector(39 downto 0);
    tm_cycles_o          : out std_logic_vector(27 downto 0);
    -- 1PPS output
    pps_p_o              : out std_logic;

    dio_o       : out std_logic_vector(3 downto 0);
    rst_aux_n_o : out std_logic
    );
end xwr_core;

architecture struct of xwr_core is

  component wr_core is
    generic(
      g_simulation          : integer                        := 0;
      g_phys_uart           : boolean                        := true;
      g_virtual_uart        : boolean                        := false;
      g_rx_buffer_size  : integer                        := 12;
      g_dpram_initf         : string                         := "";
      g_dpram_size          : integer                        := 16384;  --in 32-bit words
      g_interface_mode      : t_wishbone_interface_mode      := CLASSIC;
      g_address_granularity : t_wishbone_address_granularity := WORD);
    port(
      clk_sys_i  : in std_logic;
      clk_dmtd_i : in std_logic;
      clk_ref_i  : in std_logic;
      clk_aux_i  : in std_logic;
      rst_n_i    : in std_logic;

      dac_hpll_load_p1_o : out std_logic;
      dac_hpll_data_o    : out std_logic_vector(15 downto 0);
      dac_dpll_load_p1_o : out std_logic;
      dac_dpll_data_o    : out std_logic_vector(15 downto 0);

      phy_ref_clk_i      : in  std_logic;
      phy_tx_data_o      : out std_logic_vector(7 downto 0);
      phy_tx_k_o         : out std_logic;
      phy_tx_disparity_i : in  std_logic;
      phy_tx_enc_err_i   : in  std_logic;
      phy_rx_data_i      : in  std_logic_vector(7 downto 0);
      phy_rx_rbclk_i     : in  std_logic;
      phy_rx_k_i         : in  std_logic;
      phy_rx_enc_err_i   : in  std_logic;
      phy_rx_bitslide_i  : in  std_logic_vector(3 downto 0);
      phy_rst_o          : out std_logic;
      phy_loopen_o       : out std_logic;

      led_red_o   : out std_logic;
      led_green_o : out std_logic;
      scl_o       : out std_logic;
      scl_i       : in  std_logic;
      sda_o       : out std_logic;
      sda_i       : in  std_logic;
      btn1_i      : in  std_logic;
      btn2_i      : in  std_logic;

      uart_rxd_i : in  std_logic;
      uart_txd_o : out std_logic;

      owr_en_o : out std_logic;
      owr_i    : in  std_logic;

      wb_adr_i   : in  std_logic_vector(c_wishbone_address_width-1 downto 0);
      wb_dat_i   : in  std_logic_vector(c_wishbone_data_width-1 downto 0);
      wb_dat_o   : out std_logic_vector(c_wishbone_data_width-1 downto 0);
      wb_sel_i   : in  std_logic_vector(c_wishbone_address_width/8-1 downto 0);
      wb_we_i    : in  std_logic;
      wb_cyc_i   : in  std_logic;
      wb_stb_i   : in  std_logic;
      wb_ack_o   : out std_logic;
      wb_stall_o : out std_logic;

      ext_snk_adr_i   : in  std_logic_vector(1 downto 0)  := "00";
      ext_snk_dat_i   : in  std_logic_vector(15 downto 0) := x"0000";
      ext_snk_sel_i   : in  std_logic_vector(1 downto 0)  := "00";
      ext_snk_cyc_i   : in  std_logic                     := '0';
      ext_snk_we_i    : in  std_logic                     := '0';
      ext_snk_stb_i   : in  std_logic                     := '0';
      ext_snk_ack_o   : out std_logic;
      ext_snk_err_o   : out std_logic;
      ext_snk_stall_o : out std_logic;

      ext_src_adr_o   : out std_logic_vector(1 downto 0);
      ext_src_dat_o   : out std_logic_vector(15 downto 0);
      ext_src_sel_o   : out std_logic_vector(1 downto 0);
      ext_src_cyc_o   : out std_logic;
      ext_src_stb_o   : out std_logic;
      ext_src_we_o    : out std_logic;
      ext_src_ack_i   : in  std_logic := '1';
      ext_src_err_i   : in  std_logic := '0';
      ext_src_stall_i : in  std_logic := '0';

      txtsu_port_id_o  : out std_logic_vector(4 downto 0);
      txtsu_frame_id_o : out std_logic_vector(15 downto 0);
      txtsu_tsval_o    : out std_logic_vector(31 downto 0);
      txtsu_valid_o    : out std_logic;
      txtsu_ack_i      : in  std_logic;

      tm_dac_value_o       : out std_logic_vector(23 downto 0);
      tm_dac_wr_o          : out std_logic;
      tm_clk_aux_lock_en_i : in  std_logic;
      tm_clk_aux_locked_o  : out std_logic;
      tm_time_valid_o      : out std_logic;
      tm_utc_o             : out std_logic_vector(39 downto 0);
      tm_cycles_o          : out std_logic_vector(27 downto 0);
      pps_p_o              : out std_logic;

      dio_o       : out std_logic_vector(3 downto 0);
      rst_aux_n_o : out std_logic
    );
  end component;

begin

  WRPC : wr_core
    generic map(
      g_simulation          => g_simulation,
      g_phys_uart           => g_phys_uart,
      g_virtual_uart        => g_virtual_uart,
      g_rx_buffer_size      => g_ep_rxbuf_size,
      g_dpram_initf         => g_dpram_initf,
      g_dpram_size          => g_dpram_size,
      g_interface_mode      => g_interface_mode,
      g_address_granularity => g_address_granularity)
    port map(
      clk_sys_i  => clk_sys_i,
      clk_dmtd_i => clk_dmtd_i,
      clk_ref_i  => clk_ref_i,
      clk_aux_i  => clk_aux_i,
      rst_n_i    => rst_n_i,

      dac_hpll_load_p1_o => dac_hpll_load_p1_o,
      dac_hpll_data_o    => dac_hpll_data_o,
      dac_dpll_load_p1_o => dac_dpll_load_p1_o,
      dac_dpll_data_o    => dac_dpll_data_o,

      phy_ref_clk_i      => phy_ref_clk_i,
      phy_tx_data_o      => phy_tx_data_o,
      phy_tx_k_o         => phy_tx_k_o,
      phy_tx_disparity_i => phy_tx_disparity_i,
      phy_tx_enc_err_i   => phy_tx_enc_err_i,
      phy_rx_data_i      => phy_rx_data_i,
      phy_rx_rbclk_i     => phy_rx_rbclk_i,
      phy_rx_k_i         => phy_rx_k_i,
      phy_rx_enc_err_i   => phy_rx_enc_err_i,
      phy_rx_bitslide_i  => phy_rx_bitslide_i,
      phy_rst_o          => phy_rst_o,
      phy_loopen_o       => phy_loopen_o,

      led_red_o   => led_red_o,
      led_green_o => led_green_o,
      scl_o       => scl_o,
      scl_i       => scl_i,
      sda_o       => sda_o,
      sda_i       => sda_i,
      btn1_i      => btn1_i,
      btn2_i      => btn2_i,
      uart_rxd_i  => uart_rxd_i,
      uart_txd_o  => uart_txd_o,

      owr_en_o => owr_en_o,
      owr_i    => owr_i,

      wb_adr_i   => slave_i.adr,
      wb_dat_i   => slave_i.dat,
      wb_dat_o   => slave_o.dat,
      wb_sel_i   => slave_i.sel,
      wb_we_i    => slave_i.we,
      wb_cyc_i   => slave_i.cyc,
      wb_stb_i   => slave_i.stb,
      wb_ack_o   => slave_o.ack,
      wb_stall_o => slave_o.stall,

      ext_snk_adr_i   => wrf_snk_i.adr,
      ext_snk_dat_i   => wrf_snk_i.dat,
      ext_snk_sel_i   => wrf_snk_i.sel,
      ext_snk_cyc_i   => wrf_snk_i.cyc,
      ext_snk_we_i    => wrf_snk_i.we,
      ext_snk_stb_i   => wrf_snk_i.stb,
      ext_snk_ack_o   => wrf_snk_o.ack,
      ext_snk_err_o   => wrf_snk_o.err,
      ext_snk_stall_o => wrf_snk_o.stall,

      ext_src_adr_o   => wrf_src_o.adr,
      ext_src_dat_o   => wrf_src_o.dat,
      ext_src_sel_o   => wrf_src_o.sel,
      ext_src_cyc_o   => wrf_src_o.cyc,
      ext_src_stb_o   => wrf_src_o.stb,
      ext_src_we_o    => wrf_src_o.we,
      ext_src_ack_i   => wrf_src_i.ack,
      ext_src_err_i   => wrf_src_i.err,
      ext_src_stall_i => wrf_src_i.stall,

      txtsu_port_id_o  => timestamps_o.port_id(4 downto 0),
      txtsu_frame_id_o => timestamps_o.frame_id,
      txtsu_tsval_o    => timestamps_o.tsval,
      txtsu_valid_o    => timestamps_o.valid,
      txtsu_ack_i      => timestamps_ack_i,

      tm_dac_value_o       => tm_dac_value_o,
      tm_dac_wr_o          => tm_dac_wr_o,
      tm_clk_aux_lock_en_i => tm_clk_aux_lock_en_i,
      tm_clk_aux_locked_o  => tm_clk_aux_locked_o,
      tm_time_valid_o      => tm_time_valid_o,
      tm_utc_o             => tm_utc_o,
      tm_cycles_o          => tm_cycles_o,
      pps_p_o              => pps_p_o,

      dio_o       => dio_o,
      rst_aux_n_o => rst_aux_n_o
    );

    timestamps_o.port_id(5) <= '0';

end struct;