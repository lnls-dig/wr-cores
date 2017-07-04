-------------------------------------------------------------------------------
-- Title      : WRPC Wrapper for VFC-HD
-- Project    : WR PTP Core
-- URL        : http://www.ohwr.org/projects/wr-cores/wiki/Wrpc_core
-------------------------------------------------------------------------------
-- File       : xwrc_board_vfchd.vhd
-- Author(s)  : Dimitrios Lampridis  <dimitrios.lampridis@cern.ch>
-- Company    : CERN (BE-CO-HT)
-- Created    : 2016-07-26
-- Last update: 2017-07-04
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Top-level wrapper for WR PTP core including all the modules
-- needed to operate the core on the VFC-HD board.
-- http://www.ohwr.org/projects/vfc-hd/
-------------------------------------------------------------------------------
-- Copyright (c) 2016-2017 CERN
-------------------------------------------------------------------------------
-- GNU LESSER GENERAL PUBLIC LICENSE
--
-- This source file is free software; you can redistribute it   
-- and/or modify it under the terms of the GNU Lesser General   
-- Public License as published by the Free Software Foundation; 
-- either version 2.1 of the License, or (at your option) any   
-- later version.                                               
--
-- This source is distributed in the hope that it will be       
-- useful, but WITHOUT ANY WARRANTY; without even the implied   
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      
-- PURPOSE.  See the GNU Lesser General Public License for more 
-- details.                                                     
--
-- You should have received a copy of the GNU Lesser General    
-- Public License along with this source; if not, download it   
-- from http://www.gnu.org/licenses/lgpl-2.1.html
-- 
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library work;
use work.gencores_pkg.all;
use work.wrcore_pkg.all;
use work.wishbone_pkg.all;
use work.etherbone_pkg.all;
use work.wr_fabric_pkg.all;
use work.endpoint_pkg.all;
use work.streamers_pkg.all;
use work.wr_altera_pkg.all;
use work.wr_board_pkg.all;
use work.wr_vfchd_pkg.all;

entity xwrc_board_vfchd is
  generic(
    -- set to 1 to speed up some initialization processes during simulation
    g_simulation                : integer              := 0;
    -- Select whether to include external ref clock input
    g_with_external_clock_input : boolean              := TRUE;
    -- Number of aux clocks syntonized by WRPC to WR timebase
    g_aux_clks                  : integer              := 0;
    -- set to TRUE to use 16bit PCS (instead of default 8bit PCS)
    g_pcs_16bit                 : boolean              := FALSE;
    -- plain     = expose WRC fabric interface
    -- streamers = attach WRC streamers to fabric interface
    -- etherbone = attach Etherbone slave to fabric interface
    g_fabric_iface              : t_board_fabric_iface := plain;
    -- parameters configuration when g_fabric_iface = "streamers" (otherwise ignored)
    g_streamers_op_mode         : t_streamers_op_mode  := TX_AND_RX;
    g_tx_streamer_params        : t_tx_streamer_params := c_tx_streamer_params_defaut;
    g_rx_streamer_params        : t_rx_streamer_params := c_rx_streamer_params_defaut;
    -- memory initialisation file for embedded CPU
    g_dpram_initf               : string               := "default_altera";
    -- identification (id and ver) of the layout of words in the generic diag interface
    g_diag_id                   : integer              := 0;
    g_diag_ver                  : integer              := 0;
    -- size the generic diag interface
    g_diag_ro_size              : integer              := 0;
    g_diag_rw_size              : integer              := 0
    );
  port (
    ---------------------------------------------------------------------------
    -- Clocks/resets
    ---------------------------------------------------------------------------
    -- Clock inputs from the board
    clk_board_125m_i : in  std_logic;
    clk_board_20m_i  : in  std_logic;
    -- Aux clocks, which can be disciplined by the WR Core
    clk_aux_i        : in  std_logic_vector(g_aux_clks-1 downto 0) := (others => '0');
    -- 10MHz ext ref clock input (g_with_external_clock_input = TRUE)
    clk_10m_ext_i    : in  std_logic                               := '0';
    -- External PPS input (g_with_external_clock_input = TRUE)
    pps_ext_i        : in  std_logic                               := '0';
    -- Reset input (active low, can be async)
    areset_n_i       : in  std_logic;
    -- Optional reset input active low with rising edge detection. Does not
    -- reset PLLs.
    areset_edge_n_i     : in  std_logic := '1';
    -- 62.5MHz sys clock output
    clk_sys_62m5_o   : out std_logic;
    -- 125MHz ref clock output
    clk_ref_125m_o   : out std_logic;
    -- active low reset outputs, synchronous to 62m5 and 125m clocks
    rst_sys_62m5_n_o : out std_logic;
    rst_ref_125m_n_o : out std_logic;

    ---------------------------------------------------------------------------
    -- SPI interfaces to DACs
    ---------------------------------------------------------------------------
    dac_ref_sync_n_o  : out std_logic;
    dac_dmtd_sync_n_o : out std_logic;
    dac_din_o         : out std_logic;
    dac_sclk_o        : out std_logic;

    ---------------------------------------------------------------------------
    -- SFP I/O for transceiver and SFP management info from VFC-HD
    ---------------------------------------------------------------------------
    sfp_tx_o         : out std_logic;
    sfp_rx_i         : in  std_logic;
    -- HIGH if both of the following are true:
    -- 1. SFP is detected (plugged in)
    -- 2. The part number has been successfully read after the SFP detection
    sfp_det_valid_i  : in  std_logic := '0';
    -- 16 byte vendor Part Number (PN)
    -- (ASCII encoded, first character byte in bits 127 downto 120)
    sfp_data_i       : in  std_logic_vector (127 downto 0);
    sfp_tx_fault_i   : in  std_logic := '0';
    sfp_tx_disable_o : out std_logic;
    sfp_los_i        : in  std_logic := '0';

    ---------------------------------------------------------------------------
    -- I2C EEPROM
    ---------------------------------------------------------------------------
    eeprom_sda_i : in  std_logic;
    eeprom_sda_o : out std_logic;
    eeprom_scl_i : in  std_logic;
    eeprom_scl_o : out std_logic;

    ---------------------------------------------------------------------------
    -- Onewire interface
    ---------------------------------------------------------------------------
    onewire_i     : in  std_logic;
    onewire_oen_o : out std_logic;

    ---------------------------------------------------------------------------
    -- External WB interface
    ---------------------------------------------------------------------------
    wb_slave_o : out t_wishbone_slave_out;
    wb_slave_i : in  t_wishbone_slave_in := cc_dummy_slave_in;

    aux_master_o : out t_wishbone_master_out;
    aux_master_i : in  t_wishbone_master_in := cc_dummy_master_in;

    ---------------------------------------------------------------------------
    -- WR fabric interface (when g_fabric_iface = "plain")
    ---------------------------------------------------------------------------
    wrf_src_o : out t_wrf_source_out;
    wrf_src_i : in  t_wrf_source_in := c_dummy_src_in;
    wrf_snk_o : out t_wrf_sink_out;
    wrf_snk_i : in  t_wrf_sink_in   := c_dummy_snk_in;

    ---------------------------------------------------------------------------
    -- WR streamers (when g_fabric_iface = "streamers")
    ---------------------------------------------------------------------------
    wrs_tx_data_i  : in  std_logic_vector(g_tx_streamer_params.data_width-1 downto 0) := (others => '0');
    wrs_tx_valid_i : in  std_logic                                        := '0';
    wrs_tx_dreq_o  : out std_logic;
    wrs_tx_last_i  : in  std_logic                                        := '1';
    wrs_tx_flush_i : in  std_logic                                        := '0';
    wrs_tx_cfg_i   : in  t_tx_streamer_cfg                                := c_tx_streamer_cfg_default;
    wrs_rx_first_o : out std_logic;
    wrs_rx_last_o  : out std_logic;
    wrs_rx_data_o  : out std_logic_vector(g_rx_streamer_params.data_width-1 downto 0);
    wrs_rx_valid_o : out std_logic;
    wrs_rx_dreq_i  : in  std_logic                                        := '0';
    wrs_rx_cfg_i   : in t_rx_streamer_cfg                                 := c_rx_streamer_cfg_default;
    ---------------------------------------------------------------------------
    -- Etherbone WB master interface (when g_fabric_iface = "etherbone")
    ---------------------------------------------------------------------------
    wb_eth_master_o : out t_wishbone_master_out;
    wb_eth_master_i : in  t_wishbone_master_in := cc_dummy_master_in;

    ---------------------------------------------------------------------------
    -- Generic diagnostics interface (access from WRPC via SNMP or uart console
    ---------------------------------------------------------------------------
    aux_diag_i : in  t_generic_word_array(g_diag_ro_size-1 downto 0) := (others => (others => '0'));
    aux_diag_o : out t_generic_word_array(g_diag_rw_size-1 downto 0);

    ---------------------------------------------------------------------------
    -- Aux clocks control
    ---------------------------------------------------------------------------
    tm_dac_value_o       : out std_logic_vector(23 downto 0);
    tm_dac_wr_o          : out std_logic_vector(g_aux_clks-1 downto 0);
    tm_clk_aux_lock_en_i : in  std_logic_vector(g_aux_clks-1 downto 0) := (others => '0');
    tm_clk_aux_locked_o  : out std_logic_vector(g_aux_clks-1 downto 0);

    ---------------------------------------------------------------------------
    -- External Tx Timestamping I/F
    ---------------------------------------------------------------------------
    timestamps_o     : out t_txtsu_timestamp;
    timestamps_ack_i : in  std_logic := '1';

    ---------------------------------------------------------------------------
    -- Pause Frame Control
    ---------------------------------------------------------------------------
    fc_tx_pause_req_i   : in  std_logic                     := '0';
    fc_tx_pause_delay_i : in  std_logic_vector(15 downto 0) := x"0000";
    fc_tx_pause_ready_o : out std_logic;

    ---------------------------------------------------------------------------
    -- Timecode I/F
    ---------------------------------------------------------------------------
    tm_link_up_o    : out std_logic;
    tm_time_valid_o : out std_logic;
    tm_tai_o        : out std_logic_vector(39 downto 0);
    tm_cycles_o     : out std_logic_vector(27 downto 0);

    ---------------------------------------------------------------------------
    -- Buttons, LEDs and PPS output
    ---------------------------------------------------------------------------
    led_act_o  : out std_logic;
    led_link_o : out std_logic;
    btn1_i     : in  std_logic := '1';
    btn2_i     : in  std_logic := '1';
    -- 1PPS output
    pps_p_o    : out std_logic;
    pps_led_o  : out std_logic;
    -- Link ok indication
    link_ok_o  : out std_logic
    );

end entity xwrc_board_vfchd;


architecture struct of xwrc_board_vfchd is

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------

  -- PLLs
  signal clk_pll_62m5 : std_logic;
  signal clk_pll_125m : std_logic;
  signal clk_pll_dmtd : std_logic;
  signal pll_locked   : std_logic;
  signal clk_10m_ext  : std_logic;

  -- Reset logic
  signal areset_edge_ppulse : std_logic;
  signal rst_62m5_n         : std_logic;
  signal rstlogic_arst_n    : std_logic;
  signal rstlogic_clk_in    : std_logic_vector(1 downto 0);
  signal rstlogic_rst_out   : std_logic_vector(1 downto 0);

  -- PLL DAC ARB
  signal dac_sync_n       : std_logic_vector(1 downto 0);
  signal dac_hpll_load_p1 : std_logic;
  signal dac_hpll_data    : std_logic_vector(15 downto 0);
  signal dac_dpll_load_p1 : std_logic;
  signal dac_dpll_data    : std_logic_vector(15 downto 0);

  -- OneWire
  signal onewire_in : std_logic_vector(1 downto 0);
  signal onewire_en : std_logic_vector(1 downto 0);

  -- PHY
  signal phy8_to_wrc    : t_phy_8bits_to_wrc;
  signal phy8_from_wrc  : t_phy_8bits_from_wrc;
  signal phy16_to_wrc   : t_phy_16bits_to_wrc;
  signal phy16_from_wrc : t_phy_16bits_from_wrc;

  -- SFP I2C adapter
  signal sfp_i2c_scl_in : std_logic;
  signal sfp_i2c_sda_in : std_logic;
  signal sfp_i2c_sda_en : std_logic;

  -- External reference
  signal ext_ref_mul        : std_logic;
  signal ext_ref_mul_locked : std_logic;
  signal ext_ref_rst        : std_logic;


begin  -- architecture struct

  -----------------------------------------------------------------------------
  -- Platform-dependent part (PHY, PLLs, etc)
  -----------------------------------------------------------------------------

  cmp_xwrc_platform : xwrc_platform_altera
    generic map (
      g_fpga_family               => "arria5",
      g_with_external_clock_input => g_with_external_clock_input,
      g_use_default_plls          => TRUE,
      g_pcs_16bit                 => g_pcs_16bit)
    port map (
      areset_n_i           => areset_n_i,
      clk_10m_ext_i        => clk_10m_ext_i,
      clk_20m_vcxo_i       => clk_board_20m_i,
      clk_125m_pllref_i    => clk_board_125m_i,
      sfp_tx_o             => sfp_tx_o,
      sfp_rx_i             => sfp_rx_i,
      sfp_tx_fault_i       => sfp_tx_fault_i,
      sfp_los_i            => sfp_los_i,
      sfp_tx_disable_o     => sfp_tx_disable_o,
      clk_62m5_sys_o       => clk_pll_62m5,
      clk_125m_ref_o       => clk_pll_125m,
      clk_62m5_dmtd_o      => clk_pll_dmtd,
      pll_locked_o         => pll_locked,
      clk_10m_ext_o        => clk_10m_ext,
      phy8_o               => phy8_to_wrc,
      phy8_i               => phy8_from_wrc,
      phy16_o              => phy16_to_wrc,
      phy16_i              => phy16_from_wrc,
      ext_ref_mul_o        => ext_ref_mul,
      ext_ref_mul_locked_o => ext_ref_mul_locked,
      ext_ref_rst_i        => ext_ref_rst);

  clk_sys_62m5_o <= clk_pll_62m5;
  clk_ref_125m_o <= clk_pll_125m;

  -----------------------------------------------------------------------------
  -- Reset logic
  -----------------------------------------------------------------------------
  -- Detect when areset_edge_n_i goes high (end of reset) and use this edge to
  -- generate rstlogic_arst_n. This is needed to connect optional reset like PCIe
  -- reset. When baord runs standalone, we need to ignore PCIe reset being
  -- constantly low.
  cmp_arst_edge: gc_sync_ffs
    generic map (
      g_sync_edge => "positive")
    port map (
      clk_i    => clk_pll_62m5,
      rst_n_i  => '1',
      data_i   => areset_edge_n_i,
      ppulse_o => areset_edge_ppulse);

  -- logic AND of all async reset sources (active low)
  rstlogic_arst_n <= pll_locked and areset_n_i and (not areset_edge_ppulse);

  -- concatenation of all clocks required to have synced resets
  rstlogic_clk_in(0) <= clk_pll_62m5;
  rstlogic_clk_in(1) <= clk_pll_125m;

  cmp_rstlogic_reset : gc_reset
    generic map (
      g_clocks    => 2,                           -- 62.5MHz, 125MHz
      g_logdelay  => 4,                           -- 16 clock cycles
      g_syncdepth => 3)                           -- length of sync chains
    port map (
      free_clk_i => clk_board_125m_i,
      locked_i   => rstlogic_arst_n,
      clks_i     => rstlogic_clk_in,
      rstn_o     => rstlogic_rst_out);

  -- distribution of resets (already synchronized to their clock domains)
  rst_62m5_n <= rstlogic_rst_out(0);

  rst_sys_62m5_n_o <= rst_62m5_n;
  rst_ref_125m_n_o <= rstlogic_rst_out(1);

  -----------------------------------------------------------------------------
  -- SPI DAC (2-channel)
  -----------------------------------------------------------------------------

  cmp_spi_dac : spec_serial_dac_arb
    generic map (
      g_invert_sclk    => FALSE,
      g_num_extra_bits => 8)
    port map (
      clk_i       => clk_pll_62m5,
      rst_n_i     => rst_62m5_n,
      val1_i      => dac_dpll_data,
      load1_i     => dac_dpll_load_p1,
      val2_i      => dac_hpll_data,
      load2_i     => dac_hpll_load_p1,
      dac_clr_n_o => open,
      dac_cs_n_o  => dac_sync_n,
      dac_sclk_o  => dac_sclk_o,
      dac_din_o   => dac_din_o);

  dac_ref_sync_n_o  <= dac_sync_n(0);
  dac_dmtd_sync_n_o <= dac_sync_n(1);

  -----------------------------------------------------------------------------
  -- OneWire
  -----------------------------------------------------------------------------

  onewire_oen_o <= onewire_en(0);
  onewire_in(0) <= onewire_i;
  onewire_in(1) <= '1';

  -----------------------------------------------------------------------------
  -- SFP I2C adapter for VFC-HD
  -----------------------------------------------------------------------------
  cmp_sfp_i2c_adapter : sfp_i2c_adapter
    port map (
      clk_i           => clk_pll_62m5,
      rst_n_i         => rst_62m5_n,
      scl_i           => sfp_i2c_scl_in,
      sda_i           => sfp_i2c_sda_in,
      sda_en_o        => sfp_i2c_sda_en,
      sfp_det_valid_i => sfp_det_valid_i,
      sfp_data_i      => sfp_data_i);

  -----------------------------------------------------------------------------
  -- The WR PTP core with optional fabric interface attached
  -----------------------------------------------------------------------------

  cmp_board_common : xwrc_board_common
    generic map (
      g_simulation                => g_simulation,
      g_with_external_clock_input => g_with_external_clock_input,
      g_board_name                => "VFC ",
      -- temporary, without it vuart receives but is not able to transmit
      g_phys_uart                 => TRUE,
      g_virtual_uart              => TRUE,
      g_aux_clks                  => g_aux_clks,
      g_ep_rxbuf_size             => 1024,
      g_tx_runt_padding           => TRUE,
      g_dpram_initf               => g_dpram_initf,
      g_dpram_size                => 131072/4,
      g_interface_mode            => PIPELINED,
      g_address_granularity       => BYTE,
      g_aux_sdb                   => c_wrc_periph3_sdb,
      g_softpll_enable_debugger   => FALSE,
      g_vuart_fifo_size           => 1024,
      g_pcs_16bit                 => g_pcs_16bit,
      g_diag_id                   => g_diag_id,
      g_diag_ver                  => g_diag_ver,
      g_diag_ro_size              => g_diag_ro_size,
      g_diag_rw_size              => g_diag_rw_size,
      g_streamers_op_mode         => g_streamers_op_mode,
      g_tx_streamer_params        => g_tx_streamer_params,
      g_rx_streamer_params        => g_rx_streamer_params,
      g_fabric_iface              => g_fabric_iface
      )
    port map (
      clk_sys_i            => clk_pll_62m5,
      clk_dmtd_i           => clk_pll_dmtd,
      clk_ref_i            => clk_pll_125m,
      clk_aux_i            => clk_aux_i,
      clk_10m_ext_i        => clk_10m_ext,
      clk_ext_mul_i        => ext_ref_mul,
      clk_ext_mul_locked_i => ext_ref_mul_locked,
      clk_ext_stopped_i    => '0',
      clk_ext_rst_o        => ext_ref_rst,
      pps_ext_i            => pps_ext_i,
      rst_n_i              => rst_62m5_n,
      dac_hpll_load_p1_o   => dac_hpll_load_p1,
      dac_hpll_data_o      => dac_hpll_data,
      dac_dpll_load_p1_o   => dac_dpll_load_p1,
      dac_dpll_data_o      => dac_dpll_data,
      phy8_o               => phy8_from_wrc,
      phy8_i               => phy8_to_wrc,
      phy16_o              => phy16_from_wrc,
      phy16_i              => phy16_to_wrc,
      scl_o                => eeprom_scl_o,
      scl_i                => eeprom_scl_i,
      sda_o                => eeprom_sda_o,
      sda_i                => eeprom_sda_i,
      sfp_scl_o            => sfp_i2c_scl_in,
      sfp_scl_i            => '1',
      sfp_sda_o            => sfp_i2c_sda_in,
      sfp_sda_i            => not sfp_i2c_sda_en,
      sfp_det_i            => not sfp_det_valid_i,  -- WRPC-SW expects this active low
      spi_sclk_o           => open,
      spi_ncs_o            => open,
      spi_mosi_o           => open,
      spi_miso_i           => '0',
      uart_rxd_i           => '0',
      uart_txd_o           => open,
      owr_pwren_o          => open,
      owr_en_o             => onewire_en,
      owr_i                => onewire_in,
      wb_slave_i           => wb_slave_i,
      wb_slave_o           => wb_slave_o,
      aux_master_o         => aux_master_o,
      aux_master_i         => aux_master_i,
      wrf_src_o            => wrf_src_o,
      wrf_src_i            => wrf_src_i,
      wrf_snk_o            => wrf_snk_o,
      wrf_snk_i            => wrf_snk_i,
      wrs_tx_data_i        => wrs_tx_data_i,
      wrs_tx_valid_i       => wrs_tx_valid_i,
      wrs_tx_dreq_o        => wrs_tx_dreq_o,
      wrs_tx_last_i        => wrs_tx_last_i,
      wrs_tx_flush_i       => wrs_tx_flush_i,
      wrs_tx_cfg_i         => wrs_tx_cfg_i,
      wrs_rx_first_o       => wrs_rx_first_o,
      wrs_rx_last_o        => wrs_rx_last_o,
      wrs_rx_data_o        => wrs_rx_data_o,
      wrs_rx_valid_o       => wrs_rx_valid_o,
      wrs_rx_dreq_i        => wrs_rx_dreq_i,
      wrs_rx_cfg_i         => wrs_rx_cfg_i,
      wb_eth_master_o      => wb_eth_master_o,
      wb_eth_master_i      => wb_eth_master_i,
      aux_diag_i           => aux_diag_i,
      aux_diag_o           => aux_diag_o,
      tm_dac_value_o       => tm_dac_value_o,
      tm_dac_wr_o          => tm_dac_wr_o,
      tm_clk_aux_lock_en_i => tm_clk_aux_lock_en_i,
      tm_clk_aux_locked_o  => tm_clk_aux_locked_o,
      timestamps_o         => timestamps_o,
      timestamps_ack_i     => timestamps_ack_i,
      fc_tx_pause_req_i    => fc_tx_pause_req_i,
      fc_tx_pause_delay_i  => fc_tx_pause_delay_i,
      fc_tx_pause_ready_o  => fc_tx_pause_ready_o,
      tm_link_up_o         => tm_link_up_o,
      tm_time_valid_o      => tm_time_valid_o,
      tm_tai_o             => tm_tai_o,
      tm_cycles_o          => tm_cycles_o,
      led_act_o            => led_act_o,
      led_link_o           => led_link_o,
      btn1_i               => btn1_i,
      btn2_i               => btn2_i,
      pps_p_o              => pps_p_o,
      pps_led_o            => pps_led_o,
      link_ok_o            => link_ok_o);

end architecture struct;
