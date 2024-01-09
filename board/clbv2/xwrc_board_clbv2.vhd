-------------------------------------------------------------------------------
-- Title      : WRPC Wrapper for clbv2
-- Project    : WR PTP Core
-- URL        : http://www.ohwr.org/projects/wr-cores/wiki/Wrpc_core
-------------------------------------------------------------------------------
-- File       : xwrc_board_clbv2.vhd
-- Author(s)  : Peter Jansweijer <peterj@nikhef.nl>
-- Company    : Nikhef
-- Created    : 2017-11-08
-- Last update: 2024-01-09
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Top-level wrapper for WR PTP core including all the modules
-- needed to operate the core on the clbv2 board.
-------------------------------------------------------------------------------
-- Copyright (c) 2024 Nikhef
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
use work.wr_fabric_pkg.all;
use work.endpoint_pkg.all;
use work.streamers_pkg.all;
use work.wr_xilinx_pkg.all;
use work.wr_board_pkg.all;
use work.wr_clbv2_pkg.all;

library unisim;
use unisim.vcomponents.all;

entity xwrc_board_clbv2 is
  generic(
    -- set to 1 to speed up some initialization processes during simulation
    g_simulation                : integer              := 0;
    -- Select whether to include external ref clock input
    g_with_external_clock_input : boolean              := TRUE;
    -- Number of aux clocks syntonized by WRPC to WR timebase
    g_aux_clks                  : integer              := 1;
    -- plain     = expose WRC fabric interface
    -- streamers = attach WRC streamers to fabric interface
    -- etherbone = attach Etherbone slave to fabric interface
    g_fabric_iface              : t_board_fabric_iface := plain;
    -- parameters configuration when g_fabric_iface = "streamers" (otherwise ignored)
    g_streamers_op_mode        : t_streamers_op_mode  := TX_AND_RX;
    g_tx_streamer_params       : t_tx_streamer_params := c_tx_streamer_params_defaut;
    g_rx_streamer_params       : t_rx_streamer_params := c_rx_streamer_params_defaut;
    -- memory initialisation file for embedded CPU
    g_dpram_initf               : string               := "default_xilinx";
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
    -- Reset input (active low, can be async)
    areset_n_i          : in  std_logic;
    -- Optional reset input active low with rising edge detection. Does not
    -- reset PLLs.
    areset_edge_n_i     : in  std_logic := '1';
    -- Clock inputs from the board
    clk_20m_vcxo_i      : in  std_logic;
    clk_125m_gtx_n_i    : in  std_logic;
    clk_125m_gtx_p_i    : in  std_logic;
    -- Aux clocks, which can be disciplined by the WR Core
    clk_aux_i           : in  std_logic_vector(g_aux_clks-1 downto 0) := (others => '0');
    -- 10MHz ext ref clock input (g_with_external_clock_input = TRUE)
    clk_10m_ext_i       : in  std_logic                               := '0';
    -- External PPS input (g_with_external_clock_input = TRUE)
    pps_ext_i           : in  std_logic                               := '0';
    -- 62.5MHz sys clock output
    clk_sys_62m5_o      : out std_logic;
    -- 62.5MHz ref clock output
    clk_ref_62m5_o      : out std_logic;
    -- active low reset outputs, synchronous to 62m5 and 125m clocks
    rst_sys_62m5_n_o    : out std_logic;
    rst_ref_62m5_n_o    : out std_logic;

    ---------------------------------------------------------------------------
    -- Shared SPI interface to DACs
    ---------------------------------------------------------------------------
    plldac_sclk_o   : out std_logic;
    plldac_din_o    : out std_logic;
    pll25dac_cs_n_o : out std_logic;
    pll20dac_cs_n_o : out std_logic;

    ---------------------------------------------------------------------------
    -- SFP I/O for transceiver and SFP management info
    ---------------------------------------------------------------------------
    sfp_txp_o         : out std_logic;
    sfp_txn_o         : out std_logic;
    sfp_rxp_i         : in  std_logic;
    sfp_rxn_i         : in  std_logic;
    sfp_det_i         : in  std_logic := '1';
    sfp_sda_i         : in  std_logic;
    sfp_sda_o         : out std_logic;
    sfp_scl_i         : in  std_logic;
    sfp_scl_o         : out std_logic;
    sfp_rate_select_o : out std_logic;
    sfp_tx_fault_i    : in  std_logic := '0';
    sfp_tx_disable_o  : out std_logic;
    sfp_los_i         : in  std_logic := '0';

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
    -- UART
    ---------------------------------------------------------------------------
    uart_rxd_i : in  std_logic;
    uart_txd_o : out std_logic;

    ---------------------------------------------------------------------------
    -- No Flash memory SPI interface
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- No External WB interface
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- WR fabric interface (when g_fabric_iface = "plainfbrc")
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
    -- No Etherbone WB master interface (when g_fabric_iface = "etherbone")
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- Generic diagnostics interface (access from WRPC via SNMP or uart console
    ---------------------------------------------------------------------------
    aux_diag_i : in  t_generic_word_array(g_diag_ro_size-1 downto 0) := (others => (others => '0'));
    aux_diag_o : out t_generic_word_array(g_diag_rw_size-1 downto 0);

    ---------------------------------------------------------------------------
    -- Aux clocks control
    ---------------------------------------------------------------------------
    tm_dac_value_o       : out std_logic_vector(31 downto 0);
    tm_dac_wr_o          : out std_logic_vector(g_aux_clks-1 downto 0);
    tm_clk_aux_lock_en_i : in  std_logic_vector(g_aux_clks-1 downto 0) := (others => '0');
    tm_clk_aux_locked_o  : out std_logic_vector(g_aux_clks-1 downto 0);

    ---------------------------------------------------------------------------
    -- External Tx Timestamping I/F
    ---------------------------------------------------------------------------
    timestamps_o     : out t_txtsu_timestamp;
    timestamps_ack_i : in  std_logic := '1';

    -----------------------------------------
    -- Timestamp helper signals, used for Absolute Calibration
    -----------------------------------------
    abscal_txts_o       : out std_logic;
    abscal_rxts_o       : out std_logic;

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

end entity xwrc_board_clbv2;

architecture struct of xwrc_board_clbv2 is

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------

  -- IBUFDS
  signal clk_125m_gtx_buf   : std_logic;
  signal clk_125m_gtx_odiv2 : std_logic;
  signal clk_20m_vcxo_buf   : std_logic;
  signal clk_dmtd           : std_logic;

  -- PLLs, clocks
  signal clk_sys_62m5       : std_logic;
  signal clk_ref_62m5       : std_logic;
  signal clk_dmtd_fb        : std_logic;
  signal clk_dmtd_pll       : std_logic;
  signal pll_locked         : std_logic;
  signal clk_ref_locked     : std_logic;
  signal clk_10m_ext        : std_logic;

  -- Reset logic
  signal pll_arst           : std_logic;
  signal areset_edge_ppulse : std_logic;
  signal rst_62m5_n         : std_logic;
  signal rstlogic_arst_n    : std_logic;
  signal rstlogic_clk_in    : std_logic_vector(1 downto 0);
  signal rstlogic_rst_out   : std_logic_vector(1 downto 0);

  -- PLL DACs
  signal dac_dmtd_load   : std_logic;
  signal dac_dmtd_data      : std_logic_vector(15 downto 0);
  signal dac_refclk_load   : std_logic;
  signal dac_refclk_data      : std_logic_vector(15 downto 0);

  -- OneWire
  signal onewire_in : std_logic_vector(1 downto 0);
  signal onewire_en : std_logic_vector(1 downto 0);

  -- PHY
  signal phy16_to_wrc   : t_phy_16bits_to_wrc;
  signal phy16_from_wrc : t_phy_16bits_from_wrc;

  -- External reference
  signal ext_ref_mul         : std_logic;
  signal ext_ref_mul_locked  : std_logic;
  signal ext_ref_mul_stopped : std_logic;
  signal ext_ref_rst         : std_logic;

  signal phy_mdio_master_in   : t_wishbone_master_in := 
    (ack => '1',
     err => '0',
     rty => '0',
     stall => '0',
     dat => (others => '1')
     );
  signal phy_mdio_master_out  : t_wishbone_master_out;

begin  -- architecture struct

  -----------------------------------------------------------------------------
  -- Platform-dependent part (PHY, PLLs, buffers, etc)
  -----------------------------------------------------------------------------

  -- active high async reset for PLLs
  pll_arst <= not areset_n_i;

  -- DMTD PLL input clock buffer
  cmp_clk_dmtd_buf_i : BUFG
    port map (
      O => clk_20m_vcxo_buf,
      I => clk_20m_vcxo_i);

 -- DMTD PLL (20 MHz -> ~62,5 MHz)
 cmp_dmtd_clk_pll : MMCME2_ADV
   generic map (
     BANDWIDTH            => "OPTIMIZED",
     CLKOUT4_CASCADE      => false,
     COMPENSATION         => "ZHOLD",
     STARTUP_WAIT         => false,
     DIVCLK_DIVIDE        => 1,
     CLKFBOUT_MULT_F      => 50.000,    -- 20 MHz -> 1 GHz
     CLKFBOUT_PHASE       => 0.000,
     CLKFBOUT_USE_FINE_PS => false,
     CLKOUT0_DIVIDE_F     => 16.000,    -- 1GHz/16 -> 62.5 MHz
     CLKOUT0_PHASE        => 0.000,
     CLKOUT0_DUTY_CYCLE   => 0.500,
     CLKOUT0_USE_FINE_PS  => false,
     CLKOUT1_DIVIDE       => 16,        -- 1GHz/16 -> 62.5 MHz
     CLKOUT1_PHASE        => 0.000,
     CLKOUT1_DUTY_CYCLE   => 0.500,
     CLKOUT1_USE_FINE_PS  => false,
     CLKIN1_PERIOD        => 50.000,    -- 50ns for 20 MHz
     REF_JITTER1          => 0.010)
   port map (
     -- Output clocks
     CLKFBOUT     => clk_dmtd_fb,
     CLKOUT0      => clk_dmtd_pll,
     -- Input clock control
     CLKFBIN      => clk_dmtd_fb,
     CLKIN1       => clk_20m_vcxo_buf,
     CLKIN2       => '0',
     -- Tied to always select the primary input clock
     CLKINSEL     => '1',
     -- Ports for dynamic reconfiguration
     DADDR        => (others => '0'),
     DCLK         => '0',
     DEN          => '0',
     DI           => (others => '0'),
     DO           => open,
     DRDY         => open,
     DWE          => '0',
     -- Ports for dynamic phase shift
     PSCLK        => '0',
     PSEN         => '0',
     PSINCDEC     => '0',
     PSDONE       => open,
     -- Other control and status signals
     LOCKED       => pll_locked,
     CLKINSTOPPED => open,
     CLKFBSTOPPED => open,
     PWRDWN       => '0',
     RST          => pll_arst);

 -- DMTD PLL output clock buffer
 cmp_clk_dmtd_buf_o : BUFG
   port map (
     O => clk_dmtd,
     I => clk_dmtd_pll);

  -----------------------------------------------------------------------------
  -- Dedicated GTX clock.
  -----------------------------------------------------------------------------

  cmp_gtx_dedicated_clk : IBUFDS_GTE2
    generic map(
      CLKCM_CFG    => true,
      CLKRCV_TRST  => true,
      CLKSWING_CFG => "11")
    port map (
      O     => clk_125m_gtx_buf,
      ODIV2 => clk_125m_gtx_odiv2,
      CEB   => '0',
      I     => clk_125m_gtx_p_i,
      IB    => clk_125m_gtx_n_i);

    cmp_clk_ref62m5_buf : BUFG
      port map (
        O => clk_ref_62m5,
        I => clk_125m_gtx_odiv2);

  clk_sys_62m5   <= clk_ref_62m5;
  clk_sys_62m5_o <= clk_ref_62m5;
  clk_ref_62m5_o <= clk_ref_62m5;

  ---------------------------------------------------------------------------
  --   Kintex7 low phase drift PHY
  ---------------------------------------------------------------------------

  cmp_gtx_lp: wr_gtx_phy_family7_lp
    generic map(
      g_simulation => g_simulation)
    port map(
      clk_gtx_i      => clk_125m_gtx_buf,
      clk_dmtd_i     => clk_dmtd,
      clk_ref_i      => clk_ref_62m5,
      clk_sys_i      => clk_sys_62m5,
      rst_sys_n_i    => rst_62m5_n,
      tx_data_i      => phy16_from_wrc.tx_data,
      tx_k_i         => phy16_from_wrc.tx_k,
      tx_disparity_o => phy16_to_wrc.tx_disparity,
      tx_enc_err_o   => phy16_to_wrc.tx_enc_err,
      rx_rbclk_o     => phy16_to_wrc.rx_clk,
      clk_sampled_o  => phy16_to_wrc.rx_sampled_clk,
      rx_data_o      => phy16_to_wrc.rx_data,
      rx_k_o         => phy16_to_wrc.rx_k,
      rx_enc_err_o   => phy16_to_wrc.rx_enc_err,
      rx_bitslide_o  => phy16_to_wrc.rx_bitslide,
      rst_i          => phy16_from_wrc.rst,
      loopen_i       => phy16_from_wrc.loopen,
      tx_prbs_sel_i  => phy16_from_wrc.tx_prbs_sel,
      rdy_o          => phy16_to_wrc.rdy,
      mdio_slave_i   => phy_mdio_master_out,
      mdio_slave_o   => phy_mdio_master_in,

      pad_txn_o => sfp_txn_o,
      pad_txp_o => sfp_txp_o,
      pad_rxn_i => sfp_rxn_i,
      pad_rxp_i => sfp_rxp_i,

      tx_locked_o   => clk_ref_locked
    );

  phy16_to_wrc.ref_clk      <= clk_ref_62m5;
  phy16_to_wrc.sfp_tx_fault <= sfp_tx_fault_i;
  phy16_to_wrc.sfp_los      <= sfp_los_i;
  sfp_tx_disable_o          <= phy16_from_wrc.sfp_tx_disable;

  ---------------------------------------------------------------------------
  -- External 10MHz reference PLL for Kintex7
  ---------------------------------------------------------------------------

  gen_ext_ref_pll : if (g_with_external_clock_input = TRUE) generate
    
    signal clk_ext_fbi : std_logic;
    signal clk_ext_fbo : std_logic;
    signal clk_ext_mul : std_logic;
    signal pll_ext_rst : std_logic;

  begin
    mmcm_adv_inst : MMCME2_ADV
      generic map (
        BANDWIDTH            => "OPTIMIZED",
        CLKOUT4_CASCADE      => FALSE,
        COMPENSATION         => "ZHOLD",
        STARTUP_WAIT         => FALSE,
        DIVCLK_DIVIDE        => 1,
        CLKFBOUT_MULT_F      => 62.500,
        CLKFBOUT_PHASE       => 0.000,
        CLKFBOUT_USE_FINE_PS => FALSE,
        CLKOUT0_DIVIDE_F     => 10.000,
        CLKOUT0_PHASE        => 0.000,
        CLKOUT0_DUTY_CYCLE   => 0.500,
        CLKOUT0_USE_FINE_PS  => FALSE,
        CLKIN1_PERIOD        => 100.000,
        REF_JITTER1          => 0.005)
      port map (
        -- Output clocks
        CLKFBOUT  => clk_ext_fbo,
        CLKOUT0   => clk_ext_mul,
        -- Input clock control
        CLKFBIN   => clk_ext_fbi,
        CLKIN1    => clk_10m_ext,
        CLKIN2    => '0',
        -- Tied to always select the primary input clock
        CLKINSEL  => '1',
        -- Ports for dynamic reconfiguration
        DADDR     => (others => '0'),
        DCLK      => '0',
        DEN       => '0',
        DI        => (others => '0'),
        DO        => open,
        DRDY      => open,
        DWE       => '0',
        -- Ports for dynamic phase shift
        PSCLK     => '0',
        PSEN      => '0',
        PSINCDEC  => '0',
        PSDONE    => open, -- Other control and status signals
        LOCKED    => ext_ref_mul_locked,
        CLKINSTOPPED => ext_ref_mul_stopped,
        CLKFBSTOPPED => open,
        PWRDWN   => '0',
        RST      => pll_ext_rst);

    -- External reference input buffer
    cmp_clk_ext_buf_i : BUFG
      port map (
        O => clk_10m_ext,
        I => clk_10m_ext_i);

    -- External reference feedback buffer
    cmp_clk_ext_buf_fb : BUFG
      port map (
        O => clk_ext_fbi,
        I => clk_ext_fbo);

    -- External reference output buffer
    cmp_clk_ext_buf_o : BUFG
      port map (
        O => ext_ref_mul,
        I => clk_ext_mul);

    cmp_extend_ext_reset : gc_extend_pulse
      generic map (
        g_width => 1000)
      port map (
        clk_i      => clk_sys_62m5,
        rst_n_i    => '1',
        pulse_i    => ext_ref_rst,
        extended_o => pll_ext_rst);

  end generate gen_ext_ref_pll;

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
      clk_i    => clk_sys_62m5,
      rst_n_i  => '1',
      data_i   => areset_edge_n_i,
      ppulse_o => areset_edge_ppulse);

  -- logic AND of all async reset sources (active low)
  rstlogic_arst_n <= pll_locked and areset_n_i and (not areset_edge_ppulse);

  -- concatenation of all clocks required to have synced resets
  rstlogic_clk_in(0) <= clk_sys_62m5;
  rstlogic_clk_in(1) <= clk_ref_62m5;

  cmp_rstlogic_reset : gc_reset
    generic map (
      g_clocks    => 2,                           -- 62.5MHz, 125MHz
      g_logdelay  => 4,                           -- 16 clock cycles
      g_syncdepth => 3)                           -- length of sync chains
    port map (
      free_clk_i => clk_20m_vcxo_i,
      locked_i   => rstlogic_arst_n,
      clks_i     => rstlogic_clk_in,
      rstn_o     => rstlogic_rst_out);

  -- distribution of resets (already synchronized to their clock domains)
  rst_62m5_n <= rstlogic_rst_out(0);

  rst_sys_62m5_n_o <= rst_62m5_n;
  rst_ref_62m5_n_o <= rstlogic_rst_out(1);

  -----------------------------------------------------------------------------
  -- 2x SPI DAC
  -----------------------------------------------------------------------------

  cmp_dac_arb : spec_serial_dac_arb
    generic map (
      g_invert_sclk    => FALSE,
      g_num_extra_bits => 8)
    port map (
      clk_i         => clk_sys_62m5,
      rst_n_i       => rst_62m5_n,
      val1_i        => dac_refclk_data,
      load1_i       => dac_refclk_load,
      val2_i        => dac_dmtd_data,
      load2_i       => dac_dmtd_load,
      dac_cs_n_o(0) => pll25dac_cs_n_o,
      dac_cs_n_o(1) => pll20dac_cs_n_o,
      dac_sclk_o    => plldac_sclk_o,
      dac_din_o     => plldac_din_o);

  -----------------------------------------------------------------------------
  -- The WR PTP core with optional fabric interface attached
  -----------------------------------------------------------------------------

  cmp_board_common : xwrc_board_common
    generic map (
      g_simulation                => g_simulation,
      g_with_external_clock_input => g_with_external_clock_input,
      g_ram_address_space_size_kb => 256,
      g_board_name                => "CLB2",
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
      g_softpll_use_sampled_ref_clocks => TRUE,
      g_vuart_fifo_size           => 1024,
      g_pcs_16bit                 => TRUE,
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
      clk_sys_i            => clk_sys_62m5,
      clk_dmtd_i           => clk_dmtd,
      clk_ref_i            => clk_ref_62m5,
      clk_aux_i            => clk_aux_i,
      clk_10m_ext_i        => clk_10m_ext,
      clk_ext_mul_i        => ext_ref_mul,
      clk_ext_mul_locked_i => ext_ref_mul_locked,
      clk_ext_stopped_i    => ext_ref_mul_stopped,
      clk_ext_rst_o        => ext_ref_rst,
      pps_ext_i            => pps_ext_i,
      rst_n_i              => rst_62m5_n,
      dac_hpll_load_p1_o   => dac_dmtd_load,
      dac_hpll_data_o      => dac_dmtd_data,
      dac_dpll_load_p1_o   => dac_refclk_load,
      dac_dpll_data_o      => dac_refclk_data,
      phy16_o              => phy16_from_wrc,
      phy16_i              => phy16_to_wrc,
      phy_mdio_master_i    => phy_mdio_master_in,
      phy_mdio_master_o    => phy_mdio_master_out,
      scl_o                => eeprom_scl_o,
      scl_i                => eeprom_scl_i,
      sda_o                => eeprom_sda_o,
      sda_i                => eeprom_sda_i,
      sfp_scl_o            => sfp_scl_o,
      sfp_scl_i            => sfp_scl_i,
      sfp_sda_o            => sfp_sda_o,
      sfp_sda_i            => sfp_sda_i,
      sfp_det_i            => sfp_det_i,
      spi_sclk_o           => open,
      spi_ncs_o            => open,
      spi_mosi_o           => open,
      spi_miso_i           => '0',
      uart_rxd_i           => uart_rxd_i,
      uart_txd_o           => uart_txd_o,
      owr_pwren_o          => open,
      owr_en_o             => onewire_en,
      owr_i                => onewire_in,
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
      aux_diag_i           => aux_diag_i,
      aux_diag_o           => aux_diag_o,
      tm_dac_value_o       => tm_dac_value_o,
      tm_dac_wr_o          => tm_dac_wr_o,
      tm_clk_aux_lock_en_i => tm_clk_aux_lock_en_i,
      tm_clk_aux_locked_o  => tm_clk_aux_locked_o,
      timestamps_o         => timestamps_o,
      timestamps_ack_i     => timestamps_ack_i,
      abscal_txts_o        => abscal_txts_o,
      abscal_rxts_o        => abscal_rxts_o,
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

  sfp_rate_select_o <= '1';

  onewire_oen_o <= onewire_en(0);
  onewire_in(0) <= onewire_i;
  onewire_in(1) <= '1';

end architecture struct;
