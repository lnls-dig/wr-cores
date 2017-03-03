-------------------------------------------------------------------------------
-- Title      : WRPC reference design for SVEC
-- Project    : WR PTP Core
-- URL        : http://www.ohwr.org/projects/wr-cores/wiki/Wrpc_core
-------------------------------------------------------------------------------
-- File       : svec_wr_ref_top.vhd
-- Author(s)  : Dimitrios Lampridis  <dimitrios.lampridis@cern.ch>
-- Company    : CERN (BE-CO-HT)
-- Created    : 2017-02-16
-- Last update: 2017-03-10
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Top-level file for the WRPC reference design on the SVEC.
--
-- This is a reference top HDL that instanciates the WR PTP Core together with
-- its peripherals to be run on a SVEC card.
--
-- There are two main usecases for this HDL file:
-- * let new users easily synthesize a WR PTP Core bitstream that can be run on
--   reference hardware
-- * provide a reference top HDL file showing how the WRPC can be instantiated
--   in HDL projects.
--
-- VFC-HD:  http://www.ohwr.org/projects/svec/
--
-------------------------------------------------------------------------------
-- Copyright (c) 2017 CERN
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
use ieee.numeric_std.all;

library work;
use work.gencores_pkg.all;
use work.wishbone_pkg.all;
use work.xvme64x_core_pkg.all;
use work.wr_board_pkg.all;
use work.wr_svec_pkg.all;

library unisim;
use unisim.vcomponents.all;

entity svec_wr_ref_top is
  generic (
    g_dpram_initf : string := "../../bin/wrpc/wrc_phy8.bram";
    -- Simulation-mode enable parameter. Set by default (synthesis) to 0, and
    -- changed to non-zero in the instantiation of the top level DUT in the testbench.
    -- Its purpose is to reduce some internal counters/timeouts to speed up simulations.
    g_simulation : integer := 0
  );
  port (
    ---------------------------------------------------------------------------
    -- Clocks/resets
    ---------------------------------------------------------------------------

    -- Reset from system fpga
    rst_n_i : in std_logic;

    -- Local oscillators
    clk_20m_vcxo_i : in std_logic;                -- 20MHz VCXO clock

    clk_125m_pllref_p_i : in std_logic;           -- 125 MHz PLL reference
    clk_125m_pllref_n_i : in std_logic;

    clk_125m_gtp_n_i : in std_logic;              -- 125 MHz GTP reference
    clk_125m_gtp_p_i : in std_logic;

    ---------------------------------------------------------------------------
    -- VME interface
    ---------------------------------------------------------------------------

    vme_write_n_i    : in    std_logic;
    vme_sysreset_n_i : in    std_logic;
    vme_retry_oe_o   : out   std_logic;
    vme_retry_n_o    : out   std_logic;
    vme_lword_n_b    : inout std_logic;
    vme_iackout_n_o  : out   std_logic;
    vme_iackin_n_i   : in    std_logic;
    vme_iack_n_i     : in    std_logic;
    vme_gap_i        : in    std_logic;
    vme_dtack_oe_o   : out   std_logic;
    vme_dtack_n_o    : out   std_logic;
    vme_ds_n_i       : in    std_logic_vector(1 downto 0);
    vme_data_oe_n_o  : out   std_logic;
    vme_data_dir_o   : out   std_logic;
    vme_berr_o       : out   std_logic;
    vme_as_n_i       : in    std_logic;
    vme_addr_oe_n_o  : out   std_logic;
    vme_addr_dir_o   : out   std_logic;
    vme_irq_n_o      : out   std_logic_vector(7 downto 1);
    vme_ga_i         : in    std_logic_vector(4 downto 0);
    vme_data_b       : inout std_logic_vector(31 downto 0);
    vme_am_i         : in    std_logic_vector(5 downto 0);
    vme_addr_b       : inout std_logic_vector(31 downto 1);

    ---------------------------------------------------------------------------
    -- SPI interfaces to DACs
    ---------------------------------------------------------------------------

    pll20dac_din_o    : out std_logic;
    pll20dac_sclk_o   : out std_logic;
    pll20dac_sync_n_o : out std_logic;
    pll25dac_din_o    : out std_logic;
    pll25dac_sclk_o   : out std_logic;
    pll25dac_sync_n_o : out std_logic;

    ---------------------------------------------------------------------------
    -- SFP I/O for transceiver
    ---------------------------------------------------------------------------

    sfp_txp_o         : out   std_logic;
    sfp_txn_o         : out   std_logic;
    sfp_rxp_i         : in    std_logic;
    sfp_rxn_i         : in    std_logic;
    sfp_mod_def0_i    : in    std_logic;          -- sfp detect
    sfp_mod_def1_b    : inout std_logic;          -- scl
    sfp_mod_def2_b    : inout std_logic;          -- sda
    sfp_rate_select_o : out   std_logic;
    sfp_tx_fault_i    : in    std_logic;
    sfp_tx_disable_o  : out   std_logic;
    sfp_los_i         : in    std_logic;

    ---------------------------------------------------------------------------
    -- Carrier I2C EEPROM
    ---------------------------------------------------------------------------

    carrier_scl_b : inout std_logic;
    carrier_sda_b : inout std_logic;

    ---------------------------------------------------------------------------
    -- Onewire interface
    ---------------------------------------------------------------------------

    onewire_b : inout std_logic;

    ---------------------------------------------------------------------------
    -- UART
    ---------------------------------------------------------------------------

    uart_rxd_i : in  std_logic;
    uart_txd_o : out std_logic;

    ---------------------------------------------------------------------------
    -- SPI (flash is connected to SFPGA and routed to AFPGA
    -- once the boot process is complete)
    ---------------------------------------------------------------------------

    spi_sclk_o : out std_logic;
    spi_ncs_o  : out std_logic;
    spi_mosi_o : out std_logic;
    spi_miso_i : in  std_logic;

    ---------------------------------------------------------------------------
    -- Carrier front panel LEDs and IOs
    ---------------------------------------------------------------------------

    fp_led_line_oen_o : out std_logic_vector(1 downto 0);
    fp_led_line_o     : out std_logic_vector(1 downto 0);
    fp_led_column_o   : out std_logic_vector(3 downto 0);

    fp_gpio1_o      : out std_logic;              -- PPS output
    fp_gpio2_o      : out std_logic;              -- Ref clock div2 output
    fp_gpio3_i      : in  std_logic;              -- ext 10MHz clock input
    fp_gpio4_i      : in  std_logic;              -- ext PPS intput
    fp_term_en_o    : out std_logic_vector(4 downto 1);
    fp_gpio1_a2b_o  : out std_logic;
    fp_gpio2_a2b_o  : out std_logic;
    fp_gpio34_a2b_o : out std_logic);

end entity svec_wr_ref_top;

architecture top of svec_wr_ref_top is

  -----------------------------------------------------------------------------
  -- Constants
  -----------------------------------------------------------------------------

  -- Number of masters on the wishbone crossbar
  constant c_NUM_WB_MASTERS : integer := 2;

  -- Number of slaves on the primary wishbone crossbar
  constant c_NUM_WB_SLAVES : integer := 1;

  -- Primary Wishbone master(s) offsets
  constant c_WB_MASTER_VME     : integer := 0;
  constant c_WB_MASTER_ETHBONE : integer := 1;

  -- Primary Wishbone slave(s) offsets
  constant c_WB_SLAVE_WRC : integer := 0;

  -- sdb header address on primary crossbar
  constant c_SDB_ADDRESS : t_wishbone_address := x"00000000";

  -- f_xwb_bridge_manual_sdb(size, sdb_addr)
  -- Note: sdb_addr is the sdb records address relative to the bridge base address
  constant c_wrc_bridge_sdb : t_sdb_bridge :=
    f_xwb_bridge_manual_sdb(x"0003ffff", x"00030000");

  -- Primary wishbone crossbar layout
  constant c_WB_LAYOUT : t_sdb_record_array(c_NUM_WB_SLAVES - 1 downto 0) := (
    c_WB_SLAVE_WRC => f_sdb_embed_bridge(c_wrc_bridge_sdb, x"00040000"));

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------

  -- Wishbone buse(s) from masters attached to crossbar
  signal cnx_master_out : t_wishbone_master_out_array(c_NUM_WB_MASTERS-1 downto 0);
  signal cnx_master_in  : t_wishbone_master_in_array(c_NUM_WB_MASTERS-1 downto 0);

  -- Wishbone buse(s) to slaves attached to crossbar
  signal cnx_slave_out : t_wishbone_slave_out_array(c_NUM_WB_SLAVES-1 downto 0);
  signal cnx_slave_in  : t_wishbone_slave_in_array(c_NUM_WB_SLAVES-1 downto 0);

  -- clock and reset
  signal areset_n       : std_logic;
  signal clk_sys_62m5   : std_logic;
  signal rst_sys_62m5_n : std_logic;
  signal clk_ref_125m   : std_logic;
  signal clk_ref_div2   : std_logic;
  signal clk_ext_ref    : std_logic;

  -- I2C EEPROM
  signal eeprom_sda_in  : std_logic;
  signal eeprom_sda_out : std_logic;
  signal eeprom_scl_in  : std_logic;
  signal eeprom_scl_out : std_logic;

  -- VME
  signal vme_data_b_out    : std_logic_vector(31 downto 0);
  signal vme_addr_b_out    : std_logic_vector(31 downto 1);
  signal vme_lword_n_b_out : std_logic;
  signal Vme_data_dir_int  : std_logic;
  signal vme_addr_dir_int  : std_logic;
  signal vme_ga            : std_logic_vector(5 downto 0);

  -- SFP
  signal sfp_sda_in  : std_logic;
  signal sfp_sda_out : std_logic;
  signal sfp_scl_in  : std_logic;
  signal sfp_scl_out : std_logic;

  -- OneWire
  signal onewire_data : std_logic;
  signal onewire_oe   : std_logic;

  -- LEDs and GPIO
  signal pps         : std_logic;
  signal pps_led     : std_logic;
  signal pps_ext_in  : std_logic;
  signal svec_led    : std_logic_vector(15 downto 0);
  signal wr_led_link : std_logic;
  signal wr_led_act  : std_logic;

begin  -- architecture top

  ------------------------------------------------------------------------------
  -- System reset
  ------------------------------------------------------------------------------

  -- logic AND of all async reset sources (active low)
  areset_n <= vme_sysreset_n_i and rst_n_i;

  -----------------------------------------------------------------------------
  -- Primary wishbone Crossbar
  -----------------------------------------------------------------------------

  cmp_sdb_crossbar : xwb_sdb_crossbar
    generic map (
      g_num_masters => c_NUM_WB_MASTERS,
      g_num_slaves  => c_NUM_WB_SLAVES,
      g_registered  => TRUE,
      g_wraparound  => TRUE,
      g_layout      => c_WB_LAYOUT,
      g_sdb_addr    => c_SDB_ADDRESS)
    port map (
      clk_sys_i => clk_sys_62m5,
      rst_n_i   => rst_sys_62m5_n,
      slave_i   => cnx_master_out,
      slave_o   => cnx_master_in,
      master_i  => cnx_slave_out,
      master_o  => cnx_slave_in);

  -----------------------------------------------------------------------------
  -- VME64x Core (WB Master #1)
  -----------------------------------------------------------------------------

  cmp_vme_core : xvme64x_core
    port map (
      clk_i           => clk_sys_62m5,
      rst_n_i         => rst_sys_62m5_n,
      VME_AS_n_i      => vme_as_n_i,
      VME_RST_n_i     => '1',  -- vme_sysreset_n_i already in rst_sys_62m5_n
      VME_WRITE_n_i   => vme_write_n_i,
      VME_AM_i        => vme_am_i,
      VME_DS_n_i      => vme_ds_n_i,
      VME_GA_i        => vme_ga,
      VME_BERR_o      => vme_berr_o,
      VME_DTACK_n_o   => vme_dtack_n_o,
      VME_RETRY_n_o   => vme_retry_n_o,
      VME_RETRY_OE_o  => vme_retry_oe_o,
      VME_LWORD_n_b_i => vme_lword_n_b,
      VME_LWORD_n_b_o => vme_lword_n_b_out,
      VME_ADDR_b_i    => vme_addr_b,
      VME_DATA_b_o    => vme_data_b_out,
      VME_ADDR_b_o    => vme_addr_b_out,
      VME_DATA_b_i    => vme_data_b,
      VME_IRQ_n_o     => vme_irq_n_o,
      VME_IACK_n_i    => vme_iack_n_i,
      VME_IACKIN_n_i  => vme_iackin_n_i,
      VME_IACKOUT_n_o => vme_iackout_n_o,
      VME_DTACK_OE_o  => vme_dtack_oe_o,
      VME_DATA_DIR_o  => vme_data_dir_int,
      VME_DATA_OE_N_o => vme_data_oe_n_o,
      VME_ADDR_DIR_o  => vme_addr_dir_int,
      VME_ADDR_OE_N_o => vme_addr_oe_n_o,
      master_o        => cnx_master_out(c_WB_MASTER_VME),
      master_i        => cnx_master_in(c_WB_MASTER_VME),
      irq_i           => '0');

  vme_ga <= vme_gap_i & vme_ga_i;

  -- VME tri-state buffers
  vme_data_b    <= vme_data_b_out    when vme_data_dir_int = '1' else (others => 'Z');
  vme_addr_b    <= vme_addr_b_out    when vme_addr_dir_int = '1' else (others => 'Z');
  vme_lword_n_b <= vme_lword_n_b_out when vme_addr_dir_int = '1' else 'Z';

  vme_addr_dir_o <= vme_addr_dir_int;
  vme_data_dir_o <= vme_data_dir_int;

  -----------------------------------------------------------------------------
  -- The WR PTP core board package (WB Slave + WB Master #2 (Etherbone))
  -----------------------------------------------------------------------------

  cmp_xwrc_board_svec : xwrc_board_svec
    generic map (
      g_simulation                => g_simulation,
      g_with_external_clock_input => TRUE,
      g_dpram_initf               => g_dpram_initf,
      g_fabric_iface              => ETHERBONE)
    port map (
      clk_20m_vcxo_i      => clk_20m_vcxo_i,
      clk_125m_pllref_p_i => clk_125m_pllref_p_i,
      clk_125m_pllref_n_i => clk_125m_pllref_n_i,
      clk_125m_gtp_n_i    => clk_125m_gtp_n_i,
      clk_125m_gtp_p_i    => clk_125m_gtp_p_i,
      clk_10m_ext_i       => clk_ext_ref,
      areset_n_i          => areset_n,
      clk_sys_62m5_o      => clk_sys_62m5,
      clk_ref_125m_o      => clk_ref_125m,
      rst_sys_62m5_n_o    => rst_sys_62m5_n,
      pll20dac_din_o      => pll20dac_din_o,
      pll20dac_sclk_o     => pll20dac_sclk_o,
      pll20dac_sync_n_o   => pll20dac_sync_n_o,
      pll25dac_din_o      => pll25dac_din_o,
      pll25dac_sclk_o     => pll25dac_sclk_o,
      pll25dac_sync_n_o   => pll25dac_sync_n_o,
      sfp_txp_o           => sfp_txp_o,
      sfp_txn_o           => sfp_txn_o,
      sfp_rxp_i           => sfp_rxp_i,
      sfp_rxn_i           => sfp_rxn_i,
      sfp_det_i           => sfp_mod_def0_i,
      sfp_sda_i           => sfp_sda_in,
      sfp_sda_o           => sfp_sda_out,
      sfp_scl_i           => sfp_scl_in,
      sfp_scl_o           => sfp_scl_out,
      sfp_rate_select_o   => sfp_rate_select_o,
      sfp_tx_fault_i      => sfp_tx_fault_i,
      sfp_tx_disable_o    => sfp_tx_disable_o,
      sfp_los_i           => sfp_los_i,
      eeprom_sda_i        => eeprom_sda_in,
      eeprom_sda_o        => eeprom_sda_out,
      eeprom_scl_i        => eeprom_scl_in,
      eeprom_scl_o        => eeprom_scl_out,
      onewire_i           => onewire_data,
      onewire_oen_o       => onewire_oe,
      uart_rxd_i          => uart_rxd_i,
      uart_txd_o          => uart_txd_o,
      spi_sclk_o          => spi_sclk_o,
      spi_ncs_o           => spi_ncs_o,
      spi_mosi_o          => spi_mosi_o,
      spi_miso_i          => spi_miso_i,
      wb_slave_o          => cnx_slave_out(c_WB_SLAVE_WRC),
      wb_slave_i          => cnx_slave_in(c_WB_SLAVE_WRC),
      wb_eth_master_o     => cnx_master_out(c_WB_MASTER_ETHBONE),
      wb_eth_master_i     => cnx_master_in(c_WB_MASTER_ETHBONE),
      pps_ext_i           => pps_ext_in,
      pps_p_o             => pps,
      pps_led_o           => pps_led,
      led_link_o          => wr_led_link,
      led_act_o           => wr_led_act);

  -- tri-state Carrier EEPROM
  carrier_sda_b <= '0' when (eeprom_sda_out = '0') else 'Z';
  eeprom_sda_in <= carrier_sda_b;
  carrier_scl_b <= '0' when (eeprom_scl_out = '0') else 'Z';
  eeprom_scl_in <= carrier_scl_b;

  -- Tristates for SFP EEPROM
  sfp_mod_def1_b <= '0' when sfp_scl_out = '0' else 'Z';
  sfp_mod_def2_b <= '0' when sfp_sda_out = '0' else 'Z';
  sfp_scl_in     <= sfp_mod_def1_b;
  sfp_sda_in     <= sfp_mod_def2_b;

  -- tri-state onewire access
  onewire_b    <= '0' when (onewire_oe = '1') else 'Z';
  onewire_data <= onewire_b;

  ------------------------------------------------------------------------------
  -- Carrier front panel LEDs and LEMOs
  ------------------------------------------------------------------------------

  cmp_led_controller : gc_bicolor_led_ctrl
    generic map(
      g_nb_column    => 4,
      g_nb_line      => 2,
      g_clk_freq     => 62500000,                 -- in Hz
      g_refresh_rate => 250                       -- in Hz
      )
    port map(
      rst_n_i => rst_sys_62m5_n,
      clk_i   => clk_sys_62m5,

      led_intensity_i => "1100100",               -- in %

      led_state_i => svec_led,

      column_o   => fp_led_column_o,
      line_o     => fp_led_line_o,
      line_oen_o => fp_led_line_oen_o);

  -- LED 1
  svec_led(1 downto 0) <= c_led_green when wr_led_link = '1' else c_led_off;

  -- LED 5
  svec_led(9 downto 8) <= c_led_red_green when wr_led_act = '1' else c_led_off;

  -- LED 8
  svec_led(15 downto 14) <= c_led_green when pps_led = '1' else c_led_off;

  -- unused LEDs
  svec_led(7 downto 2)   <= (others => '0');
  svec_led(13 downto 10) <= (others => '0');

  -- Div by 2 reference clock to LEMO connector
  process(clk_ref_125m)
  begin
    if rising_edge(clk_ref_125m) then
      clk_ref_div2 <= not clk_ref_div2;
    end if;
  end process;

  -- Front panel IO configuration
  fp_gpio1_o      <= pps;
  fp_gpio2_o      <= clk_ref_div2;
  clk_ext_ref     <= fp_gpio3_i;
  pps_ext_in      <= fp_gpio4_i;
  fp_term_en_o    <= (others => '0');
  fp_gpio1_a2b_o  <= '1';
  fp_gpio2_a2b_o  <= '1';
  fp_gpio34_a2b_o <= '0';

end architecture top;
