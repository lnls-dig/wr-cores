-------------------------------------------------------------------------------
-- Title      : Platform-dependent components needed for WR PTP Core on Xilinx
-- Project    : WR PTP Core
-------------------------------------------------------------------------------
-- File       : wrc_platform_xilinx.vhd
-- Author     : Maciej Lipinski, Grzegorz Daniluk
-- Company    : CERN
-- Platform   : FPGA-generic
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description:
-- This module instantiates platform-specific modules that are needed by the
-- WR PTP Core (WRPC) to interface hardware on Xilinx FPGA. In particular it
-- contains:
-- * PHY
-- * PLLs
-- * buffers
-- * controller of DACs
-- * access to SFP config
--
-------------------------------------------------------------------------------
--
-- Copyright (c) 2016 CERN / BE-CO-HT
--
-- This source file is free software; you can redistribute it
-- and/or modify it under the terms of the GNU Lesser General
-- Public License as published by the Free Software Foundation;
-- either version 2.1 of the License, or (at your option) any
-- later version
--
-- This source is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE.  See the GNU Lesser General Public License for more
-- details
--
-- You should have received a copy of the GNU Lesser General
-- Public License along with this source; if not, download it
-- from http://www.gnu.org/licenses/lgpl-2.1.html
--
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

use work.wr_fabric_pkg.all;
use work.wishbone_pkg.all;
use work.wr_xilinx_pkg.all;
use work.wrcore_pkg.all;
use work.gencores_pkg.all;
use work.endpoint_pkg.all;

library UNISIM;
use UNISIM.vcomponents.all;

entity xwrc_platform_xilinx is
  generic
    (
      -- setting g_simulation to TRUE will speed up some initialization processes
      g_simulation         :       integer := 0;
      -- define the Familiy of Xilinx FPGAs (supported: now only spartan6)
      g_family             :       string  := "spartan6";
      g_with_10m_refin     :       integer := 0
    );
  port (
      local_reset_n_i      : in    std_logic;
      ---------------------------------------------------------------------------------------
      -- main clocks
      ---------------------------------------------------------------------------------------
      clk_20m_vcxo_i       : in    std_logic;                     -- 20MHz VCXO clock
      clk_125m_pllref_p_i  : in    std_logic;                     -- 125 MHz PLL reference
      clk_125m_pllref_n_i  : in    std_logic;
      clk_125m_gtp_n_i     : in    std_logic;                     -- 125 MHz GTP reference
      clk_125m_gtp_p_i     : in    std_logic;                     -- 125 MHz GTP reference

      ---------------------------------------------------------------------------------------
      -- External 10MHz & 1-PPS reference
      ---------------------------------------------------------------------------------------
      clk_10m_ref_p_i      : in std_logic := '0';                 -- 10MHz external reference
      clk_10m_ref_n_i      : in std_logic := '0';                 -- 10MHz external reference
      pps_ext_i            : in std_logic := '0';                 -- external 1-PPS from reference

      ---------------------------------------------------------------------------------------
      -- I2C to control DAC
      ---------------------------------------------------------------------------------------
      dac_sclk_o           : out   std_logic;                      -- Serial Clock Line
      dac_din_o            : out   std_logic;                      -- Serial Data Line
      dac_cs1_n_o          : out   std_logic;                      -- Chip Select
      dac_cs2_n_o          : out   std_logic;                      -- Chip Select

      ---------------------------------------------------------------------------------------
      -- one-wire access to thermometer
      ---------------------------------------------------------------------------------------
      carrier_onewire_b    : inout std_logic := '1';               -- read temperature sensor

      ---------------------------------------------------------------------------------------
      -- SFP pins
      ---------------------------------------------------------------------------------------
      sfp_txp_o            : out   std_logic;
      sfp_txn_o            : out   std_logic;
      sfp_rxp_i            : in    std_logic;
      sfp_rxn_i            : in    std_logic;
      sfp_mod_def0_i       : in    std_logic;                      -- sfp detect
      sfp_mod_def1_b       : inout std_logic;                      -- Config-I2C: Clk  Line
      sfp_mod_def2_b       : inout std_logic;                      -- Config-I2C: Data Line
      sfp_rate_select_b    : inout std_logic;
      sfp_tx_fault_i       : in    std_logic;
      sfp_tx_disable_o     : out   std_logic;
      sfp_los_i            : in    std_logic;

      ---------------------------------------------------------------------------------------
      --Interface to WR PTP Core (WRPC)
      ---------------------------------------------------------------------------------------

      clk_62m5_sys_o       : out std_logic;
      clk_125m_pllref_o    : out std_logic;
      clk_62m5_dmtd_o      : out std_logic;
      dacs_i               : in  t_dacs_from_wrc;
      phy8_o               : out t_phy_8bits_to_wrc;
      phy8_i               : in  t_phy_8bits_from_wrc := c_dummy_phy8_from_wrc;
      phy16_o              : out t_phy_16bits_to_wrc;
      phy16_i              : in  t_phy_16bits_from_wrc := c_dummy_phy16_from_wrc;
      owr_en_i             : in  std_logic_vector(1 downto 0);
      owr_o                : out std_logic_vector(1 downto 0);
      sfp_config_o         : out t_sfp_to_wrc;
      sfp_config_i         : in  t_sfp_from_wrc;
      ext_ref_o            : out t_extref_to_wrc;
      ext_ref_rst_i        : in  std_logic := '0'
      );

end xwrc_platform_xilinx;

architecture rtl of xwrc_platform_xilinx is

  -------------------------------------------------------------------------------------------
  -- Signals declaration
  -------------------------------------------------------------------------------------------
  -- Clocks
  signal clk_62m5_pllout_sys       : std_logic;
  signal clk_62m5_pllout_dmtd      : std_logic;
  signal clk_62m5_pllout_fb_pllref : std_logic;
  signal clk_62m5_pllout_fb_dmtd   : std_logic;
  signal clk_20m_vcxo_buf          : std_logic;
  signal clk_125m_pllref           : std_logic;
  signal clk_125m_pllref_bufg      : std_logic;
  signal clk_125m_gtp              : std_logic;
  signal clk_62m5_sys              : std_logic;
  signal clk_80m_ADC               : std_logic;

  -- External 10MHz reference
  signal clk_10m_ref  : std_logic;
  signal clk_10m_ref_bufg  : std_logic;
  signal ext_pll_rst  : std_logic;

  -- WRPC <--> EEPROM on FMC
  signal wrc_scl_out               : std_logic;
  signal wrc_sda_out               : std_logic;
  signal wrc_scl_in                : std_logic;
  signal wrc_sda_in                : std_logic;

  -- WRPC <--> SFP config
  signal sfp_scl_out               : std_logic;
  signal sfp_scl_in                : std_logic;
  signal sfp_sda_out               : std_logic;
  signal sfp_sda_in                : std_logic;

begin

  -------------------------------------------------------------------------------------------
  -- all the clock-related stuff (PLLs etc)
  -------------------------------------------------------------------------------------------
  cmp_sys_clk_pll : PLL_BASE
    generic map (
      BANDWIDTH            => "OPTIMIZED",
      CLK_FEEDBACK         => "CLKFBOUT",
      COMPENSATION         => "INTERNAL",
      DIVCLK_DIVIDE        => 1,
      CLKFBOUT_MULT        => 8,
      CLKFBOUT_PHASE       => 0.000,
      CLKOUT0_DIVIDE       => 16,         -- 62.5 MHz
      CLKOUT0_PHASE        => 0.000,
      CLKOUT0_DUTY_CYCLE   => 0.500,
      CLKOUT1_DIVIDE       => 16,         -- 62.5 MHz
      CLKOUT1_PHASE        => 0.000,
      CLKOUT1_DUTY_CYCLE   => 0.500,
      CLKOUT2_DIVIDE       => 16,
      CLKOUT2_PHASE        => 0.000,
      CLKOUT2_DUTY_CYCLE   => 0.500,
      CLKIN_PERIOD         => 8.0,
      REF_JITTER           => 0.016)
    port map (
      CLKFBOUT             => clk_62m5_pllout_fb_pllref,
      CLKOUT0              => clk_62m5_pllout_sys,
      CLKOUT1              => open,
      CLKOUT2              => open,
      CLKOUT3              => open,
      CLKOUT4              => open,
      CLKOUT5              => open,
      LOCKED               => open,
      RST                  => '0',
      CLKFBIN              => clk_62m5_pllout_fb_pllref,
      CLKIN                => clk_125m_pllref_bufg);

  cmp_dmtd_clk_pll : PLL_BASE
    generic map (
      BANDWIDTH            => "OPTIMIZED",
      CLK_FEEDBACK         => "CLKFBOUT",
      COMPENSATION         => "INTERNAL",
      DIVCLK_DIVIDE        => 1,
      CLKFBOUT_MULT        => 50,
      CLKFBOUT_PHASE       => 0.000,
      CLKOUT0_DIVIDE       => 16,         -- 62.5 MHz
      CLKOUT0_PHASE        => 0.000,
      CLKOUT0_DUTY_CYCLE   => 0.500,
      CLKOUT1_DIVIDE       => 16,         -- 62.5 MHz
      CLKOUT1_PHASE        => 0.000,
      CLKOUT1_DUTY_CYCLE   => 0.500,
      CLKOUT2_DIVIDE       => 8,
      CLKOUT2_PHASE        => 0.000,
      CLKOUT2_DUTY_CYCLE   => 0.500,
      CLKIN_PERIOD         => 50.0,
      REF_JITTER           => 0.016)
    port map (
      CLKFBOUT             => clk_62m5_pllout_fb_dmtd,
      CLKOUT0              => clk_62m5_pllout_dmtd,
      CLKOUT1              => open,
      CLKOUT2              => open,
      CLKOUT3              => open,
      CLKOUT4              => open,
      CLKOUT5              => open,
      LOCKED               => open,
      RST                  => '0',
      CLKFBIN              => clk_62m5_pllout_fb_dmtd,
      CLKIN                => clk_20m_vcxo_buf);

  cmp_clk_sys_buf : BUFG
    port map (
      O                    => clk_62m5_sys,
      I                    => clk_62m5_pllout_sys);

  clk_62m5_sys_o <= clk_62m5_sys;

  cmp_clk_dmtd_buf : BUFG
    port map (
      O                    => clk_62m5_dmtd_o,
      I                    => clk_62m5_pllout_dmtd);

  cmp_clk_vcxo : BUFG
    port map (
      O                    => clk_20m_vcxo_buf,
      I                    => clk_20m_vcxo_i );

  cmp_pllrefclk_buf : IBUFGDS
    generic map (
      DIFF_TERM            => true,                 -- Differential Termination
      IBUF_LOW_PWR         => true,                 -- Low power (TRUE) vs. performance (FALSE)
                                                    -- setting for referenced I/O standards
      IOSTANDARD           => "DEFAULT")
    port map (
      O                    => clk_125m_pllref,      -- Buffer output
      I                    => clk_125m_pllref_p_i,  -- Diff_p buffer input (connect directly
                                                    -- to top-level port)
      IB                   => clk_125m_pllref_n_i );-- Diff_n buffer input (connect directly
                                                    -- to top-level port)
  cmp_pllrefclk_bufg : BUFG
    port map (
      O => clk_125m_pllref_bufg,
      I => clk_125m_pllref);

  phy8_o.ref_clk     <= clk_125m_pllref_bufg;
  clk_125m_pllref_o  <= clk_125m_pllref_bufg;

  -------------------------------------------------------------------------------------------
  -- Dedicated clock for GTP --ML:  different in SPEC -> need check
  -------------------------------------------------------------------------------------------
  cmp_Dedicated_GTP_Clock_Buffer : IBUFGDS
    generic map(
      DIFF_TERM            => true,
      IBUF_LOW_PWR         => true,
      IOSTANDARD           => "DEFAULT")
    port map (
      O                    => clk_125m_gtp,
      I                    => clk_125m_gtp_p_i,
      IB                   => clk_125m_gtp_n_i
      );

  -------------------------------------------------------------------------------------------
  -- PLL for multiplying external 10MHz reference clock
  -------------------------------------------------------------------------------------------
  GEN_EXT_REF: if g_with_10m_refin = 1 generate
    U_Ext_BUF : IBUFGDS
      generic map (
        DIFF_TERM => true)
      port map (
        O  => clk_10m_ref,
        I  => clk_10m_ref_p_i,
        IB => clk_10m_ref_n_i);

    U_Ext_BUFG : BUFG
      port map (
        O => clk_10m_ref_bufg,
        I => clk_10m_ref);

    U_Ext_PLL : ext_pll_10_to_125m
      port map (
        clk_ext_i        => clk_10m_ref_bufg,
        clk_ext_mul_o    => ext_ref_o.clk_125m_ref,
        rst_a_i          => ext_pll_rst,
        clk_in_stopped_o => ext_ref_o.stopped,
        locked_o         => ext_ref_o.locked);

    U_Extend_EXT_Reset : gc_extend_pulse
      generic map (
        g_width => 1000)
      port map (
        clk_i      => clk_62m5_sys,
        rst_n_i    => local_reset_n_i,
        pulse_i    => ext_ref_rst_i,
        extended_o => ext_pll_rst);
  end generate;
  ext_ref_o.clk_10m_ref <= clk_10m_ref_bufg;
  ext_ref_o.pps <= pps_ext_i;

  -------------------------------------------------------------------------------
  -- Tri-state access to devices (SFP, one wire thermometer)
  -------------------------------------------------------------------------------

  -- Tristates for SFP EEPROM
  sfp_mod_def1_b    <= '0' when sfp_config_i.scl = '0' else 'Z';
  sfp_mod_def2_b    <= '0' when sfp_config_i.sda = '0' else 'Z';
  sfp_config_o.scl  <= sfp_mod_def1_b;
  sfp_config_o.sda  <= sfp_mod_def2_b;
  
  sfp_config_o.det  <= sfp_mod_def0_i;

  carrier_onewire_b <= '0' when owr_en_i(0) = '1' else 'Z';
  owr_o(0)          <= carrier_onewire_b;

  -------------------------------------------------------------------------------
  -- PHY
  -------------------------------------------------------------------------------
  gen_phy_spartan6: if(g_family = "spartan6") generate
    cmp_GTP : wr_gtp_phy_spartan6
      generic map (
        g_simulation               => g_simulation,
        g_enable_ch0               => 0,
        g_enable_ch1               => 1)
      port map (
        gtp_clk_i                  => clk_125m_gtp,
        ch0_ref_clk_i              => clk_125m_pllref_bufg,
        ch0_tx_data_i              => x"00",
        ch0_tx_k_i                 => '0',
        ch0_tx_disparity_o         => open,
        ch0_tx_enc_err_o           => open,
        ch0_rx_rbclk_o             => open,
        ch0_rx_data_o              => open,
        ch0_rx_k_o                 => open,
        ch0_rx_enc_err_o           => open,
        ch0_rx_bitslide_o          => open,
        ch0_rst_i                  => '1',
        ch0_loopen_i               => '0',
        ch1_ref_clk_i              => clk_125m_pllref_bufg,
        ch1_tx_data_i              => phy8_i.tx_data,
        ch1_tx_k_i                 => phy8_i.tx_k(0),
        ch1_tx_disparity_o         => phy8_o.tx_disparity,
        ch1_tx_enc_err_o           => phy8_o.tx_enc_err,
        ch1_rx_data_o              => phy8_o.rx_data,
        ch1_rx_rbclk_o             => phy8_o.rx_clk, 
        ch1_rx_k_o                 => phy8_o.rx_k(0),
        ch1_rx_enc_err_o           => phy8_o.rx_enc_err,
        ch1_rx_bitslide_o          => phy8_o.rx_bitslide,
        ch1_rst_i                  => phy8_i.rst,
        ch1_loopen_i               => phy8_i.loopen,
        ch1_loopen_vec_i           => phy8_i.loopen_vec,
        ch1_tx_prbs_sel_i          => phy8_i.tx_prbs_sel,
        ch1_rdy_o                  => phy8_o.rdy,
        pad_txn0_o                 => open,
        pad_txp0_o                 => open,
        pad_rxn0_i                 => '0',
        pad_rxp0_i                 => '0',
        pad_txn1_o                 => sfp_txn_o,
        pad_txp1_o                 => sfp_txp_o,
        pad_rxn1_i                 => sfp_rxn_i,
        pad_rxp1_i                 => sfp_rxp_i
      );
    sfp_tx_disable_o    <= phy8_i.sfp_tx_disable;
    phy8_o.sfp_tx_fault <= sfp_tx_fault_i;
    phy8_o.sfp_los      <= sfp_los_i;
  end generate gen_phy_spartan6;

  gen_phy_unknown: if(g_family /= "spartan6") generate
    assert false report "unknown family for Xilinx is specified" severity error;
  end generate gen_phy_unknown;

  -------------------------------------------------------------------------------
  -- DAC control
  -------------------------------------------------------------------------------
  cmp_DAC_ARB : spec_serial_dac_arb
    generic map (
      g_invert_sclk              => false,
      g_num_extra_bits           => 8)
    port map (
      clk_i                      => clk_62m5_sys,
      rst_n_i                    => local_reset_n_i,
      val1_i                     => dacs_i.dpll_data,
      load1_i                    => dacs_i.dpll_load_p1,
      val2_i                     => dacs_i.hpll_data,
      load2_i                    => dacs_i.hpll_load_p1,
      dac_cs_n_o(0)              => dac_cs1_n_o,
      dac_cs_n_o(1)              => dac_cs2_n_o,
      dac_sclk_o                 => dac_sclk_o,
      dac_din_o                  => dac_din_o);

end rtl;
