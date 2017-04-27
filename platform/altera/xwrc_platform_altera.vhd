-------------------------------------------------------------------------------
-- Title      : Altera-specific components required by WR PTP Core
-- Project    : WR PTP Core
-- URL        : http://www.ohwr.org/projects/wr-cores/wiki/Wrpc_core
-------------------------------------------------------------------------------
-- File       : xwrc_platform_altera.vhd
-- Author(s)  : Dimitrios Lampridis  <dimitrios.lampridis@cern.ch>
-- Company    : CERN (BE-CO-HT)
-- Created    : 2016-11-21
-- Last update: 2017-04-27
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: This module instantiates platform-specific modules that are
-- needed by the WR PTP Core (WRPC) to interface hardware on Altera FPGA.
-- In particular it contains the Altera transceiver PHY and PLLs.
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
use work.endpoint_pkg.all;
use work.gencores_pkg.all;
use work.wr_altera_pkg.all;

entity xwrc_platform_altera is
  generic
    (
      -- Define the family/model of Altera FPGA
      -- (supported: for now only arria5)
      g_fpga_family               : string  := "arria5";
      -- Select whether to include external ref clock input
      g_with_external_clock_input : boolean := FALSE;
      -- Set to FALSE if you want to instantiate your own PLLs
      g_use_default_plls          : boolean := TRUE;
      -- Set to TRUE to use 16bit PCS (currently unsupported)
      g_pcs_16bit                 : boolean := FALSE
      );
  port (
    ---------------------------------------------------------------------------
    -- Asynchronous reset (active low)
    ---------------------------------------------------------------------------
    areset_n_i            : in  std_logic             := '1';
    ---------------------------------------------------------------------------
    -- 10MHz ext ref clock input (g_with_external_clock_input = TRUE)
    ---------------------------------------------------------------------------
    clk_10m_ext_i         : in  std_logic             := '0';
    ---------------------------------------------------------------------------
    -- Clock inputs for default PLLs (g_use_default_plls = TRUE)
    ---------------------------------------------------------------------------
    -- 20MHz VCXO clock
    clk_20m_vcxo_i        : in  std_logic             := '0';
    -- 125 MHz PLL reference
    clk_125m_pllref_i     : in  std_logic             := '0';
    ---------------------------------------------------------------------------
    -- Clock inputs from custom PLLs (g_use_default_plls = FALSE)
    ---------------------------------------------------------------------------
    -- 62.5MHz DMTD offset clock and lock status
    clk_62m5_dmtd_i       : in  std_logic             := '0';
    clk_dmtd_locked_i     : in  std_logic             := '1';
    -- 62.5MHz Main system clock and lock status
    clk_62m5_sys_i        : in  std_logic             := '0';
    clk_sys_locked_i      : in  std_logic             := '1';
    -- 125MHz  Reference clock
    clk_125m_ref_i        : in  std_logic             := '0';
    -- 125MHz derived from 10MHz external reference and lock status
    -- (when g_with_external_clock_input = TRUE)
    clk_125m_ext_i        : in  std_logic             := '0';
    clk_ext_locked_i      : in  std_logic             := '1';
    clk_ext_stopped_i     : in  std_logic             := '0';
    clk_ext_rst_o         : out std_logic;
    ---------------------------------------------------------------------------
    -- SFP
    ---------------------------------------------------------------------------
    sfp_tx_o              : out std_logic;
    sfp_rx_i              : in  std_logic;
    sfp_tx_fault_i        : in  std_logic             := '0';
    sfp_los_i             : in  std_logic             := '0';
    sfp_tx_disable_o      : out std_logic;
    ---------------------------------------------------------------------------
    --Interface to WR PTP Core (WRPC)
    ---------------------------------------------------------------------------
    -- PLL outputs
    clk_62m5_sys_o        : out std_logic;
    clk_125m_ref_o        : out std_logic;
    clk_62m5_dmtd_o       : out std_logic;
    pll_locked_o          : out std_logic;
    clk_10m_ext_o         : out std_logic;
    -- PHY
    phy8_o                : out t_phy_8bits_to_wrc;
    phy8_i                : in  t_phy_8bits_from_wrc  := c_dummy_phy8_from_wrc;
    phy16_o               : out t_phy_16bits_to_wrc;
    phy16_i               : in  t_phy_16bits_from_wrc := c_dummy_phy16_from_wrc;
    -- External reference
    ext_ref_mul_o         : out std_logic;
    ext_ref_mul_locked_o  : out std_logic;
    ext_ref_mul_stopped_o : out std_logic;
    ext_ref_rst_i         : in  std_logic             := '0'
    );

end entity xwrc_platform_altera;

architecture rtl of xwrc_platform_altera is

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------

  -- PLLs
  signal phy_clk     : std_logic;
  signal clk_pll_sys : std_logic;
  signal clk_pll_ref : std_logic;
  signal pll_arst    : std_logic;

  -- PHY
  signal phy_ready        : std_logic;
  signal phy_loopen       : std_logic;
  signal phy_rst          : std_logic;
  signal phy_tx_clk       : std_logic;
  signal phy_tx_data      : std_logic_vector(f_pcs_data_width(g_pcs_16bit)-1 downto 0);
  signal phy_tx_k         : std_logic_vector(f_pcs_k_width(g_pcs_16bit)-1 downto 0);
  signal phy_tx_disparity : std_logic;
  signal phy_tx_enc_err   : std_logic;
  signal phy_rx_rbclk     : std_logic;
  signal phy_rx_data      : std_logic_vector(f_pcs_data_width(g_pcs_16bit)-1 downto 0);
  signal phy_rx_k         : std_logic_vector(f_pcs_k_width(g_pcs_16bit)-1 downto 0);
  signal phy_rx_enc_err   : std_logic;
  signal phy_rx_bitslide  : std_logic_vector(f_pcs_bts_width(g_pcs_16bit)-1 downto 0);

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Check for unsupported features and/or misconfiguration
  -----------------------------------------------------------------------------
  gen_unknown_fpga : if (g_fpga_family /= "arria5") generate
    assert FALSE
      report "Altera FPGA family [" & g_fpga_family & "] is not supported"
      severity ERROR;
  end generate gen_unknown_fpga;

  gen_unsupported_pcs : if (g_pcs_16bit = TRUE) generate
    assert FALSE
      report "16bit PCS not yet supported"
      severity ERROR;
  end generate gen_unsupported_pcs;

  -----------------------------------------------------------------------------
  -- Clock PLLs
  -----------------------------------------------------------------------------

  -- active high async reset for PLLs
  pll_arst <= not areset_n_i;

  gen_default_plls : if (g_use_default_plls = TRUE) generate

    -- Default PLL setup consists of two PLLs.
    -- One takes a 125MHz clock signal as input and produces:
    -- a) 62.5MHz WR PTP core main system clock
    -- b) 125MHz WR PTP core reference clock
    -- The other PLL takes a 20MHz clock signal as input and produces
    -- the 62.5MHz DMTD clock.
    --
    -- A third PLL is instantiated if also g_with_external_clock_input = TRUE.
    -- In that case, a 10MHz external reference is multiplied to generate a
    -- 125MHz reference clock
    gen_arria5_default_plls : if (g_fpga_family = "arria5") generate

      signal pll_sys_locked  : std_logic;
      signal pll_dmtd_locked : std_logic;

    begin  --gen_arria5_default_plls

      cmp_sys_clk_pll : arria5_sys_pll_default
        port map (
          refclk   => clk_125m_pllref_i,
          rst      => pll_arst,
          outclk_0 => clk_pll_sys,
          outclk_1 => clk_pll_ref,
          locked   => pll_sys_locked);

      cmp_dmtd_clk_pll : arria5_dmtd_pll_default
        port map (
          refclk   => clk_20m_vcxo_i,
          rst      => pll_arst,
          outclk_0 => clk_62m5_dmtd_o,
          locked   => pll_dmtd_locked);

      clk_62m5_sys_o <= clk_pll_sys;
      clk_125m_ref_o <= clk_pll_ref;
      pll_locked_o   <= pll_sys_locked and pll_dmtd_locked;

      gen_arria5_ext_ref_pll : if (g_with_external_clock_input = TRUE) generate

        signal pll_ext_rst : std_logic;

      begin  --gen_arria5_ext_ref_pll

        cmp_ext_ref_pll : arria5_ext_ref_pll_default
          port map (
            refclk   => clk_10m_ext_i,
            rst      => pll_ext_rst,
            outclk_0 => ext_ref_mul_o,
            locked   => ext_ref_mul_locked_o);

        cmp_extend_ext_reset : gc_extend_pulse
          generic map (
            g_width => 1000)
          port map (
            clk_i      => clk_pll_sys,
            rst_n_i    => pll_sys_locked,
            pulse_i    => ext_ref_rst_i,
            extended_o => pll_ext_rst);

      end generate gen_arria5_ext_ref_pll;

      gen_arria5_no_ext_ref_pll : if (g_with_external_clock_input = FALSE) generate
        ext_ref_mul_o         <= '0';
        ext_ref_mul_locked_o  <= '1';
      end generate gen_arria5_no_ext_ref_pll;

      -- not provided by Altera PLL
      ext_ref_mul_stopped_o <= '0';

    end generate gen_arria5_default_plls;

  end generate gen_default_plls;

  -- If external PLLs are used, just copy clock inputs to outputs
  gen_custom_plls : if (g_use_default_plls = FALSE) generate

    clk_62m5_sys_o  <= clk_62m5_sys_i;
    clk_62m5_dmtd_o <= clk_62m5_dmtd_i;
    clk_125m_ref_o  <= clk_125m_ref_i;

    clk_pll_sys <= clk_62m5_sys_i;
    clk_pll_ref <= clk_125m_ref_i;

    pll_locked_o <= clk_sys_locked_i and clk_dmtd_locked_i;

    ext_ref_mul_o         <= clk_125m_ext_i;
    ext_ref_mul_locked_o  <= clk_ext_locked_i;
    ext_ref_mul_stopped_o <= clk_ext_stopped_i;

  end generate gen_custom_plls;

  -- always pass ext reference reset input to output, even when not used
  clk_ext_rst_o <= ext_ref_rst_i;

  -- always pass ext reference clock input to output, even when not used
  clk_10m_ext_o <= clk_10m_ext_i;

  -----------------------------------------------------------------------------
  -- Transceiver PHY
  -----------------------------------------------------------------------------

  gen_arria5_phy : if (g_fpga_family = "arria5") generate

    cmp_phy : wr_arria5_phy
      generic map (
        g_pcs_16bit => g_pcs_16bit)
      port map (
        clk_reconf_i   => clk_pll_ref,
        clk_phy_i      => phy_clk,
        ready_o        => phy_ready,
        loopen_i       => phy_loopen,
        drop_link_i    => phy_rst,
        tx_clk_o       => phy_tx_clk,
        tx_data_i      => phy_tx_data,
        tx_k_i         => phy_tx_k,
        tx_disparity_o => phy_tx_disparity,
        tx_enc_err_o   => phy_tx_enc_err,
        rx_rbclk_o     => phy_rx_rbclk,
        rx_data_o      => phy_rx_data,
        rx_k_o         => phy_rx_k,
        rx_enc_err_o   => phy_rx_enc_err,
        rx_bitslide_o  => phy_rx_bitslide,
        pad_txp_o      => sfp_tx_o,
        pad_rxp_i      => sfp_rx_i);

  end generate gen_arria5_phy;

  gen_pcs_8bit : if (g_pcs_16bit = FALSE) generate

    phy_clk <= clk_pll_ref;

    phy_loopen       <= phy8_i.loopen;
    phy_rst          <= phy8_i.rst;
    phy_tx_data      <= phy8_i.tx_data;
    phy_tx_k         <= phy8_i.tx_k;
    sfp_tx_disable_o <= phy8_i.sfp_tx_disable;

    phy8_o.ref_clk      <= phy_tx_clk;
    phy8_o.tx_disparity <= phy_tx_disparity;
    phy8_o.tx_enc_err   <= phy_tx_enc_err;
    phy8_o.rx_clk       <= phy_rx_rbclk;
    phy8_o.rx_data      <= phy_rx_data;
    phy8_o.rx_k         <= phy_rx_k;
    phy8_o.rx_enc_err   <= phy_rx_enc_err;
    phy8_o.rx_bitslide  <= phy_rx_bitslide;
    phy8_o.rdy          <= phy_ready;
    phy8_o.sfp_tx_fault <= sfp_tx_fault_i;
    phy8_o.sfp_los      <= sfp_los_i;

    phy16_o <= c_dummy_phy16_to_wrc;

  end generate gen_pcs_8bit;

  gen_pcs_16bit : if (g_pcs_16bit = TRUE) generate

    phy_clk <= clk_pll_sys;

    phy_loopen       <= phy16_i.loopen;
    phy_rst          <= phy16_i.rst;
    phy_tx_data      <= phy16_i.tx_data;
    phy_tx_k         <= phy16_i.tx_k;
    sfp_tx_disable_o <= phy16_i.sfp_tx_disable;

    phy16_o.ref_clk      <= phy_tx_clk;
    phy16_o.tx_disparity <= phy_tx_disparity;
    phy16_o.tx_enc_err   <= phy_tx_enc_err;
    phy16_o.rx_clk       <= phy_rx_rbclk;
    phy16_o.rx_data      <= phy_rx_data;
    phy16_o.rx_k         <= phy_rx_k;
    phy16_o.rx_enc_err   <= phy_rx_enc_err;
    phy16_o.rx_bitslide  <= phy_rx_bitslide;
    phy16_o.rdy          <= phy_ready;
    phy16_o.sfp_tx_fault <= sfp_tx_fault_i;
    phy16_o.sfp_los      <= sfp_los_i;

    phy8_o <= c_dummy_phy8_to_wrc;

  end generate gen_pcs_16bit;


end architecture rtl;
