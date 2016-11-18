-------------------------------------------------------------------------------
-- Title      : Deterministic Altera PHY wrapper - Arria 5
-- Project    : WR PTP Core
-- URL        : http://www.ohwr.org/projects/wr-cores/wiki/Wrpc_core
-------------------------------------------------------------------------------
-- File       : wr_arria5_phy.vhd
-- Authors    : Wesley W. Terpstra
--              Dimitrios lampridis
-- Company    : GSI, CERN
-- Created    : 2013-05-14
-- Last update: 2017-02-01
-- Standard   : VHDL'93
-------------------------------------------------------------------------------
-- Description: Single channel wrapper for deterministic PHY
-------------------------------------------------------------------------------
--
-- Copyright (c) 2013 GSI / Wesley W. Terpstra
-- Copyright (c) 2016-2017 CERN
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
--
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.endpoint_pkg.all;
use work.wr_altera_pkg.all;
use work.gencores_pkg.all;

entity wr_arria5_phy is
  generic (
    g_pcs_16bit : boolean := FALSE);
  port (
    -- Clock for PHY reconfiguration core. Must be between 75MHz and 125MHz.
    clk_reconf_i   : in  std_logic;
    -- Clock for PHY. 125MHz when PCS is 8bit, 62.5MHz when PCS is 16bit.
    clk_phy_i      : in  std_logic;
    -- PLL locked, RX/TX ready, PHY reconfiguration complete
    ready_o        : out std_logic;
    -- Enable local loopback (Tx->Rx)
    loopen_i       : in  std_logic;
    -- Drop the link
    drop_link_i    : in  std_logic;
    -- Copy of clock used for TX data
    tx_clk_o       : out std_logic;
    -- Data to transmit (8/16 bits, not 8b10b-encoded)
    tx_data_i      : in  std_logic_vector(f_pcs_data_width(g_pcs_16bit)-1 downto 0);
    -- 1 when tx_data_i contains a control code, 0 when it's a data byte
    tx_k_i         : in  std_logic_vector(f_pcs_k_width(g_pcs_16bit)-1 downto 0);
    -- disparity of the currently transmitted 8b10b code (1 = plus, 0 = minus).
    tx_disparity_o : out std_logic;
    -- TX encoding error
    tx_enc_err_o   : out std_logic;
    -- RX recovered clock
    rx_rbclk_o     : out std_logic;
    -- Received data (8/16 bits, not 8b10b-encoded)
    rx_data_o      : out std_logic_vector(f_pcs_data_width(g_pcs_16bit)-1 downto 0);
    -- 1 when the byte on rx_data_o is a control code
    rx_k_o         : out std_logic_vector(f_pcs_k_width(g_pcs_16bit)-1 downto 0);
    -- RX encoding error
    rx_enc_err_o   : out std_logic;
    -- RX bitslide indication, indicating the delay of the RX path of the
    -- transceiver (in UIs). Must be valid when rx_data_o is valid.
    rx_bitslide_o  : out std_logic_vector(f_pcs_bts_width(g_pcs_16bit)-1 downto 0);
    -- Transceiver serial data I/O
    pad_txp_o      : out std_logic;
    pad_rxp_i      : in  std_logic := '0');
end wr_arria5_phy;

architecture rtl of wr_arria5_phy is

  signal rx_rbclk      : std_logic;
  signal pll_locked    : std_logic;
  signal rx_ready      : std_logic;
  signal tx_ready      : std_logic;
  signal phy_ready     : std_logic;
  signal reconfig_busy : std_logic;

  signal rx_disperr   : std_logic_vector(f_pcs_k_width(g_pcs_16bit)-1 downto 0);
  signal rx_errdetect : std_logic_vector(f_pcs_k_width(g_pcs_16bit)-1 downto 0);
  signal rx_bitslide  : std_logic_vector(4 downto 0);

  signal xcvr_to_reconfig : std_logic_vector(91 downto 0);
  signal reconfig_to_xcvr : std_logic_vector(139 downto 0);

  signal rst_reconf     : std_logic;
  signal rst_reconf_ext : std_logic;

begin

  -- management reset sync and pulse extension
  -- should be at least 2 mgmt_clk cycles according to Altera PHY docs
  mgmt_rst_sync : gc_sync_ffs
    port map (
      clk_i    => clk_reconf_i,
      rst_n_i  => '1',
      data_i   => drop_link_i,
      synced_o => rst_reconf);

  mgmt_rst_extend_pulse : gc_extend_pulse
    generic map (
      g_width => 4)
    port map (
      clk_i      => clk_reconf_i,
      rst_n_i    => '1',
      pulse_i    => rst_reconf,
      extended_o => rst_reconf_ext);

  -- Altera PHY calibration block
  U_Reconf : arria5_phy_reconf
    port map (
      reconfig_busy             => reconfig_busy,
      mgmt_clk_clk              => clk_reconf_i,
      mgmt_rst_reset            => rst_reconf_ext,
      reconfig_mgmt_address     => (others => '0'),
      reconfig_mgmt_read        => '0',
      reconfig_mgmt_readdata    => open,
      reconfig_mgmt_waitrequest => open,
      reconfig_mgmt_write       => '0',
      reconfig_mgmt_writedata   => (others => '0'),
      reconfig_to_xcvr          => reconfig_to_xcvr,
      reconfig_from_xcvr        => xcvr_to_reconfig);

  -- Altera PHY with 8bit PCS
  gen_arria5_phy8 : if (g_pcs_16bit = FALSE) generate
    U_The_PHY : arria5_phy8
      port map (
        phy_mgmt_clk                => clk_reconf_i,
        phy_mgmt_clk_reset          => rst_reconf_ext,
        phy_mgmt_address            => "010000101",  -- 0x085
        phy_mgmt_read               => '0',
        phy_mgmt_readdata           => open,
        phy_mgmt_waitrequest        => open,
        phy_mgmt_write              => '1',
        phy_mgmt_writedata          => (0 => '1', others => '0'),
        tx_ready                    => tx_ready,
        rx_ready                    => rx_ready,
        pll_ref_clk(0)              => clk_phy_i,
        tx_serial_data(0)           => pad_txp_o,
        tx_bitslipboundaryselect    => (others => '0'),
        pll_locked(0)               => pll_locked,
        rx_serial_data(0)           => pad_rxp_i,
        rx_runningdisp              => open,
        rx_disperr                  => rx_disperr,
        rx_errdetect                => rx_errdetect,
        rx_bitslipboundaryselectout => rx_bitslide,
        tx_clkout(0)                => tx_clk_o,
        rx_clkout(0)                => rx_rbclk,
        tx_parallel_data            => tx_data_i,
        tx_datak                    => tx_k_i,
        rx_parallel_data            => rx_data_o,
        rx_datak                    => rx_k_o,
        reconfig_from_xcvr          => xcvr_to_reconfig,
        reconfig_to_xcvr            => reconfig_to_xcvr);

    rx_enc_err_o <= rx_disperr(0) or rx_errdetect(0);
  end generate gen_arria5_phy8;

  -- Altera PHY with 16bit PCS
  gen_arria5_phy16 : if (g_pcs_16bit = TRUE) generate
    U_The_PHY : arria5_phy16
      port map (
        phy_mgmt_clk                => clk_reconf_i,
        phy_mgmt_clk_reset          => rst_reconf_ext,
        phy_mgmt_address            => "010000101",  -- 0x085
        phy_mgmt_read               => '0',
        phy_mgmt_readdata           => open,
        phy_mgmt_waitrequest        => open,
        phy_mgmt_write              => '1',
        phy_mgmt_writedata          => (0 => '1', others => '0'),
        tx_ready                    => tx_ready,
        rx_ready                    => rx_ready,
        pll_ref_clk(0)              => clk_phy_i,
        tx_serial_data(0)           => pad_txp_o,
        tx_bitslipboundaryselect    => (others => '0'),
        pll_locked(0)               => pll_locked,
        rx_serial_data(0)           => pad_rxp_i,
        rx_runningdisp              => open,
        rx_disperr                  => rx_disperr,
        rx_errdetect                => rx_errdetect,
        rx_bitslipboundaryselectout => rx_bitslide,
        tx_clkout(0)                => tx_clk_o,
        rx_clkout(0)                => rx_rbclk,
        tx_parallel_data            => tx_data_i,
        tx_datak                    => tx_k_i,
        rx_parallel_data            => rx_data_o,
        rx_datak                    => rx_k_o,
        reconfig_from_xcvr          => xcvr_to_reconfig,
        reconfig_to_xcvr            => reconfig_to_xcvr);

    rx_enc_err_o <= rx_disperr(0) or rx_errdetect(0) or
                    rx_disperr(1) or rx_errdetect(1);
  end generate gen_arria5_phy16;

  rx_rbclk_o <= rx_rbclk;

  phy_ready <= pll_locked and tx_ready and rx_ready and not reconfig_busy;

  -- synchronize ready to rx clock
  cmp_gc_sync_ffs_phy_ready : gc_sync_ffs
    port map (
      clk_i    => rx_rbclk,
      rst_n_i  => '1',
      data_i   => phy_ready,
      synced_o => ready_o);

  rx_bitslide_o <= rx_bitslide(f_pcs_bts_width(g_pcs_16bit)-1 downto 0);

  -- [TODO] DL: not sure how to get these yet
  tx_disparity_o <= '0';
  tx_enc_err_o   <= '0';

end rtl;
