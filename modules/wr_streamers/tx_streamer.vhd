-------------------------------------------------------------------------------
-- Title      : Wrapper for xtx_streamer
-- Project    : WR Streamers
-- URL        : http://www.ohwr.org/projects/wr-cores/wiki/WR_Streamers
-------------------------------------------------------------------------------
-- File       : tx_streamer.vhd
-- Author     : Tomasz Wlostowski <tomasz.wlostowski@cern.ch>
-- Company    : CERN
-- Platform   : FPGA-generics
-- Standard   : VHDL
-- Created    : 2012-10-01
-------------------------------------------------------------------------------
-- Description:
--
-- Wrapper for xtx_streamer that is needed/used for SystemVerilog simulation
-------------------------------------------------------------------------------
--
-- Copyright (c) 2012-2017 CERN/BE-CO-HT
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

use work.wishbone_pkg.all;
use work.wr_fabric_pkg.all;
use work.streamers_pkg.all;

entity tx_streamer is
  
  generic (
    g_data_width             : integer := 32;
    g_tx_buffer_size         : integer := 32;
    g_tx_threshold           : integer := 16;
    g_tx_max_words_per_frame : integer := 128;
    g_tx_timeout             : integer := 1024;
    g_escape_code_disable    : boolean := FALSE;
    g_simulation             : integer := 0;
    g_sim_startup_cnt        : integer := 6250
    );

  port (
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    -- Endpoint/WRC interface 
    src_dat_o   : out std_logic_vector(15 downto 0);
    src_adr_o   : out std_logic_vector(1 downto 0);
    src_sel_o   : out std_logic_vector(1 downto 0);
    src_cyc_o   : out std_logic;
    src_stb_o   : out std_logic;
    src_we_o    : out std_logic;
    src_stall_i : in  std_logic;
    src_ack_i   : in  std_logic;
    src_err_i   : in  std_logic;

    clk_ref_i       : in std_logic                     := '0';
    tm_time_valid_i : in std_logic                     := '0';
    tm_tai_i        : in std_logic_vector(39 downto 0) := x"0000000000";
    tm_cycles_i     : in std_logic_vector(27 downto 0) := x"0000000";

    tx_data_i      : in  std_logic_vector(g_data_width-1 downto 0);
    tx_valid_i     : in  std_logic;
    tx_dreq_o      : out std_logic;
    tx_last_p1_i   : in  std_logic := '1';
    tx_flush_p1_i  : in  std_logic := '0';
    tx_reset_seq_i : in  std_logic := '0';
    tx_frame_p1_o  : out std_logic;

    -- MAC address
    cfg_mac_local_i  : in std_logic_vector(47 downto 0) := x"000000000000";
    cfg_mac_target_i : in std_logic_vector(47 downto 0) := x"ffffffffffff";
    cfg_ethertype_i  : in std_logic_vector(15 downto 0) := x"dbff"
    );

end tx_streamer;

architecture rtl of tx_streamer is

  signal src_in  : t_wrf_source_in;
  signal src_out : t_wrf_source_out;
  signal tx_streamer_cfg: t_tx_streamer_cfg;
  
begin  -- rtl

  U_Wrapped_Streamer : xtx_streamer
    generic map (
      g_data_width             => g_data_width,
      g_tx_buffer_size         => g_tx_buffer_size,
      g_tx_threshold           => g_tx_threshold,
      g_tx_max_words_per_frame => g_tx_max_words_per_frame,
      g_tx_timeout             => g_tx_timeout,
      g_escape_code_disable    => g_escape_code_disable,
      g_simulation             => g_simulation,
      g_sim_startup_cnt        => g_sim_startup_cnt)
    port map (
      clk_sys_i        => clk_sys_i,
      rst_n_i          => rst_n_i,
      src_i            => src_in,
      src_o            => src_out,

      clk_ref_i        => clk_ref_i,
      tm_time_valid_i  => tm_time_valid_i,
      tm_tai_i         => tm_tai_i,
      tm_cycles_i      => tm_cycles_i,

      tx_data_i        => tx_data_i,
      tx_valid_i       => tx_valid_i,
      tx_dreq_o        => tx_dreq_o,
      tx_last_p1_i     => tx_last_p1_i,
      tx_flush_p1_i    => tx_flush_p1_i,
      tx_reset_seq_i   => tx_reset_seq_i,
      tx_frame_p1_o    => tx_frame_p1_o,

      tx_streamer_cfg_i=> tx_streamer_cfg);

  src_adr_o    <= src_out.adr;
  src_dat_o    <= src_out.dat;
  src_sel_o    <= src_out.sel;
  src_stb_o    <= src_out.stb;
  src_we_o     <= src_out.we;
  src_cyc_o    <= src_out.cyc;
  src_in.ack   <= src_ack_i;
  src_in.stall <= src_stall_i;
  src_in.err   <= src_err_i;

  tx_streamer_cfg.mac_local  <= cfg_mac_local_i;
  tx_streamer_cfg.mac_target <= cfg_mac_target_i;
  tx_streamer_cfg.ethertype  <= cfg_ethertype_i;

end rtl;
