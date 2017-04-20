-------------------------------------------------------------------------------
-- Title      : WR Streamers Private Packages
-- Project    : WR Streamers
-------------------------------------------------------------------------------
-- File       : streamers_priv_pkg.vhd
-- Author     : Maciej Lipinski
-- Company    : CERN
-- Platform   : FPGA-generics
-- Standard   : VHDL
-- Created    : 2017-04-20
-------------------------------------------------------------------------------
-- Description:
-- Private package of streamers: all the components/functions used only by
-- streamers, not useful by users/applications
-------------------------------------------------------------------------------
--
-- Copyright (c) 2017 CERN/BE-CO-HT
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
use work.wishbone_pkg.all;  -- needed for t_wishbone_slave_in, etc

package streamers_priv_pkg is

  component xtx_streamers_stats is
    generic (
      g_cnt_width            : integer := 32);
    port (
      clk_i                  : in std_logic;
      rst_n_i                : in std_logic;
      sent_frame_i           : in std_logic;
      reset_stats_i          : in std_logic;
      snapshot_ena_i         : in std_logic := '0';
      sent_frame_cnt_o       : out std_logic_vector(g_cnt_width-1 downto 0));
  end component;

  component xrx_streamers_stats is
    generic (
      g_cnt_width            : integer := 32;
      g_acc_width            : integer := 64);
    port (
      clk_i                  : in std_logic;
      rst_n_i                : in std_logic;
      rcvd_frame_i           : in std_logic;
      lost_block_i           : in std_logic;
      lost_frame_i           : in std_logic;
      lost_frames_cnt_i      : in std_logic_vector(14 downto 0);
      rcvd_latency_i         : in  std_logic_vector(27 downto 0);
      rcvd_latency_valid_i   : in  std_logic;
      tm_time_valid_i        : in  std_logic;
      snapshot_ena_i         : in std_logic := '0';
      reset_stats_i          : in std_logic;
      rcvd_frame_cnt_o       : out std_logic_vector(g_cnt_width-1 downto 0);
      lost_frame_cnt_o       : out std_logic_vector(g_cnt_width-1 downto 0);
      lost_block_cnt_o       : out std_logic_vector(g_cnt_width-1 downto 0);
      latency_cnt_o          : out std_logic_vector(g_cnt_width-1 downto 0);
      latency_acc_overflow_o : out std_logic;
      latency_acc_o          : out std_logic_vector(g_acc_width-1  downto 0);
      latency_max_o          : out std_logic_vector(27  downto 0);
      latency_min_o          : out std_logic_vector(27  downto 0));
    end component;

end streamers_priv_pkg;