-------------------------------------------------------------------------------
-- Title      : Btrain over White Rabbit
-- Project    : Btrain
-------------------------------------------------------------------------------
-- File       : BtrainFMC_pkg.vhd
-- Author     : Maciej Lipinski
-- Company    : CERN
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
--
-- package to be called where xwr_transmission is used. Important
-- part of the package are the two constants that provide information about 
-- the generic array size - this array is used to provide information about 
-- statistics to the WRPC
--
-------------------------------------------------------------------------------
--
-- Copyright (c) 2016 CERN/BE-CO-HT
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
-- details
--
-- You should have received a copy of the GNU Lesser General
-- Public License along with this source; if not, download it
-- from http://www.gnu.org/licenses/lgpl-2.1.html
--
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2016-05-30  1.0     mlipinsk         Created

library ieee;
use ieee.std_logic_1164.all;
use work.wishbone_pkg.all;  -- needed for t_wishbone_slave_in, etc
use work.streamers_pkg.all; -- needed for streamers
use work.wr_fabric_pkg.all; -- neede for :t_wrf_source_in, etc
use work.wrcore_pkg.all;    -- needed for t_generic_word_array
use work.wr_transmission_wbgen2_pkg.all;

package wr_transmission_pkg is

  constant c_WR_TRANS_ARR_SIZE_OUT : integer := c_STREAMERS_ARR_SIZE_OUT+3;
  constant c_WR_TRANS_ARR_SIZE_IN  : integer := c_STREAMERS_ARR_SIZE_IN;
  
  component xwr_transmission is
    generic (
      g_data_width : integer := 32
      );
    port (
      clk_sys_i : in std_logic;
      rst_n_i   : in std_logic;
      src_i : in  t_wrf_source_in;
      src_o : out t_wrf_source_out;
      snk_i : in  t_wrf_sink_in;
      snk_o : out t_wrf_sink_out;
      tx_data_i : in std_logic_vector(g_data_width-1 downto 0);
      tx_valid_i : in std_logic;
      tx_dreq_o : out std_logic;
      tx_last_p1_i : in std_logic := '1';
      tx_flush_p1_i : in std_logic := '0';
      rx_first_p1_o         : out std_logic;
      rx_last_p1_o          : out std_logic;
      rx_data_o          : out std_logic_vector(g_data_width-1 downto 0);
      rx_valid_o         : out std_logic;
      rx_dreq_i          : in  std_logic;
      clk_ref_i : in std_logic := '0';
      tm_time_valid_i : in std_logic := '0';
      tm_tai_i : in std_logic_vector(39 downto 0) := x"0000000000";
      tm_cycles_i : in std_logic_vector(27 downto 0) := x"0000000";
      wb_slave_i               : in  t_wishbone_slave_in := cc_dummy_slave_in;
      wb_slave_o               : out t_wishbone_slave_out;
      snmp_array_o           : out t_generic_word_array(c_WR_TRANS_ARR_SIZE_OUT-1 downto 0);
      snmp_array_i           : in  t_generic_word_array(c_WR_TRANS_ARR_SIZE_IN -1 downto 0)

      );

  end component;
end package;


