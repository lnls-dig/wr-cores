-------------------------------------------------------------------------------
-- Title      : WR Streamers statistics
-- Project    : White Rabbit Streamers
-------------------------------------------------------------------------------
-- File       : xrtx_streamers_stats.vhd
-- Author     : Maciej Lipinski
-- Company    : CERN
-- Created    : 2016-06-08
-- Last update: 2016-06-12
-- Platform   : FPGA-generics
-- Standard   : VHDL
-------------------------------------------------------------------------------
-- Description:
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
-- details.                                                     
--
-- You should have received a copy of the GNU Lesser General    
-- Public License along with this source; if not, download it   
-- from http://www.gnu.org/licenses/lgpl-2.1.html
--
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2016-06-08  1.0      mlipinsk        created
-- 2016-06-12  1.1      mlipinsk        added generic word arrays for SNMP
---------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.wishbone_pkg.all;  -- needed for t_wishbone_slave_in, etc
use work.streamers_pkg.all; -- needed for streamers
use work.wr_fabric_pkg.all; -- neede for :t_wrf_source_in, etc
use work.wrcore_pkg.all;    -- needed for t_generic_word_array
use work.wr_transmission_wbgen2_pkg.all;

entity xrtx_streamers_stats is
  
  generic (
    -- Width of frame counters
    g_cnt_width            : integer := 32; -- minimum 15 bits, max 32
    g_acc_width            : integer := 64  -- max value 64
    );
  port (
    clk_i                  : in std_logic;
    rst_n_i                : in std_logic;

    -- input signals from streamers
    sent_frame_i           : in std_logic;
    rcvd_frame_i           : in std_logic;
    lost_block_i           : in std_logic;
    lost_frame_i           : in std_logic;
    lost_frames_cnt_i      : in std_logic_vector(14 downto 0);
    rcvd_latency_i         : in  std_logic_vector(27 downto 0);
    rcvd_latency_valid_i   : in  std_logic;
    tm_time_valid_i        : in std_logic := '0';
    tm_tai_i               : in std_logic_vector(39 downto 0) := x"0000000000";
    tm_cycles_i            : in std_logic_vector(27 downto 0) := x"0000000";

    -- statistic control
    reset_stats_i          : in std_logic;
    ----------------------- statistics ----------------------------------------
    -- output statistics: time of last reset of statistics
    reset_time_tai_o       : out std_logic_vector(39 downto 0) := x"0000000000";
    reset_time_cycles_o    : out std_logic_vector(27 downto 0) := x"0000000";
    -- output statistics: tx/rx counters
    sent_frame_cnt_o       : out std_logic_vector(g_cnt_width-1 downto 0);
    rcvd_frame_cnt_o       : out std_logic_vector(g_cnt_width-1 downto 0);
    lost_frame_cnt_o       : out std_logic_vector(g_cnt_width-1 downto 0);
    lost_block_cnt_o       : out std_logic_vector(g_cnt_width-1 downto 0);
    -- output statistics: latency
    latency_cnt_o          : out std_logic_vector(g_cnt_width-1 downto 0);
    latency_acc_overflow_o : out std_logic;
    latency_acc_o          : out std_logic_vector(g_acc_width-1  downto 0);
    latency_max_o          : out std_logic_vector(27  downto 0);
    latency_min_o          : out std_logic_vector(27  downto 0);

    snmp_array_o           : out t_generic_word_array(c_STREAMERS_ARR_SIZE_OUT-1 downto 0);
    snmp_array_i           : in  t_generic_word_array(c_STREAMERS_ARR_SIZE_IN -1 downto 0)
    );

end xrtx_streamers_stats;
  
architecture rtl of xrtx_streamers_stats is
  
  signal reset_time_tai    : std_logic_vector(39 downto 0);
  signal reset_time_cycles : std_logic_vector(27 downto 0);

  signal sent_frame_cnt    : unsigned(g_cnt_width-1  downto 0);
  signal rcvd_frame_cnt    : unsigned(g_cnt_width-1  downto 0);
  signal lost_frame_cnt    : unsigned(g_cnt_width-1  downto 0);
  signal lost_block_cnt    : unsigned(g_cnt_width-1  downto 0);
  signal latency_cnt       : unsigned(g_cnt_width-1  downto 0);

  signal latency_max       : std_logic_vector(27  downto 0);
  signal latency_min       : std_logic_vector(27  downto 0);
  signal latency_acc       : unsigned(g_acc_width-1+1  downto 0);
  signal latency_acc_overflow: std_logic;

  signal reset_stats_remote: std_logic;
  -- for code cleanness
  constant cw              : integer := g_cnt_width;
  constant aw              : integer := g_acc_width;
begin

  -- process that timestamps the reset so that we can make statistics over time
  p_timestamp_reset: process(clk_i)
  begin
    if rising_edge(clk_i) then
      if (rst_n_i = '0') then
        reset_time_tai       <= (others => '0');
        reset_time_cycles    <= (others => '0');
      else
        if(reset_stats_i = '1'   and tm_time_valid_i = '1') then -- initial timestamp after restart
          reset_time_tai     <= tm_tai_i;
          reset_time_cycles  <= tm_cycles_i;
        end if;
      end if;
    end if;
  end process;

  reset_time_tai_o    <= reset_time_tai;
  reset_time_cycles_o <= reset_time_cycles;

  -- process that counts stuff: receved/send/lost frames
  p_cnts: process(clk_i)
  begin
    if rising_edge(clk_i) then
      if (rst_n_i = '0' or reset_stats_i = '1') then
        sent_frame_cnt        <= (others => '0');
        rcvd_frame_cnt        <= (others => '0');
        lost_frame_cnt        <= (others => '0');
        lost_block_cnt        <= (others => '0');
      else
        -- count sent frames
        if(sent_frame_i = '1') then
          sent_frame_cnt <= sent_frame_cnt + 1;
        end if;
        -- count received frames
        if(rcvd_frame_i = '1') then
          rcvd_frame_cnt <= rcvd_frame_cnt + 1;
        end if;
        -- count lost frames
        if(lost_frame_i = '1') then
          lost_frame_cnt <= lost_frame_cnt + resize(unsigned(lost_frames_cnt_i),lost_frame_cnt'length);
        end if;
        -- count lost blocks
        if(lost_block_i = '1') then
          lost_block_cnt <= lost_block_cnt + 1;
        end if;
      end if;
    end if;
  end process;

  sent_frame_cnt_o       <= std_logic_vector(sent_frame_cnt);
  rcvd_frame_cnt_o       <= std_logic_vector(rcvd_frame_cnt);
  lost_frame_cnt_o       <= std_logic_vector(lost_frame_cnt);
  lost_block_cnt_o       <= std_logic_vector(lost_block_cnt);

  p_latency_stats: process(clk_i)
  begin
    if rising_edge(clk_i) then
      if (rst_n_i = '0' or reset_stats_i = '1') then
        latency_max            <= (others => '0');
        latency_min            <= (others => '1');
        latency_acc            <= (others => '0');
        latency_cnt            <= (others => '0');
        latency_acc_overflow   <= '0';
      else
        if(rcvd_latency_valid_i = '1' and tm_time_valid_i = '1') then
          if(latency_max < rcvd_latency_i) then
            latency_max <= rcvd_latency_i;
          end if;
          if(latency_min > rcvd_latency_i) then
            latency_min <= rcvd_latency_i;
          end if;
          if(latency_acc(g_acc_width) ='1') then
            latency_acc_overflow   <= '1';
          end if;
          latency_cnt <= latency_cnt + 1;
          latency_acc <= latency_acc + resize(unsigned(rcvd_latency_i),latency_acc'length);
        end if;
      end if;
    end if;
  end process;
  
  latency_max_o      <= latency_max;
  latency_min_o      <= latency_min;
  latency_acc_o      <= std_logic_vector(latency_acc(g_acc_width-1 downto 0));
  latency_cnt_o      <= std_logic_vector(latency_cnt);
  latency_acc_overflow_o <= latency_acc_overflow;

  -- Generic communication with WRPC that allows SNMP access via generic array of 32-bits 
  -- std_logic_vectors
  assert (cw <= 32) 
    report "g_cnt_width value not suppported by f_pack_streamers_statistics" severity error;
  assert (aw <= 64) 
    report "g_cnt_width value not suppported by f_pack_streamers_statistics" severity error;

  reset_stats_remote                <= snmp_array_i(0)(0);

  snmp_array_o(0)(             0)   <= latency_acc_overflow;          -- status bit
  snmp_array_o(0)(   31 downto 1)   <= (others => '0');
  snmp_array_o(1)(   31 downto 0)   <= x"0" & reset_time_cycles( 27 downto 0);
  snmp_array_o(2)(   31 downto 0)   <= reset_time_tai(    31 downto 0);
  snmp_array_o(3)(   31 downto 0)   <= x"000000" & reset_time_tai(    39 downto 32);

  snmp_array_o(4 )(cw-1 downto 0)   <= std_logic_vector(sent_frame_cnt(cw-1 downto 0));
  snmp_array_o(4 )(31   downto cw)  <= (others => '0');
  snmp_array_o(5 )(cw-1 downto 0)   <= std_logic_vector(rcvd_frame_cnt(cw-1 downto 0));
  snmp_array_o(4 )(31   downto cw)  <= (others => '0');
  snmp_array_o(6 )(cw-1 downto 0)   <= std_logic_vector(lost_frame_cnt(cw-1 downto 0));
  snmp_array_o(4 )(31   downto cw)  <= (others => '0');
  snmp_array_o(7 )(cw-1 downto 0)   <= std_logic_vector(lost_block_cnt(cw-1 downto 0));
  snmp_array_o(4 )(31   downto cw)  <= (others => '0');
  snmp_array_o(8 )(cw-1 downto 0)   <= std_logic_vector(latency_cnt(   cw-1 downto 0));
  snmp_array_o(4 )(31   downto cw)  <= (others => '0');
  
  snmp_array_o(9 )(31 downto 0)     <= x"0" & latency_max(27 downto 0);
  snmp_array_o(10)(31 downto 0)     <= x"0" & latency_min(27 downto 0);
 
  CNT_SINGLE_WORD_gen: if(aw < 33) generate
    snmp_array_o(11)(aw-1    downto  0) <= std_logic_vector(latency_acc(aw-1 downto 0));
    snmp_array_o(11)(31      downto aw) <= std_logic_vector(latency_acc(aw-1 downto 0));
  end generate;
  CNT_TWO_WORDs_gen:   if(aw > 32) generate
    snmp_array_o(11)(31      downto 0)     <= std_logic_vector(latency_acc(31    downto 0));
    snmp_array_o(12)(aw-32-1 downto 0)     <= std_logic_vector(latency_acc(aw-1 downto 32));
    snmp_array_o(12)(31      downto aw-32) <= (others => '0'); 
  end generate;

end rtl;