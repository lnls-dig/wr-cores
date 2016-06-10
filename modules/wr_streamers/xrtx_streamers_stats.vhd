-------------------------------------------------------------------------------
-- Title      : WR Streamers statistics
-- Project    : White Rabbit Streamers
-------------------------------------------------------------------------------
-- File       : xrtx_streamers_stats.vhd
-- Author     : Maciej Lipinski
-- Company    : CERN
-- Created    : 2016-06-08
-- Last update: 2016-06-08
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
---------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.wishbone_pkg.all;  -- needed for t_wishbone_slave_in, etc
use work.streamers_pkg.all; -- needed for streamers
use work.wr_fabric_pkg.all; -- neede for :t_wrf_source_in, etc
use work.wr_transmission_wbgen2_pkg.all;

entity xrtx_streamers_stats is
  
  generic (
    -- Width of frame counters
    g_cnt_width            : integer := 32; -- minimum 15 bits
    g_acc_width            : integer := 64
    );
  port (
    clk_i                  : in std_logic;
    rst_n_i                : in std_logic;

    -- input signals from streamers
    sent_frame_i           : in std_logic;
    rcvd_frame_i           : in std_logic;
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
    -- output statistics: latency
    latency_cnt_o          : out std_logic_vector(g_cnt_width-1 downto 0);
    latency_acc_overflow_o : out std_logic;
    latency_acc_o          : out std_logic_vector(g_acc_width-1  downto 0);
    latency_max_o          : out std_logic_vector(27  downto 0);
    latency_min_o          : out std_logic_vector(27  downto 0)
    );

end xrtx_streamers_stats;
  
architecture rtl of xrtx_streamers_stats is
  signal sent_frame_cnt    : unsigned(g_cnt_width-1  downto 0);
  signal rcvd_frame_cnt    : unsigned(g_cnt_width-1  downto 0);
  signal lost_frame_cnt    : unsigned(g_cnt_width-1  downto 0);
  signal latency_cnt       : unsigned(g_cnt_width-1  downto 0);
  
  signal latency_max       : std_logic_vector(27  downto 0);
  signal latency_min       : std_logic_vector(27  downto 0);
  signal latency_acc       : unsigned(g_acc_width-1+1  downto 0);
begin

  -- process that timestamps the reset so that we can make statistics over time
  p_timestamp_reset: process(clk_i)
  begin
    if rising_edge(clk_i) then
      if (rst_n_i = '0') then
        reset_time_tai_o      <= (others => '0');
        reset_time_cycles_o   <= (others => '0');
      else
        if(reset_stats_i = '1'   and tm_time_valid_i = '1') then -- initial timestamp after restart
          reset_time_tai_o    <= tm_tai_i;
          reset_time_cycles_o <= tm_cycles_i;
        end if;
      end if;
    end if;
  end process;

  -- process that counts stuff: receved/send/lost frames
  p_cnts: process(clk_i)
  begin
    if rising_edge(clk_i) then
      if (rst_n_i = '0' or reset_stats_i = '1') then
        sent_frame_cnt        <= (others => '0');
        rcvd_frame_cnt        <= (others => '0');
        lost_frame_cnt        <= (others => '0');
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
      end if;
    end if;
  end process;
  sent_frame_cnt_o       <= std_logic_vector(sent_frame_cnt);
  rcvd_frame_cnt_o       <= std_logic_vector(rcvd_frame_cnt);
  lost_frame_cnt_o       <= std_logic_vector(lost_frame_cnt);

  p_latency_stats: process(clk_i)
  begin
    if rising_edge(clk_i) then
      if (rst_n_i = '0' or reset_stats_i = '1') then
        latency_max            <= (others => '0');
        latency_min            <= (others => '1');
        latency_acc            <= (others => '0');
        latency_cnt            <= (others => '0');
        latency_acc_overflow_o <= '0';
      else
        if(rcvd_latency_valid_i = '1' and tm_time_valid_i = '1') then
          if(latency_max < rcvd_latency_i) then
            latency_max <= rcvd_latency_i;
          end if;
          if(latency_min > rcvd_latency_i) then
            latency_min <= rcvd_latency_i;
          end if;
          if(latency_acc(g_acc_width) ='1') then
            latency_acc_overflow_o <= '1';
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
end rtl;