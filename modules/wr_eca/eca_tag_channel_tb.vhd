--! @file eca_tag_channel_tb.vhd
--! @brief ECA tag channel testbench
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2015 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This core tests the behaviour of the eca_tag_channel unit
--!
--! This library is free software; you can redistribute it and/or
--! modify it under the terms of the GNU Lesser General Public
--! License as published by the Free Software Foundation; either
--! version 3 of the License, or (at your option) any later version.
--!
--! This library is distributed in the hope that it will be useful,
--! but WITHOUT ANY WARRANTY; without even the implied warranty of
--! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--! Lesser General Public License for more details.
--!  
--! You should have received a copy of the GNU Lesser General Public
--! License along with this library. If not, see <http://www.gnu.org/licenses/>.
---------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.eca_internals_pkg.all;

entity eca_tag_channel_tb is
  generic(
    g_case : natural);
  port(
    clk_i   : in std_logic;
    rst_n_i : in std_logic);
end eca_tag_channel_tb;

architecture rtl of eca_tag_channel_tb is

  constant c_log_size       : natural := 1 + g_case; -- smaller => tests edge cases better
  constant c_log_multiplier : natural := g_case mod 3;
  constant c_log_latency    : natural := c_log_size + c_log_multiplier; -- !!! test: +1,0,-1
  constant c_log_max_delay  : natural := c_log_latency+2;
  constant c_ticks          : natural := 2**c_log_multiplier;
  
  signal r_time    : t_time    := (others => '0');
  signal r_stall   : std_logic;
  signal r_channel : t_channel := c_idle_channel;
  signal s_channel : t_channel;
  
begin

  channel : eca_tag_channel
    generic map(
      g_log_size       => c_log_size,
      g_log_multiplier => c_log_multiplier,
      g_log_max_delay  => c_log_max_delay,
      g_log_latency    => c_log_latency)
    port map(
      clk_i        => clk_i,
      rst_n_i      => rst_n_i,
      time_i       => r_time,
      channel_i    => r_channel,
      stall_i      => r_stall,
      channel_o    => s_channel,
      overflow_o   => open);

  main : process(rst_n_i, clk_i) is
    type t_times is array(natural range <>) of t_time;
    
    variable s1, s2 : positive := 42;
    variable valid  : std_logic;
    variable stall  : std_logic;
    variable ignore : std_logic;
    variable event  : t_event;
    variable param  : t_param;
    variable tag    : t_tag;
    variable tef    : t_tef;
    variable time   : t_time;
    variable seq    : t_time;
    
  begin
    if rst_n_i = '0' then
      r_time <= (others => '0');
      -- r_time(r_time'high) <= '1'; -- test for overflow
      r_channel <= c_idle_channel;
      r_stall   <= '0';
      seq := (others => '0');
      ignore := '0';
    elsif rising_edge(clk_i) then
      r_time <= f_eca_add(r_time, 2**c_log_multiplier);
      
      p_eca_uniform(s1, s2, valid);
      p_eca_uniform(s1, s2, stall);
      p_eca_uniform(s1, s2, event);
      p_eca_uniform(s1, s2, param);
      p_eca_uniform(s1, s2, tag);
      p_eca_uniform(s1, s2, tef);
      p_eca_uniform(s1, s2, time);
      
      time(time'high downto c_log_latency+3) := (others => '0');
      time := f_eca_add(time, r_time);
      
      r_channel.valid    <= valid;
      r_channel.delayed  <= '0';
      r_channel.conflict <= '0';
      r_channel.late     <= '0';
      r_channel.early    <= '0';
      r_channel.event    <= event;
      r_channel.param    <= param;
      r_channel.tag      <= tag;
      r_channel.tef      <= tef;
      r_channel.time     <= time;
      r_stall <= stall;
      
      -- On time; timestamp must fall within one clock tick
      if s_channel.valid = '1' and ignore = '0' then
        assert unsigned(r_time) <= unsigned(s_channel.time)        report "Valid too late"  severity failure;
        assert unsigned(s_channel.time) < unsigned(r_time)+c_ticks report "Valid too early" severity failure;
      end if;
      
      -- At worst delayed; times increase strictly monotone
      if (s_channel.valid or s_channel.delayed) = '1' and ignore = '0' then
        assert unsigned(s_channel.time) > unsigned(seq) report "Action timestamps not monotonic" severity failure;
        seq := s_channel.time;
      end if;
      
      -- Conflicts must have the same timestamp as the last action
      if s_channel.conflict = '1' then
        assert unsigned(s_channel.time) = unsigned(seq) report "Conflict does not have same timestamp" severity failure;
      end if;
      
      -- If the channel claims this is late, it should be late!
      if (s_channel.delayed or s_channel.late) = '1' then
        assert unsigned(r_time) > unsigned(s_channel.time) report "Late action was early" severity failure;
      end if;
      
      -- If the channel claims this is early, it should be early!
      if s_channel.early = '1' then
        assert unsigned(s_channel.time) >= unsigned(r_time)+c_ticks report "Early action was late" severity failure;
      end if;
      
      -- Confirm function of stall line
      assert ignore = '0' or s_channel.valid = '1' report "Valid went low while stalled" severity failure;
      ignore := s_channel.valid and r_stall;
      
      -- !!! include a way to ensure actions come out exactly once
      
    end if;
  end process;

end rtl;
