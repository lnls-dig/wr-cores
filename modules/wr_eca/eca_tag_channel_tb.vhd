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

  -- Test these edge cases:
  --  size = 1, 2, 3,...
  --  mult = 0, 1, 2
  --  cals = 0, 1, 2
  
  -- g_case size mult cals
  --   0     1    0    0
  --   1     2    0    1
  --   2     3    1    0
  --   3     4    2    2
  
  function f_mult return natural is
  begin
    case g_case is
      when 0 => return 0;
      when 1 => return 0;
      when 2 => return 1;
      when others => return 2;
    end case;
  end f_mult;
  
  function f_cals return natural is
  begin
    case g_case is
      when 0 => return 0;
      when 1 => return 1;
      when 2 => return 0;
      when others => return 2;
    end case;
  end f_cals;
  
  function f_1(x : boolean) return std_logic is
  begin
    if x then return '1'; else return '0'; end if;
  end f_1;
  
  constant c_num_channels   : natural := 2;
  constant c_log_size       : natural := 1 + g_case; -- smaller => tests edge cases better
  constant c_log_multiplier : natural := f_mult;
  constant c_log_calendars  : natural := f_cals;
  constant c_log_latency    : natural := c_log_size + c_log_multiplier + 1 - c_log_calendars;
  constant c_log_max_delay  : natural := c_log_latency+2; -- ensures testing of 'early'
  constant c_ticks          : natural := 2**c_log_multiplier;
  constant c_pipeline_depth : natural := 12;
  
  signal r_time     : t_time    := (others => '0');
  signal r_stall    : std_logic;
  signal r_num      : std_logic_vector(f_eca_log2_min1(c_num_channels)-1 downto 0);
  signal s_num      : std_logic_vector(f_eca_log2_min1(c_num_channels)-1 downto 0);
  signal r_channel  : t_channel := c_idle_channel;
  signal s_channel  : t_channel;
  signal s_overflow : std_logic;
  
  function f_nat(x : std_logic) return natural is
  begin
    if x = '1' then return 1; else return 0; end if;
  end f_nat;
  
begin

  channel : eca_tag_channel
    generic map(
      g_num_channels   => c_num_channels,
      g_log_size       => c_log_size,
      g_log_multiplier => c_log_multiplier,
      g_log_max_delay  => c_log_max_delay,
      g_log_latency    => c_log_latency)
    port map(
      clk_i        => clk_i,
      rst_n_i      => rst_n_i,
      time_i       => r_time,
      channel_i    => r_channel,
      num_i        => r_num,
      stall_i      => r_stall,
      snoop_i      => (others => '0'),
      channel_o    => s_channel,
      num_o        => s_num,
      overflow_o   => s_overflow);

  main : process(rst_n_i, clk_i) is
    type t_nat_array is array(natural range <>) of natural;
    variable busy     : std_logic_vector(2**(c_log_size+1)-1 downto 0) := (others => '0');
    variable early    : std_logic_vector(2**(c_log_size+1)-1 downto 0);
    variable late     : std_logic_vector(2**(c_log_size+1)-1 downto 0);
    variable count    : t_nat_array(2**(c_log_size+1)-1 downto 0);
    variable scan     : natural := 0;
    variable conflict : std_logic_vector(c_num_channels-1 downto 0) := (others => '0');
    
    variable s1, s2 : positive := 42;
    variable valid  : std_logic;
    variable stall  : std_logic;
    variable ignore : std_logic;
    variable num    : std_logic_vector(r_num'range);
    variable event  : t_event;
    variable param  : t_param;
    variable tag    : t_tag;
    variable tef    : t_tef;
    variable time   : t_time;
    variable seq    : t_time;
    variable flags  : natural;
    variable index  : natural;
    
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
      
      p_eca_uniform(s1, s2, stall);
      p_eca_uniform(s1, s2, num);
      p_eca_uniform(s1, s2, event);
      p_eca_uniform(s1, s2, param);
      p_eca_uniform(s1, s2, tag);
      p_eca_uniform(s1, s2, tef);
      p_eca_uniform(s1, s2, time);
      
      -- Only insert an action if the test slot was not taken
      index := to_integer(unsigned(param(c_log_size downto 0)));
      valid := not busy(index);
      
      -- Precalculate if it's ok to be late/early
      time(time'high downto c_log_latency+3) := (others => '0');
      if valid = '1' then
        busy(index)  := '1';
        early(index) := f_1(unsigned(time) >= 2**c_log_max_delay + c_pipeline_depth*c_ticks);
        late(index)  := f_1(unsigned(time) <  2**c_log_latency   + c_pipeline_depth*c_ticks);
        count(index) := 20;
      end if;
      time := f_eca_add(time, r_time);
      
      -- Input rejected due to overflow; reclaim index
      if s_overflow = '1' and r_channel.valid = '1' then
        index := to_integer(unsigned(r_channel.param(c_log_size downto 0)));
        busy(index)  := '0';
      end if;
      
      r_num              <= num;
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
      
      -- All control lines must be valid
      assert (s_channel.valid    xnor s_channel.valid)    = '1' report "Valid meta-value"    severity failure;
      assert (s_channel.delayed  xnor s_channel.delayed)  = '1' report "Delayed meta-value"  severity failure;
      assert (s_channel.conflict xnor s_channel.conflict) = '1' report "Conflict meta-value" severity failure;
      assert (s_channel.late     xnor s_channel.late)     = '1' report "Late meta-value"     severity failure;
      assert (s_channel.early    xnor s_channel.early)    = '1' report "Early meta-value"    severity failure;
      
      -- Only one flag may be set
      flags := f_nat(s_channel.valid) + f_nat(s_channel.delayed) + f_nat(s_channel.conflict) +
               f_nat(s_channel.late)  + f_nat(s_channel.early);
      assert flags <= 1 report "Too many control lines set" severity failure;
      
      -- Check that anything being output matches what we put in
      if (r_stall = '0' or s_channel.valid = '0') and flags > 0 then
        index := to_integer(unsigned(s_channel.param(c_log_size downto 0)));
        assert busy(index) = '1' report "An invalid action was output" severity failure;
        assert s_channel.late  = '0' or late(index)  = '1' report "An action was late that should not be"  severity failure;
        assert s_channel.early = '0' or early(index) = '1' report "An action was early that should not be" severity failure;
        busy(index) := '0';
      end if;
      
      -- On time; timestamp must fall within one clock tick
      if s_channel.valid = '1' and ignore = '0' then
        assert unsigned(r_time) <= unsigned(s_channel.time)        report "Valid too late"  severity failure;
        assert unsigned(s_channel.time) < unsigned(r_time)+c_ticks report "Valid too early" severity failure;
      end if;
      
      -- At worst delayed; times increase strictly monotone
      if (s_channel.valid or s_channel.delayed) = '1' and ignore = '0' then
        assert unsigned(s_channel.time) >= unsigned(seq) report "Action timestamps not monotonic" severity failure;
        if unsigned(s_channel.time) > unsigned(seq) then
          conflict := (others => '0');
        else
          assert conflict(to_integer(unsigned(s_num))) = '0' report "No conflict despite prior match" severity failure;
        end if;
        conflict(to_integer(unsigned(s_num))) := '1';
        seq := s_channel.time;
      end if;
      
      -- Conflicts must have the same timestamp as the last action
      if s_channel.conflict = '1' then
        assert unsigned(s_channel.time) = unsigned(seq) report "Conflict does not have same timestamp" severity failure;
        assert conflict(to_integer(unsigned(s_num))) = '1' report "Conflict with nothing?" severity failure;
      end if;
      
      -- If the channel claims this is late, it should be late!
      if (s_channel.delayed or s_channel.late) = '1' then
        assert unsigned(r_time) > unsigned(s_channel.time) report "Late action was early" severity failure;
      end if;
      
      -- If the channel claims this is early, it should be early!
      -- ... except it is possible that even though it was early, a super-long stall made it late ;)
      if s_channel.early = '1' then
        -- assert unsigned(s_channel.time) >= unsigned(r_time)+c_ticks report "Early action was late (this is possible if a long enough stall happens before)" severity warning;
      end if;
      
      -- Confirm function of stall line
      assert ignore = '0' or s_channel.valid = '1' report "Valid went low while stalled" severity failure;
      ignore := s_channel.valid and r_stall;
      
      -- Check to see that events actually leave
      index := index + 1;
      if index = 2**(c_log_size+1) then index := 0; end if;
      if busy(index) = '1' then
        if count(index) = 0 then
          report "Action " & integer'image(index) & " has been unexecuted for a long time ..." severity warning;
        else
          count(index) := count(index) - 1;
        end if;
      end if;
      
    end if;
  end process;

end rtl;
