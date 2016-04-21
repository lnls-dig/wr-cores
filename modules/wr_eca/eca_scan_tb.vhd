--! @file eca_scan_tb.vhd
--! @brief ECA channel scan test code
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2015 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This core tests the behaviour of the eca_scan unit
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

entity eca_scan_tb is
  generic(
    g_case : natural);
  port(
    clk_i   : in std_logic;
    rst_n_i : in std_logic);
end eca_scan_tb;

architecture rtl of eca_scan_tb is

  constant c_ext_size       : natural := 3;
  constant c_log_size       : natural := 1 + g_case; -- smaller => tests edge cases better
  constant c_log_multiplier : natural := g_case mod 3;
  constant c_log_latency    : natural := c_log_size + c_log_multiplier + 1;
  constant c_log_max_delay  : natural := 32;
  
  signal r_time     : t_time    := (others => '0');
  signal r_wen      : std_logic := '0';
  signal s_stall    : std_logic;
  signal r_deadline : t_time;
  signal r_idx      : std_logic_vector(c_log_size-1 downto 0);
  signal r_ext      : std_logic_vector(c_ext_size-1 downto 0);
  signal s_stb      : std_logic;
  signal r_stb1     : std_logic;
  signal s_late     : std_logic;
  signal s_early    : std_logic;
  signal s_idx      : std_logic_vector(c_log_size   -1 downto 0);
  signal r_idx1     : std_logic_vector(c_log_size   -1 downto 0);
  signal s_low      : std_logic_vector(c_log_latency-1 downto 0);
  
begin

  scan : eca_scan
    generic map(
      g_ext_size       => c_ext_size,
      g_log_size       => c_log_size,
      g_log_multiplier => c_log_multiplier,
      g_log_max_delay  => c_log_max_delay,
      g_log_latency    => c_log_latency)
    port map(
      clk_i        => clk_i,
      rst_n_i      => rst_n_i,
      time_i       => r_time,
      wen_i        => r_wen,
      stall_o      => s_stall,
      deadline_i   => r_deadline,
      idx_i        => r_idx,
      ext_i        => r_ext,
      scan_stb_o   => s_stb,
      scan_late_o  => s_late,
      scan_early_o => s_early,
      scan_low_o   => s_low,
      scan_idx_o   => s_idx,
      scan_ext_o   => open);

  main : process(rst_n_i, clk_i) is
    type t_times is array(natural range <>) of t_time;
    
    variable s1, s2   : positive := 42;
    variable wen      : std_logic;
    variable index    : std_logic_vector(c_log_size-1 downto 0);
    variable offset   : std_logic_vector(c_log_latency downto 0);
    variable deadline : t_time;
    variable idx      : integer;
    variable ext      : std_logic_vector(c_ext_size-1 downto 0);
    variable deadlines: t_times(2**c_log_size-1 downto 0) := (others => (others => '1'));
  begin
    if rst_n_i = '0' then
      r_wen  <= '0';
      r_time <= (others => '0');
      r_stb1 <= '0';
      -- r_time(r_time'high) <= '1'; -- test for overflow
    elsif rising_edge(clk_i) then
      p_eca_uniform(s1, s2, offset);
      p_eca_uniform(s1, s2, ext);
      p_eca_uniform(s1, s2, index);
      
      -- Record deadline
      if r_wen = '1' and s_stall = '0' then
        deadlines(to_integer(unsigned(r_idx))) := r_deadline;
      end if;
      
      deadline := f_eca_add(f_eca_add(r_time, offset), 2**c_log_latency + 6*(2**c_log_multiplier));
      wen := f_eca_and(deadlines(to_integer(unsigned(index))));
      r_time     <= f_eca_add(r_time, 2**c_log_multiplier);
      r_wen      <= wen;
      r_deadline <= deadline;
      r_idx      <= index;
      r_ext      <= ext;
      
      -- Delay by one cycle
      r_stb1 <= s_stb;
      r_idx1 <= s_idx;
      
      -- Free and check the record
      if r_stb1 = '1' then
        idx := to_integer(unsigned(r_idx1));
        
        assert deadlines(idx)(s_low'range) = s_low report "Wrong low bits in deadline" severity error;
        assert s_early='0' report "Early should be impossible in this test" severity error;
        assert s_late ='0' report "Late should be impossible in this test"  severity error;
        
        assert s_late ='1' or unsigned(deadlines(idx)) >= unsigned(r_time)                    report "Deadline not met (late by "  & integer'image(to_integer(unsigned(r_time)-unsigned(deadlines(idx)))) & ")" severity failure;
        assert s_early='1' or unsigned(deadlines(idx))  < unsigned(r_time) + 2**c_log_latency report "Deadline not met (early by " & integer'image(to_integer(unsigned(deadlines(idx))-unsigned(r_time))) & ")" severity failure;
        
        deadlines(idx) := (others => '1');
      end if;
      
      -- Make sure there's nothing missing
      for i in deadlines'range loop
        assert unsigned(deadlines(i)) >= unsigned(r_time) report "Deadline missed, disable this assertion to check if it ever comes out later" severity failure;
      end loop;
    end if;
  end process;

end rtl;
