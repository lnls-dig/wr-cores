--! @file eca_fifo_tb.vhd
--! @brief ECA FIFO test code
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2015 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This core tests the behaviour of the eca_fifo unit
--!
--------------------------------------------------------------------------------
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

entity eca_fifo_tb is
  generic(
    g_case  : natural);
  port(
    clk_i   : in std_logic;
    rst_n_i : in std_logic);
end eca_fifo_tb;

architecture rtl of eca_fifo_tb is

  constant c_size : natural := g_case + 1;
  constant c_cols : natural := 32;

  signal r_push   : std_logic := '0';
  signal r_pop    : std_logic := '0';
  signal s_valid  : std_logic;
  signal s_fresh  : std_logic;
  signal s_data_i : t_eca_matrix(0 downto 0, c_cols-1 downto 0);
  signal s_data_o : t_eca_matrix(0 downto 0, c_cols-1 downto 0);
  signal r_push_count : unsigned(c_cols-1 downto 0) := (others => '0');
  signal r_pop_count  : unsigned(c_cols-1 downto 0) := (others => '0');
  signal s_pop_count  : unsigned(c_cols-1 downto 0);
  
begin

  fifo : eca_fifo
    generic map(
      g_log_size => c_size,
      g_rows     => 1,
      g_cols     => c_cols)
    port map(
      clk_i   => clk_i,
      rst_n_i => rst_n_i,
      push_i  => r_push,
      data_i  => s_data_i,
      pop_i   => r_pop,
      valid_o => s_valid,
      fresh_o => s_fresh,
      data_o  => s_data_o);
  
  bits : for i in 0 to c_cols-1 generate
    s_data_i(0,i) <= r_push_count(i);
    s_pop_count(i) <= s_data_o(0,i);
  end generate;
  
  main : process(clk_i, rst_n_i) is
    variable s1, s2 : positive := 42;
    variable push, pop : std_logic;
  begin
    if rst_n_i = '0' then
      r_push <= '0';
      r_pop  <= '0';
      r_push_count <= (others => '0');
      r_pop_count  <= (others => '0');
    elsif rising_edge(clk_i) then
      
      assert (s_valid = '0') or (s_pop_count > r_pop_count)
      report "Data wrong"
      severity failure;
      
      assert (s_valid = '0') or (s_fresh = '1') = (s_pop_count = r_push_count-2)
      report "Freshness flag wrong"
      severity failure;
      
      r_push_count <= r_push_count + 1;
      if (r_pop and s_valid) = '1' then
        r_pop_count <= s_pop_count;
      end if;
      
      p_eca_uniform(s1, s2, push);
      p_eca_uniform(s1, s2, pop);
      
      r_push <= push;
      r_pop  <= pop;
      
    end if;
  end process;
  
end rtl;
