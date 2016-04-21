--! @file eca_compact_tb.vhd
--! @brief ECA compacting network test code
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2015 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This core tests the behaviour of the eca_bitonic unit
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

entity eca_compact_tb is
  generic(
    g_case  : natural);
  port(
    clk_i   : in std_logic;
    rst_n_i : in std_logic);
end eca_compact_tb;

architecture rtl of eca_compact_tb is

  constant c_size : natural := g_case + 1;
  constant c_wide : natural := f_eca_log2(c_size);
  
  function f_nums return t_eca_matrix is
    variable result : t_eca_matrix(c_size-1 downto 0, c_wide-1 downto 0);
    variable count  : unsigned(c_wide-1 downto 0);
  begin
    for i in 0 to c_size-1 loop
      count := to_unsigned(i, count'length);
      for b in 0 to c_wide-1 loop
        result(i, b) := count(b);
      end loop;
    end loop;
    return result;
  end f_nums;
  
  function f_count(x : std_logic_vector) return natural is
    variable result : natural := 0;
  begin
    for i in x'range loop
      if x(i) = '1' then result := result + 1; end if;
    end loop;
    return result;
  end f_count;
  
  constant c_nums : t_eca_matrix := f_nums;
  constant c_zero : std_logic_vector(c_size-1 downto 0) := (others => '0');
  
  signal s_nums  : t_eca_matrix(c_size-1 downto 0, c_wide-1 downto 0);
  signal s_valid : std_logic_vector(c_size-1 downto 0);
  signal r_valid : std_logic_vector(c_size-1 downto 0);
  
  type t_nat_array is array(c_wide downto 0) of natural;
  signal r_nat_array : t_nat_array;
  
begin

  sort : eca_compact
    generic map(
      g_size => c_size,
      g_wide => c_wide)
    port map(
      clk_i   => clk_i,
      rst_n_i => rst_n_i,
      valid_i => r_valid,
      valid_o => s_valid,
      data_i  => c_nums,
      data_o  => s_nums);
  
  main : process(clk_i, rst_n_i) is
    variable s1, s2 : integer := 42;
    variable valid : std_logic_vector(c_size-1 downto 0);
    variable prev, this : unsigned(c_wide-1 downto 0);
  begin
    if rst_n_i = '0' then
      r_valid <= (others => '0');
    elsif rising_edge(clk_i) then
      p_eca_uniform(s1, s2, valid);
      r_valid <= valid;
      
      -- s_valid must be a contiguous run of 0's followed by 1's
      assert (std_logic_vector(unsigned(s_valid) + 1) and s_valid) = c_zero
      report "valid_o is not of the form 0000000111111"
      severity failure;
      
      -- Make sure we have the right number of 1s
      r_nat_array <= f_count(valid) & r_nat_array(r_nat_array'high downto 1);
      assert r_nat_array(0) = f_count(s_valid)
      report "Misplaced some 1s in the pipeline"
      severity failure;
      
      -- Numbers must be ascending
      for b in 0 to c_wide-1 loop
        prev(b) := s_nums(0,b);
      end loop;
      for i in 1 to c_size-1 loop
        for b in 0 to c_wide-1 loop
          this(b) := s_nums(i,b);
        end loop;
        
        assert s_valid(i) = '0' or prev < this
        report "Numeric sequence is not strictly ascending"
        severity failure;
        prev := this;
      end loop;
    end if;
  end process;

end rtl;
