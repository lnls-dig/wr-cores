--! @file eca_bitonic_tb.vhd
--! @brief ECA bitonic sorting network test code
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

entity eca_bitonic_tb is
  generic(
    g_case  : natural);
  port(
    clk_i   : in std_logic;
    rst_n_i : in std_logic);
end eca_bitonic_tb;

architecture rtl of eca_bitonic_tb is
  -- Out of principle, tell quartus to leave my design alone.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  constant c_size : natural := g_case;
  constant c_full : natural := 2**c_size;
  constant c_wide : natural := 8;
  
  signal r_nums : t_eca_matrix(c_full-1 downto 0, c_wide-1 downto 0) := (others => (others => '0'));
  signal nums_o : t_eca_matrix(c_full-1 downto 0, c_wide-1 downto 0);
  
begin

  sort : eca_bitonic
    generic map(
      g_log_size => c_size,
      g_wide     => c_wide,
      g_order    => true)
    port map(
      clk_i   => clk_i,
      rst_n_i => rst_n_i,
      en_i    => '1',
      nums_i  => r_nums,
      nums_o  => nums_o);
  
  main : process(clk_i, rst_n_i) is
    variable s1, s2 : integer := 42;
    variable rand, last, value : std_logic_vector(c_wide-1 downto 0);
  begin
    if rst_n_i = '0' then
      r_nums <= (others => (others => '0'));
    elsif rising_edge(clk_i) then
      -- Generate a random test vector
      for i in 0 to c_full-1 loop
        p_eca_uniform(s1, s2, rand);
        for b in 0 to c_wide-1 loop
          r_nums(i, b) <= rand(b);
        end loop;
      end loop;
      
      -- Confirm that the output is sorted
      last := (others => '0');
      for i in 0 to c_full-1 loop
        for b in 0 to c_wide-1 loop
          value(b) := nums_o(i,b);
        end loop;
        assert unsigned(last) <= unsigned(value) report "Sort order incorrect" severity failure;
        last := value;
      end loop;
      
    end if;
  end process;

end rtl;
