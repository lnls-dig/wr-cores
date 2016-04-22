--! @file eca_bitonic_swap.vhd
--! @brief ECA Bitonic Sorting Network (swap entity)
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2016 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! Exchange two numbers to be be in sorted order
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

entity eca_bitonic_swap is
  generic(
    g_wide  : natural;
    g_order : boolean); -- true = smallest first
  port(
    clk_i   : in  std_logic;
    rst_n_i : in  std_logic;
    en_i    : in  std_logic;
    a_i     : in  std_logic_vector(g_wide-1 downto 0);
    b_i     : in  std_logic_vector(g_wide-1 downto 0);
    a_o     : out std_logic_vector(g_wide-1 downto 0);
    b_o     : out std_logic_vector(g_wide-1 downto 0));
end eca_bitonic_swap;

architecture rtl of eca_bitonic_swap is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  signal r_a, r_b : std_logic_vector(g_wide-1 downto 0) := (others => '0');
  signal s_flip : boolean;

begin

  regs : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_a <= (others => '0');
      r_b <= (others => '0');
    elsif rising_edge(clk_i) then
      if en_i = '1' then
        r_a <= a_i;
        r_b <= b_i;
      end if;
    end if;
  end process;
  
  s_flip <= (unsigned(r_b) < unsigned(r_a)) = g_order;
  
  a_o <= r_b when s_flip else r_a;
  b_o <= r_a when s_flip else r_b;
  
end rtl;
