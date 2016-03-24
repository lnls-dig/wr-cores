--! @file eca_free_tb.vhd
--! @brief ECA channel buffer test code
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2015 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This core tests the behaviour of the eca_free unit
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

entity eca_free_tb is
  generic(
    g_case  : natural);
  port(
    clk_i   : in std_logic;
    rst_n_i : in std_logic);
end eca_free_tb;

architecture rtl of eca_free_tb is

  constant c_size : natural := 1 + g_case; -- smaller tests edge cases faster
  
  signal s_full    : std_logic;
  signal s_alloc   : std_logic;
  signal s_free    : std_logic;
  signal s_entry_i : std_logic_vector(c_size-1 downto 0);
  signal s_entry_o : std_logic_vector(c_size-1 downto 0);
  
begin

  buf : eca_free
    generic map(
      g_log_size => c_size)
    port map(
      clk_i      => clk_i,
      rst_n_i    => rst_n_i,
      used_o     => open,
      full_o     => s_full,
      alloc_i    => s_alloc,
      entry_o    => s_entry_o,
      free_i     => s_free,
      entry_i    => s_entry_i);
  
  test : process(clk_i, rst_n_i) is
    variable s1, s2 : positive := 42;
    variable alloc : std_logic;
    constant c_fifo_deep : natural := 2**(c_size+1);
    
    type t_fifo is array(c_fifo_deep-1 downto 0) of std_logic_vector(c_size-1 downto 0);
    variable fifo_dat : t_fifo;
    variable fifo_stb : std_logic_vector(c_fifo_deep-1 downto 0);
  begin
    if rst_n_i = '0' then
      alloc   := '0';
      s_alloc <= '0';
      s_free  <= '0';
      s_entry_i <= (others => '-');
      fifo_dat := (others => (others => '-'));
      fifo_stb := (others => '0');
    elsif rising_edge(clk_i) then
      s_free    <= fifo_stb(0);
      s_entry_i <= fifo_dat(0);
      
      if s_full = '0' then
        for i in c_fifo_deep-1 downto 1 loop
          assert fifo_stb(i) = '0' or fifo_dat(i) /= s_entry_o
          report "duplicated entry"
          severity failure;
        end loop;
      end if;
      
      fifo_dat := s_entry_o & fifo_dat(fifo_dat'high downto 1);
      fifo_stb := (s_alloc and not s_full) & fifo_stb(fifo_stb'high downto 1);
      
      p_eca_uniform(s1, s2, alloc);
      s_alloc <= alloc;
    end if;
  end process;

end rtl;
