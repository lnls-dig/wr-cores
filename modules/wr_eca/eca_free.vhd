--! @file eca_free.vhd
--! @brief ECA Free Stack
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component allocates and releases indexes on request.
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

entity eca_free is
  generic(
    g_log_size : natural);
  port(
    clk_i   : in  std_logic;
    rst_n_i : in  std_logic;
    used_o  : out std_logic_vector(g_log_size downto 0);
    full_o  : out std_logic;
    alloc_i : in  std_logic; -- consume the entry output when alloc_i=1
    entry_o : out std_logic_vector(g_log_size-1 downto 0);
    free_i  : in  std_logic; -- release the entry input when free_i=1
    entry_i : in  std_logic_vector(g_log_size-1 downto 0));
end eca_free;

architecture rtl of eca_free is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  constant c_zero : std_logic_vector(g_log_size-1 downto 0) := (others => '0');

  -- While r_index('high) = '0', wipe the free table with the lower g_log_size bits
  signal s_mux   : std_logic_vector(           1 downto 0);
  signal r_used  : std_logic_vector(g_log_size   downto 0) := "1" & c_zero;
  signal s_used  : std_logic_vector(g_log_size   downto 0);
  signal s_freed : std_logic_vector(g_log_size-1 downto 0);
  signal s_top   : std_logic_vector(g_log_size-1 downto 0);
  signal s_avail : std_logic_vector(g_log_size-1 downto 0);
  signal r_reset : std_logic_vector(g_log_size   downto 0) := (others => '0');
  signal r_ready : std_logic := '0';
  signal s_full  : std_logic;
  signal s_alloc : std_logic;
  signal s_free  : std_logic;
  signal s_wen   : std_logic;

begin

  -- Manage free buffer entries
  stack : eca_sdp
    generic map(
      g_addr_bits  => g_log_size,
      g_data_bits  => g_log_size,
      g_bypass     => true,
      g_dual_clock => false)
    port map(
      w_clk_i  => clk_i,
      w_en_i   => s_wen,
      w_addr_i => s_used(g_log_size-1 downto 0),
      w_data_i => s_freed,
      r_clk_i  => clk_i,
      r_addr_i => s_used(g_log_size-1 downto 0),
      r_data_o => s_top);
  
  -- While wiping the table, we are full and also forcing a free
  s_full  <= r_used(r_used'high) or not r_ready;
  s_alloc <= alloc_i and not s_full;
  s_free  <= free_i or not r_reset(r_reset'high);
  
  s_wen   <= not s_alloc and s_free;
  s_freed <= f_eca_mux(r_reset(r_reset'high), entry_i, r_reset(g_log_size-1 downto 0));
  s_avail <= f_eca_mux(free_i, entry_i, s_top);
  
  s_mux <= (std_logic_vector'("") & s_free & s_alloc);
  with s_mux select
  s_used <=
    r_used                when "00",
    f_eca_add(r_used,  1) when "01",
    f_eca_add(r_used, -1) when "10",
    r_used                when "11",
    (others => '-')       when others;
  
  main : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_reset <= (others => '0');
      r_used  <= "1" & c_zero;
      r_ready <= '0';
    elsif rising_edge(clk_i) then
      r_reset <= f_eca_add(r_reset, not r_reset(r_reset'high));
      r_used  <= s_used;
      r_ready <= r_reset(r_reset'high);
    end if;
  end process;
  
  used_o  <= r_used;
  full_o  <= s_full;
  entry_o <= s_avail;
  
end rtl;
