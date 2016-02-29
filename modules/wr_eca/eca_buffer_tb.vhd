--! @file eca_buffer_tb.vhd
--! @brief ECA channel buffer test code
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2015 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This core tests the behaviour of the eca_buffer unit
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

entity eca_buffer_tb is
  port(
    clk_i   : in std_logic;
    rst_n_i : in std_logic);
end eca_buffer_tb;

architecture rtl of eca_buffer_tb is

  signal s_channel : t_channel := c_idle_channel;
  
begin

  buf : eca_buffer
    generic map(
      g_ext_size       => 1,
      g_log_size       => 8,
      g_log_multiplier => 3,
      g_log_latency    => 12,
      g_num_ports      => 1)
    port map(
      clk_i      => clk_i,
      rst_n_i    => rst_n_i,
      time_i     => (others => '0'),
      full_o     => open,
      channel_i  => s_channel,
      ext_i      => (others => '0'),
      read_idx_i => (others => '0'),
      channel_o  => open,
      scan_stb_o => open,
      scan_low_o => open,
      scan_idx_o => open,
      scan_ext_o => open,
      free_stb_i => '0',
      free_idx_i => (others => '0'));

  test : process(clk_i, rst_n_i) is
    variable s1, s2 : positive := 42;
    variable alloc : std_logic;
  begin
    if rst_n_i = '0' then
      alloc := '0';
    elsif rising_edge(clk_i) then
      p_eca_uniform(s1, s2, alloc);
      s_channel.valid <= alloc;
      
      -- shift index
    end if;
  end process;

end rtl;
