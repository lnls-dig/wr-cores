--! @file eca_buffer.vhd
--! @brief ECA Buffer for actions in the future
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component receives actions to be executed, in any order.
--! It stores them until their deadline arrives
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

entity eca_buffer is
  generic(
    g_ext_size       : natural; -- extra bits tracked by scanner
    g_log_size       : natural; -- 2**g_log_size       = buffer entries
    g_log_multiplier : natural; -- 2**g_log_multiplier = ticks per cycle
    g_log_latency    : natural; -- 2**g_log_latency    = ticks of calendar delay
    g_num_ports      : natural);-- g_num_ports = 2**(g_log_size+g_log_multiplier+1-g_log_latency)
  port(
    clk_i      : in  std_logic;
    rst_n_i    : in  std_logic;
    -- Current time, minus some coefficient
    time_i     : in  t_time;
    -- Write port
    full_o     : out std_logic; -- if full_o=1, new actions not accepted (overflow)
    channel_i  : in  t_channel; -- valid = strobe, expected to be registered
    ext_i      : in  std_logic_vector(g_ext_size-1 downto 0);
    -- Read port
    read_idx_i : in  std_logic_vector(g_log_size-1 downto 0);
    channel_o  : out t_channel;
    -- Scan reports ready for calendar (each op is held for two cycles)
    scan_stb_o : out std_logic_vector(g_num_ports-1 downto 0);
    scan_low_o : out std_logic_vector(g_log_latency*g_num_ports-1 downto 0);
    scan_idx_o : out std_logic_vector(g_log_size   *g_num_ports-1 downto 0);
    scan_ext_o : out std_logic_vector(g_ext_size   *g_num_ports-1 downto 0);
    -- Free a record
    free_stb_i : in  std_logic;
    free_idx_i : in  std_logic_vector(g_log_size-1 downto 0));
end eca_buffer;

architecture rtl of eca_buffer is

  signal s_full  : std_logic;
  signal s_we    : std_logic;
  signal s_alloc : std_logic_vector(g_log_size-1 downto 0);

begin

  correct :
    assert g_num_ports = 2**(g_log_size+g_log_multiplier+1-g_log_latency)
    report "g_num_ports not set correctly"
    severity failure;
  
  sane : 
    assert g_num_ports >= 1
    report "g_num_ports must be at least 1"
    severity failure;
  
  -- Manage free buffer entries
  free : eca_free
    generic map(
      g_log_size => g_log_size)
    port map(
      clk_i   => clk_i,
      rst_n_i => rst_n_i,
      full_o  => s_full,
      alloc_i => channel_i.valid,
      entry_o => s_alloc,
      free_i  => free_stb_i,
      entry_i => free_idx_i);
  
  full_o <= s_full;
  s_we <= channel_i.valid and not s_full;
  
  -- Data table (all the channel meta-data)
  data : eca_data
    generic map(
      g_log_size => g_log_size)
    port map(
      clk_i   => clk_i,
      rst_n_i => rst_n_i,
      w_en_i  => s_we,
      w_idx_i => s_alloc,
      w_dat_i => channel_i,
      r_idx_i => read_idx_i,
      r_dat_o => channel_o);
  
  -- Scan table (valid + counter + ext)
  -- read, decrement

end rtl;
