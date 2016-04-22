--! @file wr_eca.vhd
--! @brief Event-Condition-Action package
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component wraps the raw ECA suitably for use with White-Rabbit.
--! It adds Wishbone event channels and translates WR time.
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

use work.wishbone_pkg.all;
use work.eca_pkg.t_gpio_array;
use work.eca_internals_pkg.all;

entity wr_eca is
  generic(
    g_channel_types  : t_nat_array;
    g_channel_nums   : t_nat_array;
    g_num_streams    : natural :=  1; -- Number of streams  (must be >= 1)
    g_num_ios        : natural :=  8; -- Number of gpios
    g_log_table_size : natural :=  8; -- 2**g_log_table_size = maximum number of conditions
    g_log_queue_size : natural :=  8; -- 2**g_log_size       = maximum number of pending actions
    g_log_max_delay  : natural := 32);-- 2**g_log_max_delay  = maximum delay before executed as early
  port(
    -- ECA control registers
    c_clk_i     : in  std_logic;
    c_rst_n_i   : in  std_logic;
    c_slave_i   : in  t_wishbone_slave_in;
    c_slave_o   : out t_wishbone_slave_out;
    -- Action clock domain
    a_clk_i     : in  std_logic;
    a_rst_n_i   : in  std_logic; -- Hold for at least 10 cycles
    a_tai_i     : in  std_logic_vector(39 downto 0);
    a_cycles_i  : in  std_logic_vector(27 downto 0);
    a_time_o    : out t_time;
    -- Input streams (lower index has priority)
    a_stream_i  : in  t_stream_array(g_num_streams-1 downto 0);
    a_stall_o   : out std_logic_vector(g_num_streams-1 downto 0);
    -- Output actions
    a_stall_i   : in  std_logic_vector(g_channel_types'range);
    a_channel_o : out t_channel_array(g_channel_types'range);
    a_io_o      : out t_gpio_array(g_num_ios-1 downto 0);
    -- Interrupts that report failure conditions
    i_clk_i     : in  std_logic;
    i_rst_n_i   : in  std_logic;
    i_master_i  : in  t_wishbone_master_in;
    i_master_o  : out t_wishbone_master_out);
end wr_eca;

architecture rtl of wr_eca is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  signal sa_time : t_time;
  signal sa_io   : t_eca_matrix(g_num_ios-1 downto 0, 7 downto 0);
  
begin

  E0 : eca
    generic map(
      g_channel_types  => g_channel_types,
      g_channel_nums   => g_channel_nums,
      g_num_ios        => g_num_ios,
      g_num_streams    => g_num_streams,
      g_log_table_size => g_log_table_size,
      g_log_queue_size => g_log_queue_size,
      g_log_multiplier => 3, -- 125*2**3 = 1GHz
      g_log_max_delay  => g_log_max_delay,
      g_log_latency    => 12, -- 2**12 = 4096ns < 5us as required
      g_log_counter    => 20) -- saftd can easily poll at 2**20ns = 1s = 1Hz
    port map(
      c_clk_i     => c_clk_i,
      c_rst_n_i   => c_rst_n_i,
      c_slave_i   => c_slave_i,
      c_slave_o   => c_slave_o,
      a_clk_i     => a_clk_i,
      a_rst_n_i   => a_rst_n_i,
      a_time_i    => sa_time,
      a_stream_i  => a_stream_i,
      a_stall_o   => a_stall_o,
      a_stall_i   => a_stall_i,
      a_channel_o => a_channel_o,
      a_io_o      => sa_io,
      i_clk_i     => i_clk_i,
      i_rst_n_i   => i_rst_n_i,
      i_master_i  => i_master_i,
      i_master_o  => i_master_o);
  
  ios : for i in 0 to g_num_ios-1 generate
    bits : for b in 0 to 7 generate
      a_io_o(i)(b) <= sa_io(i,b);
    end generate;
  end generate;

  T0 : eca_wr_time
    port map(
      clk_i    => a_clk_i,
      rst_n_i  => a_rst_n_i,
      tai_i    => a_tai_i,
      cycles_i => a_cycles_i,
      time_o   => sa_time);

  a_time_o <= sa_time;

end rtl;
