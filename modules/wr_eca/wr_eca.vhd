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
    g_num_ios        : natural :=  8; -- Number of gpios
    g_num_channels   : natural :=  1; -- Number of channels (must be >= 1)
    g_num_streams    : natural :=  1; -- Number of streams  (must be >= 1)
    g_log_table_size : natural :=  8; -- 2**g_log_table_size = maximum number of conditions
    g_log_queue_size : natural :=  8; -- 2**g_log_size       = maximum number of pending actions
    g_log_max_delay  : natural := 32);-- 2**g_log_max_delay  = maximum delay before executed as early
  port(
    -- Stream events to the ECA unit (lower index has priority)
    e_clk_i     : in  std_logic_vector          (g_num_streams-1 downto 0);
    e_rst_n_i   : in  std_logic_vector          (g_num_streams-1 downto 0);
    e_slave_i   : in  t_wishbone_slave_in_array (g_num_streams-1 downto 0);
    e_slave_o   : out t_wishbone_slave_out_array(g_num_streams-1 downto 0);
    -- ECA control registers
    c_clk_i     : in  std_logic;
    c_rst_n_i   : in  std_logic;
    c_slave_i   : in  t_wishbone_slave_in;
    c_slave_o   : out t_wishbone_slave_out;
    -- Actions output according to time
    a_clk_i     : in  std_logic;
    a_rst_n_i   : in  std_logic; -- Hold for at least 10 cycles
    a_tai_i     : in  std_logic_vector(39 downto 0);
    a_cycles_i  : in  std_logic_vector(27 downto 0);
    a_stall_i   : in  std_logic_vector(g_num_channels-1 downto 0);
    a_channel_o : out t_channel_array(g_num_channels-1 downto 0);
    a_io_o      : out t_gpio_array(g_num_ios-1 downto 0);
    -- Interrupts that report failure conditions
    i_clk_i     : in  std_logic;
    i_rst_n_i   : in  std_logic;
    i_master_i  : in  t_wishbone_master_in;
    i_master_o  : out t_wishbone_master_out);
end wr_eca;

architecture rtl of wr_eca is
  -- Out of principle, tell quartus to leave my design alone.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  
  signal sa_time0    : t_time;
  
  signal sa_stb      : std_logic_vector(g_num_streams-1 downto 0);
  signal sa_event    : t_event_array   (g_num_streams-1 downto 0);
  signal sa_param    : t_param_array   (g_num_streams-1 downto 0);
  signal sa_tef      : t_tef_array     (g_num_streams-1 downto 0);
  signal sa_time     : t_time_array    (g_num_streams-1 downto 0);
  
  signal sa_full_stb   : std_logic_vector(g_num_streams downto 0);
  signal sa_full_stall : std_logic_vector(g_num_streams downto 0);
  signal sa_full_event : t_event_array   (g_num_streams downto 0);
  signal sa_full_param : t_param_array   (g_num_streams downto 0);
  signal sa_full_tef   : t_tef_array     (g_num_streams downto 0);
  signal sa_full_time  : t_time_array    (g_num_streams downto 0);
  
  signal sa_io : t_eca_matrix(g_num_ios-1 downto 0, 7 downto 0);
  
begin

  E0 : eca
    generic map(
      g_num_ios        => g_num_ios,
      g_num_channels   => g_num_channels,
      g_log_table_size => g_log_table_size,
      g_log_queue_size => g_log_queue_size,
      g_log_multiplier => 3, -- 125*2**3 = 1GHz
      g_log_max_delay  => g_log_max_delay,
      g_log_latency    => 12, -- 2**12 = 4096ns < 5us as required
      g_log_counter    => 20) -- saftd can easily poll at 2**20ns = 1s = 1Hz
    port map(
      e_stb_i     => sa_full_stb  (0),
      e_stall_o   => sa_full_stall(0),
      e_event_i   => sa_full_event(0),
      e_param_i   => sa_full_param(0),
      e_tef_i     => sa_full_tef  (0),
      e_time_i    => sa_full_time (0),
      c_clk_i     => c_clk_i,
      c_rst_n_i   => c_rst_n_i,
      c_slave_i   => c_slave_i,
      c_slave_o   => c_slave_o,
      a_clk_i     => a_clk_i,
      a_rst_n_i   => a_rst_n_i,
      a_time_i    => sa_time0,
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
      time_o   => sa_time0);
  
  sa_full_stb  (g_num_streams) <= '0';
  sa_full_event(g_num_streams) <= (others => '-');
  sa_full_param(g_num_streams) <= (others => '-');
  sa_full_tef  (g_num_streams) <= (others => '-');
  sa_full_time (g_num_streams) <= (others => '-');
  
  -- Priority access goes to #0
  Sx : for stream in 0 to g_num_streams-1 generate
    S : eca_wb_event
      port map(
        w_clk_i   => e_clk_i  (stream),
        w_rst_n_i => e_rst_n_i(stream),
        w_slave_i => e_slave_i(stream),
        w_slave_o => e_slave_o(stream),
        
        e_clk_i   => a_clk_i,
        e_rst_n_i => a_rst_n_i,
        e_stb_o   => sa_stb  (stream),
        e_stall_i => sa_full_stall(stream),
        e_event_o => sa_event(stream),
        e_param_o => sa_param(stream),
        e_tef_o   => sa_tef  (stream),
        e_time_o  => sa_time (stream));
    
    sa_full_stall(stream+1) <= sa_stb  (stream) or                           sa_full_stall(stream);
    sa_full_stb  (stream)   <= sa_stb  (stream) or                           sa_full_stb  (stream+1);
    sa_full_event(stream)   <= sa_event(stream) when sa_stb(stream)='1' else sa_full_event(stream+1);
    sa_full_param(stream)   <= sa_param(stream) when sa_stb(stream)='1' else sa_full_param(stream+1);
    sa_full_tef  (stream)   <= sa_tef  (stream) when sa_stb(stream)='1' else sa_full_tef  (stream+1);
    sa_full_time (stream)   <= sa_time (stream) when sa_stb(stream)='1' else sa_full_time (stream+1);
  end generate;
  
end rtl;
