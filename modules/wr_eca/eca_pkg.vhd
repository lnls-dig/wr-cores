--! @file eca_pkg.vhd
--! @brief Event-Condition-Action package
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This package defines all the needed types and components for the ECA unit.
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
use work.wishbone_pkg.all;
use work.eca_auto_pkg.all;
use work.eca_queue_auto_pkg.all;
use work.eca_tlu_auto_pkg.all;
use work.eca_ac_wbm_auto_pkg.all;
use work.eca_internals_pkg.all;

package eca_pkg is

 constant c_eca_slave_sdb        : t_sdb_device := work.eca_auto_pkg.c_eca_slave_sdb;
 constant c_eca_queue_slave_sdb  : t_sdb_device := work.eca_queue_auto_pkg.c_eca_queue_slave_sdb;
 constant c_eca_tlu_slave_sdb    : t_sdb_device := work.eca_tlu_auto_pkg.c_eca_tlu_slave_sdb;
 constant c_eca_ac_wbm_slave_sdb : t_sdb_device := work.eca_ac_wbm_auto_pkg.c_eca_ac_wbm_slave_sdb;

 constant c_eca_event_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"02",
    abi_ver_minor => x"00",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"4", -- 32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"0000000000000003",
    product => (
    vendor_id     => x"0000000000000651",
    device_id     => x"8752bf45",
    version       => x"00000001",
    date          => x"20130204",
    name          => "ECA_UNIT:EVENTS_IN ")));

  subtype t_nat_array     is work.eca_internals_pkg.t_nat_array;
  subtype t_stream        is work.eca_internals_pkg.t_stream;
  subtype t_stream_array  is work.eca_internals_pkg.t_stream_array;
  subtype t_channel       is work.eca_internals_pkg.t_channel;
  subtype t_channel_array is work.eca_internals_pkg.t_channel_array;
  subtype t_time          is work.eca_internals_pkg.t_time;
  type    t_gpio_array    is array(natural range <>) of std_logic_vector(7 downto 0);
  
  constant c_linux        : natural := 1;
  constant c_wb_master    : natural := 2;
  constant c_embedded_cpu : natural := 3;
  constant c_scubus_tag   : natural := 128;
  
  -- White-Rabbit variant of Event-Condition-Action Unit
  component wr_eca is
    generic(
      g_channel_types  : t_nat_array := (0 => c_linux);
      g_channel_nums   : t_nat_array := (0 => 32); -- Anything not explicitly set is 1
      g_num_streams    : natural :=  1; -- Number of streams
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
  end component;
  
  -- Convert WB writes into inbound ECA event stream
  component eca_wb_event is
    port(
      w_clk_i    : in  std_logic;
      w_rst_n_i  : in  std_logic;
      w_slave_i  : in  t_wishbone_slave_in;
      w_slave_o  : out t_wishbone_slave_out;
      e_clk_i    : in  std_logic;
      e_rst_n_i  : in  std_logic;
      e_stream_o : out t_stream;
      e_stall_i  : in  std_logic);
  end component;
  
  -- Convert input edges into inbound ECA event stream
  component eca_tlu is
    generic(
      g_inputs  : natural;
      g_history : natural := 10000; -- 10us
      g_stable  : natural := 10);
    port(
      c_clk_i    : in  std_logic;
      c_rst_n_i  : in  std_logic;
      c_slave_i  : in  t_wishbone_slave_in;
      c_slave_o  : out t_wishbone_slave_out;
      a_clk_i    : in  std_logic;
      a_rst_n_i  : in  std_logic;
      a_time_i   : in  t_time;
      a_gpio_i   : in  t_gpio_array(g_inputs-1 downto 0);
      a_stream_o : out t_stream;
      a_stall_i  : in  std_logic);
  end component;
  
  -- FIFO-style interface to access the output of an ECA channel
  component eca_queue is
    generic(
      g_queue_id  : natural);
    port(
      a_clk_i     : in  std_logic;
      a_rst_n_i   : in  std_logic;
      a_stall_o   : out std_logic;
      a_channel_i : in  t_channel;
      q_clk_i     : in  std_logic;
      q_rst_n_i   : in  std_logic;
      q_slave_i   : in  t_wishbone_slave_in;
      q_slave_o   : out t_wishbone_slave_out);
  end component;

  -- sends channel_i.tag to the scu bus
  component eca_scubus_channel is
  port(
    clk_i     : in  std_logic;
    rst_n_i   : in  std_logic;
    channel_i : in  t_channel;
    tag_valid : out std_logic;
    tag       : out std_logic_vector(31 downto 0));  
  end component eca_scubus_channel;
  
  -- uses channel tag to replay specified bus ops macro on wishbone master 
  component eca_ac_wbm is
    generic(
      g_entries  : natural := 32;
      g_ram_size : natural := 256);
    port(
      clk_ref_i   : in  std_logic;                                            
      rst_ref_n_i : in  std_logic;
      channel_i   : in  t_channel;
      
      clk_sys_i   : in  std_logic;
      rst_sys_n_i : in  std_logic;
      slave_i     : in  t_wishbone_slave_in  := ('0', '0', x"00000000", x"F", '0', x"00000000"); 
      slave_o     : out t_wishbone_slave_out;
      master_o    : out t_wishbone_master_out;
      master_i    : in  t_wishbone_master_in);
  end component;
  
end eca_pkg;
