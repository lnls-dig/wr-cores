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
use work.eca_internals_pkg.all;
use work.wishbone_pkg.all;
use work.eca_ac_wbm_auto_pkg.c_eca_ac_wbm_slave_sdb;

package eca_pkg is

 constant c_eca_ac_wbm_slave_sdb : t_sdb_device := work.eca_ac_wbm_auto_pkg.c_eca_ac_wbm_slave_sdb;

 constant c_eca_sdb : t_sdb_device := (
    abi_class     => x"0000", -- undocumented device
    abi_ver_major => x"02",
    abi_ver_minor => x"00",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7", -- 8/16/32-bit port granularity
    sdb_component => (
    addr_first    => x"0000000000000000",
    addr_last     => x"000000000000007f",
    product => (
    vendor_id     => x"0000000000000651",
    device_id     => x"8752bf44",
    version       => x"00000001",
    date          => x"20130204",
    name          => "ECA_UNIT:CONTROL   ")));
    
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

  type t_channel       is eca_internals_pkg.t_channel;
  type t_name          is eca_internals_pkg.t_name;
  type t_channel_array is eca_internals_pkg.t_channel_array;
  type t_name_array    is eca_internals_pkg.t_name_array;
  
  -- Convert a string into a name (aka, pad it)
  function f_name(name : string) return t_name;
  
  -- White-Rabbit variant of Event-Condition-Action Unit
  component wr_eca is
    generic(
      g_eca_name       : t_name;
      g_channel_names  : t_name_array;
      g_log_table_size : natural := 7; -- 128 entries -- condition table
      g_log_queue_len  : natural := 8; -- 256 entries -- action queue size
      g_num_channels   : natural := 4; -- max 256
      g_num_streams    : natural := 1;
      g_log_clock_mult : natural := 4; -- a_clk_i and c_clk_i must be within 16*
      g_inspect_queue  : boolean := true;
      g_inspect_table  : boolean := true);
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
      a_channel_o : out t_channel_array(g_num_channels-1 downto 0);
      -- Interrupts that report failure conditions
      i_clk_i     : in  std_logic;
      i_rst_n_i   : in  std_logic;
      i_master_i  : in  t_wishbone_master_in;
      i_master_o  : out t_wishbone_master_out);
  end component;
  
  -- Raw Event-Condition-Action Unit
  component eca is
    generic(
      g_eca_name       : t_name;
      g_channel_names  : t_name_array;
      g_log_table_size : natural := 7; -- 128 entries -- condition table
      g_log_queue_len  : natural := 8; -- 256 entries -- action queue size
      g_num_channels   : natural := 4; -- max 256
      g_log_clock_mult : natural := 4; -- a_clk_i and c_clk_i must be within 16*
      g_inspect_queue  : boolean := true;
      g_inspect_table  : boolean := true;
      g_frequency_mul  : natural := 1; -- 125MHz = 1*5^9*2^6/1
      g_frequency_5s   : natural := 9; -- mul:32, 5s:8, 2s:8, div:16 bits
      g_frequency_2s   : natural := 6;
      g_frequency_div  : natural := 1);
    port(
      -- Push events to the ECA unit (a_clk_i domain)
      e_stb_i     : in  std_logic;
      e_stall_o   : out std_logic;
      e_event_i   : in  t_event;
      e_param_i   : in  t_param;
      e_tef_i     : in  t_tef;
      e_time_i    : in  t_time;
      e_index_o   : out std_logic_vector(7 downto 0);
      -- ECA control registers
      c_clk_i     : in  std_logic;
      c_rst_n_i   : in  std_logic;
      c_slave_i   : in  t_wishbone_slave_in;
      c_slave_o   : out t_wishbone_slave_out;
      -- Actions output according to time
      a_clk_i     : in  std_logic;
      a_rst_n_i   : in  std_logic;
      a_time_i    : in  t_time;
      a_channel_o : out t_channel_array(g_num_channels-1 downto 0);
      -- Interrupts that report failure conditions
      i_clk_i     : in  std_logic;
      i_rst_n_i   : in  std_logic;
      i_master_i  : in  t_wishbone_master_in;
      i_master_o  : out t_wishbone_master_out);
  end component;
  
  -- Put low bits of param to address tag
  component eca_wb_channel is
    port(
      clk_i     : in  std_logic;
      rst_n_i   : in  std_logic;
      channel_i : in  t_channel;
      master_o  : out t_wishbone_master_out;
      master_i  : in  t_wishbone_master_in);
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
  
  -- Convert White Rabbit time to ECA time
  component eca_wr_time is
    port(
      clk_i    : in  std_logic;
      rst_n_i  : in  std_logic;
      tai_i    : in  std_logic_vector(39 downto 0);
      cycles_i : in  std_logic_vector(27 downto 0);
      time_o   : out t_time);
  end component;
  
  -- Convert WB writes into inbound ECA events
  component eca_wb_event is
    port(
      w_clk_i   : in  std_logic;
      w_rst_n_i : in  std_logic;
      w_slave_i : in  t_wishbone_slave_in;
      w_slave_o : out t_wishbone_slave_out;
      e_clk_i   : in  std_logic;
      e_rst_n_i : in  std_logic;
      e_stb_o   : out std_logic;
      e_stall_i : in  std_logic;
      e_event_o : out t_event;
      e_param_o : out t_param;
      e_tef_o   : out t_tef;
      e_time_o  : out t_time;
      e_index_i : in  std_logic_vector(7 downto 0));
  end component;
  
end eca_pkg;

package body eca_pkg is

  function f_name(name : string) return t_name is
    variable result : t_name;
  begin
    for i in 1 to 63 loop
      if i > name'length then
        result(64-i) := (others => '0');
      else
        result(64-i) := std_logic_vector(to_unsigned(character'pos(name(i)), 7));
      end if;
    end loop;
    result(0) := (others => '0');
    return result;
  end f_name;
  
end eca_pkg;
