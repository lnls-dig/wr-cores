--! @file eca_internals_pkg.vhd
--! @brief Event-Condition-Action internal components package
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
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
use ieee.math_real.all;

use work.wishbone_pkg.all;

package eca_internals_pkg is
  
  constant c_event_bits : natural := 64;
  constant c_param_bits : natural := 64;
  constant c_tag_bits   : natural := 32;
  constant c_tef_bits   : natural := 32;
  constant c_time_bits  : natural := 64;
  constant c_num_bits   : natural :=  8; -- max allowed is 16
  constant c_code_bits  : natural :=  3;
  
  subtype t_event is std_logic_vector(c_event_bits-1 downto 0);
  subtype t_param is std_logic_vector(c_param_bits-1 downto 0);
  subtype t_tag   is std_logic_vector(c_tag_bits-1   downto 0);
  subtype t_tef   is std_logic_vector(c_tef_bits-1   downto 0);
  subtype t_time  is std_logic_vector(c_time_bits-1  downto 0);
  subtype t_num   is std_logic_vector(c_num_bits-1   downto 0);
  subtype t_code  is std_logic_vector(c_code_bits-1  downto 0);
  
  type t_stream is record
    stb   : std_logic;
    event : t_event;
    param : t_param;
    tef   : t_tef;
    time  : t_time;
  end record t_stream;
  
  type t_channel is record
    valid    : std_logic;
    delayed  : std_logic;
    conflict : std_logic;
    late     : std_logic;
    early    : std_logic;
    num      : t_num;
    event    : t_event;
    param    : t_param;
    tag      : t_tag;
    tef      : t_tef;
    deadline : t_time;
    executed : t_time;
  end record t_channel;
  
  constant c_idle_channel : t_channel := (
    '0', '0', '0', '0', '0',
    (others => '0'),
    (others => '0'),
    (others => '0'),
    (others => '0'),
    (others => '0'),
    (others => '0'),
    (others => '0'));
  
  type t_nat_array     is array(natural range <>) of natural;
  type t_stream_array  is array(natural range <>) of t_stream;
  type t_channel_array is array(natural range <>) of t_channel;
  type t_num_array     is array(natural range <>) of t_num;
  type t_code_array    is array(natural range <>) of t_code;
  
  type t_eca_matrix is array(natural range <>, natural range <>) of std_logic;

  function f_eca_active_high(x : boolean) return std_logic;
  function f_eca_and(x : std_logic_vector) return std_logic;
  function f_eca_or(x : std_logic_vector) return std_logic;
  function f_eca_eq(x, y : std_logic_vector) return std_logic;
  function f_eca_safe(x : std_logic_vector) return std_logic;
  function f_eca_mux(m : std_logic; x, y : std_logic) return std_logic;
  function f_eca_mux(m : std_logic; x, y : std_logic_vector) return std_logic_vector;
  function f_eca_log2(x : natural) return natural;
  function f_eca_log2_min1(x : natural) return natural;
  function f_eca_max(a, b : natural) return natural;
  function f_eca_ripple(a, b : std_logic_vector; c : std_logic) return std_logic_vector;
  function f_eca_gray_encode(x : std_logic_vector) return std_logic_vector;
  function f_eca_gray_decode(x : std_logic_vector; step : natural) return std_logic_vector;
  function f_eca_add(x : std_logic_vector; y : integer) return std_logic_vector;
  function f_eca_add(x : std_logic_vector; y : std_logic) return std_logic_vector;
  function f_eca_add(x, y : std_logic_vector) return std_logic_vector;
  function f_eca_sub(x, y : std_logic_vector) return std_logic_vector;
  function f_eca_delta(x, previous, current : std_logic_vector) return std_logic_vector;
  function f_eca_1hot_decode(x : std_logic_vector) return std_logic_vector;
  
  procedure p_eca_uniform(variable s1, s2 : inout positive; variable x : inout std_logic_vector);
  procedure p_eca_uniform(variable s1, s2 : inout positive; variable x : inout std_logic);
  
  component eca_bitonic_swap is
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
  end component;
  
  component eca_bitonic_helper is
    generic(
      g_log_size : natural;
      g_wide     : natural;
      g_order    : boolean); -- true = smallest first
    port(
      clk_i   : in  std_logic;
      rst_n_i : in  std_logic;
      en_i    : in  std_logic;
      nums_i  : in  t_eca_matrix(2**g_log_size-1 downto 0, g_wide-1 downto 0);
      nums_o  : out t_eca_matrix(2**g_log_size-1 downto 0, g_wide-1 downto 0));
  end component;

  component eca_bitonic is
    generic(
      g_log_size : natural;
      g_wide     : natural;
      g_order    : boolean); -- true = smallest first
    port(
      clk_i   : in  std_logic;
      rst_n_i : in  std_logic := '1';
      en_i    : in  std_logic := '1';
      nums_i  : in  t_eca_matrix(2**g_log_size-1 downto 0, g_wide-1 downto 0);
      nums_o  : out t_eca_matrix(2**g_log_size-1 downto 0, g_wide-1 downto 0));
  end component;
  
  component eca_bitonic_tb is
    generic(
      g_case  : natural := 0);
    port(
      clk_i   : in std_logic;
      rst_n_i : in std_logic);
  end component;

  component eca_compact is
    generic(
      g_size : natural;
      g_wide : natural;
      g_step : natural := 1);
    port(
      clk_i   : in  std_logic;
      rst_n_i : in  std_logic;
      holes_i : in  std_logic_vector(g_size-1 downto 0) := (others => '-');
      valid_i : in  std_logic_vector(g_size-1 downto 0);
      valid_o : out std_logic_vector(g_size-1 downto 0);
      data_i  : in  t_eca_matrix(g_size-1 downto 0, g_wide-1 downto 0);
      data_o  : out t_eca_matrix(g_size-1 downto 0, g_wide-1 downto 0));
  end component;

  component eca_compact_tb is
    generic(
      g_case  : natural := 0);
    port(
      clk_i   : in std_logic;
      rst_n_i : in std_logic);
  end component;

  -- Registers its inputs. Async outputs. 
  -- When r_clk_i=w_clk_i, r_data_o is undefined.
  component eca_sdp is
    generic(
      g_addr_bits  : natural;
      g_data_bits  : natural;
      g_bypass     : boolean;
      g_dual_clock : boolean := false);
    port(
      r_clk_i  : in  std_logic;
      r_addr_i : in  std_logic_vector(g_addr_bits-1 downto 0);
      r_data_o : out std_logic_vector(g_data_bits-1 downto 0);
      w_clk_i  : in  std_logic;
      w_en_i   : in  std_logic;
      w_addr_i : in  std_logic_vector(g_addr_bits-1 downto 0);
      w_data_i : in  std_logic_vector(g_data_bits-1 downto 0));
  end component;

  component eca_tdp is
    generic(
      g_addr_bits  : natural;
      g_data_bits  : natural);
    port(
      clk_i    : in  std_logic;
      a_wen_i  : in  std_logic;
      a_addr_i : in  std_logic_vector(g_addr_bits-1 downto 0);
      a_data_i : in  std_logic_vector(g_data_bits-1 downto 0); 
      a_data_o : out std_logic_vector(g_data_bits-1 downto 0);
      b_wen_i  : in  std_logic;
      b_addr_i : in  std_logic_vector(g_addr_bits-1 downto 0);
      b_data_i : in  std_logic_vector(g_data_bits-1 downto 0); 
      b_data_o : out std_logic_vector(g_data_bits-1 downto 0));
  end component;
  
  -- Async logic applied inputs. Async outputs. 
  -- When x_en_i='1' and x_ack_o='1' the address on x_addr_i will be read with
  -- output available on x_data_o the next cycle. Whatever is fed into x_data_i
  -- on that cycle will be written back to the same address.
  -- The core guarantees read-new-data ordering between and within both ports.
  component eca_rmw is
    generic(
      g_addr_bits  : natural := 8;
      g_data_bits  : natural := 8);
    port(
      clk_i     : in  std_logic;
      rst_n_i   : in  std_logic;
      a_en_i    : in  std_logic;
      a_ack_o   : out std_logic; -- a has priority, so a_ack_o=a_en_i
      a_addr_i  : in  std_logic_vector(g_addr_bits-1 downto 0);
      a_data_o  : out std_logic_vector(g_data_bits-1 downto 0);
      a_data_i  : in  std_logic_vector(g_data_bits-1 downto 0);
      b_en_i    : in  std_logic;
      b_ack_o   : out std_logic;
      b_addr_i  : in  std_logic_vector(g_addr_bits-1 downto 0);
      b_data_o  : out std_logic_vector(g_data_bits-1 downto 0);
      b_data_i  : in  std_logic_vector(g_data_bits-1 downto 0));
  end component;
  
  -- Testbech for eca_rmw
  component eca_rmw_tb is
    generic(
      g_case  : natural := 0);
    port(
      clk_i   : in std_logic;
      rst_n_i : in std_logic);
  end component;
  
  component eca_fifo is
    generic(
      g_log_size : natural;
      g_rows     : natural;
      g_cols     : natural);
    port(
      clk_i   : in  std_logic;
      rst_n_i : in  std_logic;
      push_i  : in  std_logic;
      full_o  : out std_logic;
      data_i  : in  t_eca_matrix(g_rows-1 downto 0, g_cols-1 downto 0);
      pop_i   : in  std_logic;
      valid_o : out std_logic;
      fresh_o : out std_logic;
      data_o  : out t_eca_matrix(g_rows-1 downto 0, g_cols-1 downto 0));
  end component;
  
  component eca_fifo_tb is
    generic(
      g_case  : natural := 0);
    port(
      clk_i   : in std_logic;
      rst_n_i : in std_logic);
  end component;
  
  component eca_piso_fifo is
    generic(
      g_log_size  : natural; -- g_log_size > g_log_ports
      g_log_ports : natural;
      g_width     : natural);
    port(
      clk_i   : in  std_logic;
      rst_n_i : in  std_logic;
      push_i  : in  std_logic_vector(2**g_log_ports-1 downto 0);
      data_i  : in  t_eca_matrix(2**g_log_ports-1 downto 0, g_width-1 downto 0);
      pop_i   : in  std_logic;
      valid_o : out std_logic;
      fresh_o : out std_logic; -- If the record was not delayed in the FIFO
      data_o  : out std_logic_vector(g_width-1 downto 0));
  end component;
  
  component eca_piso_fifo_tb is
    generic(
      g_case  : natural := 0);
    port(
      clk_i   : in std_logic;
      rst_n_i : in std_logic);
  end component;
  
  component eca_free is
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
  end component;
  
  -- Testbech for eca_free
  component eca_free_tb is
    generic(
      g_case  : natural := 0);
    port(
      clk_i   : in std_logic;
      rst_n_i : in std_logic);
  end component;
  
  component eca_data is
    generic(
      g_log_size : natural);
    port(
      clk_i   : in  std_logic;
      rst_n_i : in  std_logic;
      w_en_i  : in  std_logic;
      w_idx_i : in  std_logic_vector(g_log_size-1 downto 0);
      w_dat_i : in  t_channel;
      r_idx_i : in  std_logic_vector(g_log_size-1 downto 0);
      r_dat_o : out t_channel);
  end component;
  
  component eca_scan is
    generic(
      g_ext_size       : natural; -- extra bits tracked by scanner
      g_log_size       : natural; -- 2**g_log_size       = buffer entries
      g_log_multiplier : natural; -- 2**g_log_multiplier = ticks per cycle
      g_log_max_delay  : natural; -- 2**g_log_max_delay  = maximum delay before executed as early
      g_log_latency    : natural);-- 2**g_log_latency    = ticks of calendar delay
    port(
      clk_i        : in  std_logic;
      rst_n_i      : in  std_logic;
      -- Current time, minus some coefficient
      time_i       : in  t_time;
      -- Write port
      wen_i        : in  std_logic;
      stall_o      : out std_logic;
      deadline_i   : in  t_time;
      idx_i        : in  std_logic_vector(g_log_size-1 downto 0);
      ext_i        : in  std_logic_vector(g_ext_size-1 downto 0);
      -- Scan reports ready for calendar (each op is held for two cycles)
      scan_stb_o   : out std_logic;
      scan_late_o  : out std_logic;
      scan_early_o : out std_logic;
      scan_low_o   : out std_logic_vector(g_log_latency-1 downto 0);
      scan_idx_o   : out std_logic_vector(g_log_size   -1 downto 0);
      scan_ext_o   : out std_logic_vector(g_ext_size   -1 downto 0));
  end component;

  -- Testbech for eca_scan
  component eca_scan_tb is
    generic(
      g_case  : natural := 0);
    port(
      clk_i   : in std_logic;
      rst_n_i : in std_logic);
  end component;
  
  component eca_tag_channel is
    generic(
      g_support_io     : boolean := false; -- Should io_o be driven?
      g_never_delayed  : boolean := false; -- Report delayed as valid?
      g_num_channels   : natural :=  1; -- Number of channels emulated by this instance
      g_log_size       : natural :=  8; -- 2**g_log_size = maximum number of pending actions
      g_log_multiplier : natural :=  3; -- 2**g_log_multiplier = ticks per cycle
      g_log_max_delay  : natural := 32; -- 2**g_log_max_delay  = maximum delay before executed as early
      g_log_latency    : natural := 12);-- 2**g_log_latency    = ticks of calendar delay
    port(
      clk_i      : in  std_logic;
      rst_n_i    : in  std_logic;
      -- Timestamps used for pipeline stages
      time_i     : in  t_time;
      -- Push a record to the queue
      overflow_o : out std_logic;
      used_o     : out std_logic_vector(g_log_size downto 0);
      channel_i  : in  t_channel;
      clr_i      : in  std_logic;
      set_i      : in  std_logic;
      -- Pick an action to output while idle
      snoop_i    : in  std_logic_vector(g_log_size-1 downto 0);
      snoop_o    : out t_channel;
      snoop_ok_o : out std_logic;
      -- Free record
      free_i     : in  std_logic;
      index_i    : in  std_logic_vector(g_log_size-1 downto 0);
      -- Output of the channel
      stall_i    : in  std_logic;
      channel_o  : out t_channel;
      index_o    : out std_logic_vector(g_log_size-1 downto 0);
      io_o       : out t_eca_matrix(g_num_channels-1 downto 0, 2**g_log_multiplier-1 downto 0));
  end component;
  
  -- Testbech for eca_tag_channel
  component eca_tag_channel_tb is
    generic(
      g_case  : natural := 0);
    port(
      clk_i   : in std_logic;
      rst_n_i : in std_logic);
  end component;
  
  component eca_channel is
    generic(
      g_support_io     : boolean := false; -- Should io_o be driven?
      g_never_delayed  : boolean := false; -- Report delayed as valid?
      g_num_channels   : natural :=  1; -- Number of channels emulated by this instance (must be >= 1)
      g_log_size       : natural :=  8; -- 2**g_log_size = maximum number of pending actions
      g_log_multiplier : natural :=  3; -- 2**g_log_multiplier = ticks per cycle
      g_log_max_delay  : natural := 32; -- 2**g_log_max_delay  = maximum delay before executed as early
      g_log_latency    : natural := 12; -- 2**g_log_latency    = ticks of calendar delay
      g_log_counter    : natural := 20);-- number of bits in the counters reported
    port(
      clk_i       : in  std_logic;
      rst_n_i     : in  std_logic;
      -- Timestamps used for pipeline stages
      time_i      : in  t_time;
      -- Push a record to the queue
      overflow_o  : out std_logic;
      channel_i   : in  t_channel;
      clr_i       : in  std_logic;
      set_i       : in  std_logic;
      -- Output of the channel
      stall_i     : in  std_logic;
      channel_o   : out t_channel;
      io_o        : out t_eca_matrix(g_num_channels-1 downto 0, 2**g_log_multiplier-1 downto 0);
      -- Bus access ports
      req_clk_i   : in  std_logic;
      req_rst_n_i : in  std_logic;
      req_stb_i   : in  std_logic; -- positive edge triggered
      req_num_i   : in  t_num;
      req_type_i  : in  std_logic_vector(1 downto 0); -- 0=late, 1=early, 2=conflict, 3=delayed
      req_field_i : in  std_logic_vector(3 downto 0); -- See comments at start of eca_channel.vhd
      req_valid_o : out std_logic;
      req_data_o  : out std_logic_vector(31 downto 0);
      -- MSI delivery ports
      msi_clk_i   : in  std_logic;
      msi_rst_n_i : in  std_logic;
      msi_ack_i   : in  std_logic;
      msi_stb_o   : out std_logic;
      msi_code_o  : out t_code; -- See commends at start of eca_channel.vhd
      msi_num_o   : out t_num);
  end component;

  -- Testbech for eca_channel
  component eca_channel_tb is
    generic(
      g_case  : natural := 0);
    port(
      clk_i   : in std_logic;
      rst_n_i : in std_logic);
  end component;
  
  -- Expects registers for inputs. Async outputs.
  -- c1_o is available after 1 cycle (2 once registered)
  -- c2_o, x2_o are available after 2 cycles (3 once registered)
  component eca_adder is
    generic(
      g_data_bits : natural := 64;
      g_parts     : natural := 4);
    port(
      clk_i   : in  std_logic;
      stall_i : in  std_logic := '0';
      a_i     : in  std_logic_vector(g_data_bits-1 downto 0);
      b_i     : in  std_logic_vector(g_data_bits-1 downto 0);
      c_i     : in  std_logic := '0';
      c1_o    : out std_logic;
      x2_o    : out std_logic_vector(g_data_bits-1 downto 0);
      c2_o    : out std_logic);
  end component;
  
  -- Expects a register for inputs. Async output.
  -- c1_o is available after 1 cycle (2 once registered)
  -- c2_o, x2_o are available after 2 cycles (3 once registered)
  component eca_offset is
    generic(
      g_data_bits : natural := 64;
      g_parts     : natural := 4;
      g_offset    : natural := 1);
    port(
      clk_i   : in  std_logic;
      stall_i : in  std_logic := '0';
      a_i     : in  std_logic_vector(g_data_bits-1 downto 0);
      c1_o    : out std_logic;
      x2_o    : out std_logic_vector(g_data_bits-1 downto 0);
      c2_o    : out std_logic);
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
  
  component eca_search is
    generic(
      g_log_table_size : natural := 8);
    port(
      clk_i      : in  std_logic;
      rst_n_i    : in  std_logic;
      -- Accept external events
      e_stb_i    : in  std_logic;
      e_stall_o  : out std_logic;
      e_page_i   : in  std_logic;
      e_event_i  : in  t_event;
      e_param_i  : in  t_param;
      e_tef_i    : in  t_tef;
      e_time_i   : in  t_time;
      -- Feed located event rules to the walker
      w_stb_o    : out std_logic;
      w_stall_i  : in  std_logic;
      w_page_o   : out std_logic;
      w_first_o  : out std_logic_vector(g_log_table_size-1 downto 0);
      w1_event_o : out t_event;
      w1_param_o : out t_param;
      w1_tef_o   : out t_tef;
      w1_time_o  : out t_time;
      -- Access the search table
      t_clk_i    : in  std_logic;
      t_page_i   : in  std_logic;
      t_addr_i   : in  std_logic_vector(g_log_table_size downto 0);
      tw_en_i    : in  std_logic;
      tw_valid_i : in  std_logic;
      tw_first_i : in  std_logic_vector(g_log_table_size-1 downto 0);
      tw_event_i : in  t_event;
      tr_valid_o : out std_logic;
      tr_first_o : out std_logic_vector(g_log_table_size-1 downto 0);
      tr_event_o : out t_event);
  end component;
  
  component eca_walker is
    generic(
      g_log_table_size : natural := 8;
      g_num_channels   : natural := 4);
    port(
      clk_i        : in  std_logic;
      rst_n_i      : in  std_logic;
      -- Feed in an index to scan from search
      b_stb_i      : in  std_logic;
      b_stall_o    : out std_logic;
      b_page_i     : in  std_logic;
      b_first_i    : in  std_logic_vector(g_log_table_size-1 downto 0);
      b1_event_i   : in  t_event; -- all one cycle AFTER the b_stb_i
      b1_param_i   : in  t_param;
      b1_tef_i     : in  t_tef;
      b1_time_i    : in  t_time;
      -- Outputs for the channel queue
      q_channel_o  : out t_channel_array (g_num_channels-1 downto 0);
      -- Write to the table
      t_clk_i      : in  std_logic;
      t_page_i     : in  std_logic;
      t_addr_i     : in  std_logic_vector(g_log_table_size-1 downto 0);
      tw_en_i      : in  std_logic;
      tw_valid_i   : in  std_logic;
      tw_delayed_i : in  std_logic;
      tw_conflict_i: in  std_logic;
      tw_late_i    : in  std_logic;
      tw_early_i   : in  std_logic;
      tw_next_i    : in  std_logic_vector(g_log_table_size-1 downto 0);
      tw_time_i    : in  t_time;
      tw_tag_i     : in  t_tag;
      tw_num_i     : in  t_num;
      tw_channel_i : in  std_logic_vector(f_eca_log2(g_num_channels)-1 downto 0);
      tr_valid_o   : out std_logic;
      tr_delayed_o : out std_logic;
      tr_conflict_o: out std_logic;
      tr_late_o    : out std_logic;
      tr_early_o   : out std_logic;
      tr_next_o    : out std_logic_vector(g_log_table_size-1 downto 0);
      tr_time_o    : out t_time;
      tr_tag_o     : out t_tag;
      tr_num_o     : out t_num;
      tr_channel_o : out std_logic_vector(f_eca_log2(g_num_channels)-1 downto 0));
  end component;
  
  component eca_msi is
    generic(
      g_num_channels : natural :=  1);
    port(
      -- Configuration interface
      c_clk_i        : in  std_logic;
      c_rst_n_i      : in  std_logic;
      c_chan_i       : in  std_logic_vector(7 downto 0);
      c_enable_stb_i : in  std_logic;
      c_enable_i     : in  std_logic;
      c_enable_o     : out std_logic;
      c_target_stb_i : in  std_logic;
      c_target_i     : in  std_logic_vector(31 downto 0);
      c_target_o     : out std_logic_vector(31 downto 0);
      c_stall_o      : out std_logic;
      -- MSI controller
      i_clk_i        : in  std_logic;
      i_rst_n_i      : in  std_logic;
      i_ack_o        : out std_logic_vector(g_num_channels-1 downto 0);
      i_stb_i        : in  std_logic_vector(g_num_channels-1 downto 0);
      i_code_i       : in  t_code_array(g_num_channels-1 downto 0);
      i_num_i        : in  t_num_array(g_num_channels-1 downto 0);
      i_master_i     : in  t_wishbone_master_in;
      i_master_o     : out t_wishbone_master_out);
  end component;

  component eca is
    generic(
      g_channel_types  : t_nat_array;
      g_channel_nums   : t_nat_array;   -- Anything not explicitly set is 1
      g_num_streams    : natural :=  1; -- Number of streams
      g_num_ios        : natural :=  8; -- Number of gpios
      g_log_table_size : natural :=  8; -- 2**g_log_table_size = maximum number of conditions
      g_log_queue_size : natural :=  8; -- 2**g_log_size       = maximum number of pending actions
      g_log_multiplier : natural :=  3; -- 2**g_log_multiplier = ticks per cycle
      g_log_max_delay  : natural := 32; -- 2**g_log_max_delay  = maximum delay before executed as early
      g_log_latency    : natural := 12; -- 2**g_log_latency    = ticks of calendar delay
      g_log_counter    : natural := 20);-- number of bits in the counters reported
    port(
      -- ECA control registers
      c_clk_i     : in  std_logic;
      c_rst_n_i   : in  std_logic;
      c_slave_i   : in  t_wishbone_slave_in;
      c_slave_o   : out t_wishbone_slave_out;
      -- Action clock domain
      a_clk_i     : in  std_logic;
      a_rst_n_i   : in  std_logic;
      a_time_i    : in  t_time;
      -- Input streams (lower index has priority)
      a_stream_i  : in  t_stream_array(g_num_streams-1 downto 0);
      a_stall_o   : out std_logic_vector(g_num_streams-1 downto 0);
      -- Output actions
      a_stall_i   : in  std_logic_vector(g_channel_types'range);
      a_channel_o : out t_channel_array(g_channel_types'range);
      a_io_o      : out t_eca_matrix(g_num_ios-1 downto 0, 2**g_log_multiplier-1 downto 0);
      -- Interrupts that report failure conditions
      i_clk_i     : in  std_logic;
      i_rst_n_i   : in  std_logic;
      i_master_i  : in  t_wishbone_master_in;
      i_master_o  : out t_wishbone_master_out);
  end component;

  component eca_tlu_fsm is
    generic(
      g_history: natural := 10000; -- how many ticks in the past can events come?
      g_stable : natural := 10;
      g_serdes : natural := 8);
    port(
      clk_i    : in  std_logic;
      rst_n_i  : in  std_logic;
      enable_i : in  std_logic;
      stable_i : in  std_logic_vector(g_stable-1 downto 0);
      time_i   : in  t_time;
      gpio_i   : in  std_logic_vector(g_serdes-1 downto 0);
      ack_i    : in  std_logic;
      stb_o    : out std_logic;
      edge_o   : out std_logic;
      time_o   : out t_time);
  end component;

end eca_internals_pkg;

package body eca_internals_pkg is

  function f_eca_and(x : std_logic_vector) return std_logic is
    alias y : std_logic_vector(x'high downto x'low) is x;
    constant c_mid : natural := (y'high + y'low) / 2;
  begin
    if y'length = 0 then return '0'; end if;
    if y'length = 1 then return y(y'low); end if;
    return f_eca_and(y(y'high downto c_mid+1)) and f_eca_and(y(c_mid downto y'low));
  end f_eca_and;
  
  function f_eca_or(x : std_logic_vector) return std_logic is
    alias y : std_logic_vector(x'high downto x'low) is x;
    constant c_mid : natural := (y'high + y'low) / 2;
  begin
    if y'length = 0 then return '1'; end if;
    if y'length = 1 then return y(y'low); end if;
    return f_eca_or(y(y'high downto c_mid+1)) or f_eca_or(y(c_mid downto y'low));
  end f_eca_or;
  
  function f_eca_eq(x, y : std_logic_vector) return std_logic is
  begin
    return f_eca_and(x xnor y);
  end f_eca_eq;
  
  function f_eca_safe(x : std_logic_vector) return std_logic is
  begin
    return f_eca_eq(x, x);
  end f_eca_safe;
  
  function f_eca_mux(m : std_logic; x, y : std_logic) return std_logic is
  begin
    case m is
      when '1' => return x;
      when '0' => return y;
      when others => return 'X';
    end case;
  end f_eca_mux;
  
  function f_eca_mux(m : std_logic; x, y : std_logic_vector) return std_logic_vector is
    constant bad : std_logic_vector(x'range) := (others => 'X');
  begin
    assert (x'length = y'length) report "vector-vector size mismatch" severity failure;
    case m is
      when '1' => return x;
      when '0' => return y;
      when others => return bad;
    end case;
  end f_eca_mux;
  
  function f_eca_active_high(x : boolean) return std_logic is
  begin
    if x then
      return '1';
    else
      return '0';
    end if;
  end f_eca_active_high;

  function f_eca_log2(x : natural) return natural is
    variable rest   : natural := x;
    variable result : natural := 0;
  begin
    while rest > 1 loop
      result := result + 1;
      rest   := (rest + 1) / 2;
    end loop;
    return result;
  end f_eca_log2;
  
  function f_eca_log2_min1(x : natural) return natural is
  begin
    if x <= 1 then return 1; else return f_eca_log2(x); end if;
  end f_eca_log2_min1;
  
  function f_eca_ripple(a, b : std_logic_vector; c : std_logic) return std_logic_vector is
    constant len : natural := a'length;
    variable aw, bw, rw : std_logic_vector(len+1 downto 0);
    variable x : std_logic_vector(len downto 0);
  begin
    aw := "0" & a & c;
    bw := "0" & b & c;
    rw := std_logic_vector(unsigned(aw) + unsigned(bw));
    x := rw(len+1 downto 1);
    return x;
  end f_eca_ripple;
  
  function f_eca_max(a, b : natural) return natural is
  begin
    if a > b then 
      return a; 
    else 
      return b;
    end if;
  end f_eca_max;
  
  function f_eca_gray_encode(x : std_logic_vector) return std_logic_vector is
    variable o : std_logic_vector(x'length downto 0);
  begin
    o := (x & '0') xor ('0' & x);
    return o(x'length downto 1);
  end f_eca_gray_encode;
  
  -- Call with step=1
  function f_eca_gray_decode(x : std_logic_vector; step : natural) return std_logic_vector is
    constant len : natural := x'length;
    alias    y : std_logic_vector(len-1 downto 0) is x;
    variable z : std_logic_vector(len-1 downto 0) := (others => '0');
  begin
    if step >= len then
      return y;
    else
      z(len-step-1 downto 0) := y(len-1 downto step);
      return f_eca_gray_decode(y xor z, step+step);
    end if;
  end f_eca_gray_decode;

  function f_eca_add(x : std_logic_vector; y : integer) return std_logic_vector is
  begin
    if y < 0 then
      return std_logic_vector(unsigned(x) - to_unsigned(-y, x'length));
    else
      return std_logic_vector(unsigned(x) + to_unsigned(y, x'length));
    end if;
  end function;
  
  function f_eca_add(x : std_logic_vector; y : std_logic) return std_logic_vector is
  begin
    return std_logic_vector(unsigned(x) + ("" & y));
  end function;
  
  function f_eca_add(x, y : std_logic_vector) return std_logic_vector is
  begin
    return std_logic_vector(unsigned(x) + unsigned(y));
  end function;
  
  function f_eca_sub(x, y : std_logic_vector) return std_logic_vector is
  begin
    return std_logic_vector(unsigned(x) - unsigned(y));
  end function;
  
  function f_eca_delta(x, previous, current : std_logic_vector) return std_logic_vector is
  begin
    return std_logic_vector(unsigned(x) + (unsigned(current) - unsigned(previous)));
  end function;
  
  function f_eca_1hot_decode(x : std_logic_vector) return std_logic_vector is
    variable result : std_logic_vector(f_eca_log2_min1(x'length)-1 downto 0);
    variable picked : std_logic_vector(x'range);
    variable step   : natural := 1;
  begin
    for i in result'low to result'high loop
      picked := (others => '0');
      for j in picked'range loop
        if (j / step) mod 2 = 1 then
          picked(j) := x(j);
        end if;
      end loop;
      result(i) := f_eca_or(picked);
      step := step + step;
    end loop;
    return result;
  end f_eca_1hot_decode;
  
  procedure p_eca_uniform(variable s1, s2 : inout positive; variable x : inout std_logic_vector) is
    constant c_take_bits : natural := 30;
    alias y : std_logic_vector(x'length-1 downto 0) is x;
    variable raw : real;
    variable int : integer;
    variable vec : std_logic_vector(c_take_bits-1 downto 0);
  begin
    for i in 0 to (y'length-1)/c_take_bits loop
      uniform(s1, s2, raw);
      int := integer(trunc(raw * real(2**c_take_bits)));
      vec := std_logic_vector(to_unsigned(int, c_take_bits));
      if (i+1)*c_take_bits > y'high then
        y(y'high downto i*c_take_bits) := vec(y'high-i*c_take_bits downto 0);
      else
        y((i+1)*c_take_bits-1 downto i*c_take_bits) := vec;
      end if;
    end loop;
  end procedure;
  
  procedure p_eca_uniform(variable s1, s2 : inout positive; variable x : inout std_logic) is
    variable result : std_logic_vector(0 downto 0);
  begin
    p_eca_uniform(s1, s2, result);
    x := result(0);
  end procedure;
  
end eca_internals_pkg;
