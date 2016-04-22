--! @file eca_channel.vhd
--! @brief ECA Action Channel
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component receives actions to be executed, in any order.
--! It outputs actions when their deadline is due, essentially sorting them.
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

use work.eca_internals_pkg.all;

-- req_field_i selects these values
--   ====== Indexed by req_num_i and req_type_i =====
--   0: Event ID bits 63-32
--   1: Event ID bits 31- 0
--   2: Param    bits 63-32
--   3: Param    bits 31- 0
--   4: Tag
--   5: TEF
--   6: Scheduled time bits 63-32
--   7: Scheduled time bits 31- 0
--   8: Executed time  bits 63-32
--   9: Executed time  bits 31- 0
--  11: Number of errors (clears to 0, new MSI once non-zero, releases fields 0-9)
--  ====== Indexed by req_num_i =====
--  12: Number of valid  (clears to 0, new MSI once non-zero)
--  13: Number of overflows (clears to 0, new MSI once non-zero)
--  ====== Global =====
--  14: Full<<16|MaxFull    (new MSI when MaxFull changes)
--  15: Full<<16|MaxFull    (clears Maxfull and immediately generates a new MSI)

-- msi_code_o has these values
--  ===== Associated msi_num_o =====
--  0: late     action
--  1: early    action
--  2: conflict action
--  3: delayed  action
--  4: valid    action
--  5: overflow count changed
--  ===== msi_num_o is 'X' =====
--  6: MaxFull changed

entity eca_channel is
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
end eca_channel;

architecture rtl of eca_channel is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  constant c_count_bits : natural := g_log_counter;
  constant c_valid_bits : natural := f_eca_log2_min1(g_num_channels);
  constant c_saved_bits : natural := 2 + c_valid_bits;
  constant c_data_bits  : natural := g_log_size + c_count_bits + 64;
  constant c_num_bits   : natural := f_eca_log2_min1(g_num_channels);
  
  constant c_zero : std_logic_vector(c_count_bits-1 downto 0) := (others => '0');
  
  signal r_wipe : std_logic_vector(c_saved_bits downto 0) := (others => '0');
  signal s_safe : std_logic;
  
  signal s_channel_i: t_channel;
  signal s_channel_o: t_channel;
  signal s_snoop    : t_channel;
  signal s_set_used : std_logic;
  signal s_used     : std_logic_vector(g_log_size   downto 0);
  signal r_used     : std_logic_vector(g_log_size   downto 0);
  signal s_index    : std_logic_vector(g_log_size-1 downto 0);
  signal r_index    : std_logic_vector(g_log_size-1 downto 0);
  signal s_num      : std_logic_vector(c_num_bits-1 downto 0);
  signal s_snoop_ok : std_logic;
  
  signal s_free_stb : std_logic;
  signal r_free_stb : std_logic := '0';
  signal s_free_idx : std_logic_vector(g_log_size-1 downto 0);
  signal r_free_idx : std_logic_vector(g_log_size-1 downto 0);
  
  signal s_error    : std_logic;
  signal s_final    : std_logic;
  signal r_final    : std_logic := '0';
  signal s_busy     : std_logic;
  signal r_busy     : std_logic := '0';
  signal s_steal    : std_logic;
  signal s_code     : std_logic_vector(1 downto 0);
  signal s_atom_free: std_logic;
  signal s_late_free: std_logic;

  -- Control of the error counter memory (late/early/delayed/conflict)
  signal s_wen      : std_logic;
  signal s_ridx     : std_logic_vector(c_saved_bits-1 downto 0);
  signal r_ridx     : std_logic_vector(c_saved_bits-1 downto 0);
  signal s_widx     : std_logic_vector(c_saved_bits-1 downto 0);
  signal s_data_i   : std_logic_vector(c_data_bits-1 downto 0);
  signal s_data_o   : std_logic_vector(c_data_bits-1 downto 0);
  signal s_count_o  : std_logic_vector(c_count_bits-1 downto 0);
  signal s_count_i  : std_logic_vector(c_count_bits-1 downto 0);
  signal s_index_o  : std_logic_vector(g_log_size-1 downto 0);
  signal s_index_i  : std_logic_vector(g_log_size-1 downto 0);
  signal r_hold     : std_logic;
  signal r_time     : t_time;
  signal s_time_o   : t_time;
  signal s_time_i   : t_time;
  signal s_zero     : std_logic;
  
  -- Control of the valid counter memory
  signal s_repeat   : std_logic;
  signal r_repeat   : std_logic := '0';
  signal s_valid    : std_logic;
  signal r_valid    : std_logic := '0';
  signal s_val_ridx : std_logic_vector(c_valid_bits-1 downto 0);
  signal r_val_ridx : std_logic_vector(c_valid_bits-1 downto 0);
  signal s_val_widx : std_logic_vector(c_valid_bits-1 downto 0);
  signal s_val_wen  : std_logic;
  signal s_val_data_i : std_logic_vector(c_count_bits-1 downto 0);
  signal s_val_data_o : std_logic_vector(c_count_bits-1 downto 0);
  
  -- Control of the overflow counter memory
  signal s_overflow : std_logic;
  signal r_overflow : std_logic := '0';
  signal s_ovr_ridx : std_logic_vector(c_valid_bits-1 downto 0);
  signal r_ovr_ridx : std_logic_vector(c_valid_bits-1 downto 0);
  signal s_ovr_widx : std_logic_vector(c_valid_bits-1 downto 0);
  signal s_ovr_wen  : std_logic;
  signal s_ovr_data_i : std_logic_vector(c_count_bits-1 downto 0);
  signal s_ovr_data_o : std_logic_vector(c_count_bits-1 downto 0);
  
  -- Wishbone padded output registers
  signal s_val_count    : std_logic_vector(31 downto 0)             := (others => '0');
  signal s_over_count   : std_logic_vector(31 downto 0)             := (others => '0');
  signal s_used_output  : std_logic_vector(31 downto 0)             := (others => '0');
  signal r_most_used    : std_logic_vector(g_log_size downto 0)     := (others => '0');
  
  -- Latched by bus_clk_i, but held until request is complete
  signal s_req_in     : std_logic;
  signal rs_req_num   : std_logic_vector(c_num_bits-1 downto 0);
  signal rs_req_type  : std_logic_vector(1 downto 0);
  signal rs_req_field : std_logic_vector(3 downto 0);
  signal s_req_rok    : std_logic;
  signal r_req_rok    : std_logic := '1';
  signal rc_req_num   : std_logic_vector(c_num_bits-1 downto 0);
  signal rc_req_type  : std_logic_vector(1 downto 0);
  signal rc_req_field : std_logic_vector(3 downto 0);
  signal s_error_field: std_logic;
  signal s_valid_field: std_logic;
  signal s_over_field : std_logic;
  signal s_ack_field  : std_logic;
  signal s_full_field : std_logic;
  signal s_valid_clear: std_logic;
  signal s_over_clear : std_logic;
  signal s_ack_clear  : std_logic;
  signal s_full_clear : std_logic;
  
  -- Request signalling
  signal r_req_old  : std_logic := '0'; -- req_clk
  signal r_req_xor1 : std_logic := '0'; -- req_clk
  signal r_req_xor2 : std_logic := '0'; -- clk
  signal r_req_xor3 : std_logic := '0'; -- clk
  signal r_req_xor4 : std_logic := '0'; -- clk
  
  -- Remember the index requested
  signal r_req_aok : std_logic := '1'; -- address ok
  signal r_req_sok : std_logic := '1'; -- snoop ok
  signal s_req_idx : std_logic;
  signal r_req_idx : std_logic_vector(g_log_size-1 downto 0);
  signal r_req_cnt : std_logic_vector(c_count_bits-1 downto 0);
  signal s_req_cnt : std_logic_vector(31 downto 0) := (others => '0');
  signal r_req_time: t_time;
  
  -- Remember the data requested
  signal s_req_dok : std_logic;
  signal r_req_dok : std_logic := '1';
  signal s_req_dat : std_logic_vector(31 downto 0);
  signal s_req_val : std_logic;
  signal rc_req_dat: std_logic_vector(31 downto 0);
  
  -- Synchronize the output strobe
  signal s_req_ack  : std_logic;
  signal r_req_ack  : std_logic := '1';
  signal r_req_xor5 : std_logic := '0'; -- clk
  signal r_req_xor6 : std_logic := '0'; -- req_clk
  signal r_req_xor7 : std_logic := '0'; -- req_clk
  signal r_req_xor8 : std_logic := '0'; -- req_clk
  signal r_req_out  : std_logic := '0'; -- req_clk
  
  -- Final output registers
  signal s_req_ook  : std_logic;
  signal rs_req_dat : std_logic_vector(31 downto 0);
  
  -- Saturated increment
  function f_increment(x : std_logic_vector; a : std_logic := '1') return std_logic_vector is
  begin
    return f_eca_mux(f_eca_and(x), x, f_eca_add(x, a));
  end f_increment;
  
  -- Interrupt vector processing
  signal r_raise_err   : std_logic_vector(g_num_channels*4-1 downto 0) := (others => '0');
  signal r_raise_val   : std_logic_vector(g_num_channels*1-1 downto 0) := (others => '0');
  signal r_raise_over  : std_logic_vector(g_num_channels*1-1 downto 0) := (others => '0');
  signal r_raise_full  : std_logic := '0';
  signal s_raised      : std_logic_vector(g_num_channels*6 downto 0);
  signal r_mask_err    : std_logic_vector(g_num_channels*4-1 downto 0) := (others => '0');
  signal r_mask_val    : std_logic_vector(g_num_channels*1-1 downto 0) := (others => '0');
  signal r_mask_over   : std_logic_vector(g_num_channels*1-1 downto 0) := (others => '0');
  signal r_mask_full   : std_logic := '0';
  signal s_masked      : std_logic_vector(g_num_channels*6 downto 0);
  signal s_pending     : std_logic_vector(g_num_channels*6 downto 0);
  signal s_selected    : std_logic_vector(g_num_channels*6 downto 0);
  signal r_selected    : std_logic_vector(g_num_channels*6 downto 0);
  signal s_select_err  : std_logic_vector(g_num_channels*4-1 downto 0);
  signal s_select_val  : std_logic_vector(g_num_channels*1-1 downto 0);
  signal s_select_over : std_logic_vector(g_num_channels*1-1 downto 0);
  signal s_select_full : std_logic;
  signal s_msi_err_num : std_logic_vector(f_eca_log2(g_num_channels)+1 downto 0);
  signal s_msi_val_num : std_logic_vector(c_num_bits-1 downto 0);
  signal s_msi_ovr_num : std_logic_vector(c_num_bits-1 downto 0);
  
  -- Interrupt reporting
  signal sc_msi_rdy  : std_logic;
  signal rc_msi_rdy  : std_logic := '0';
  signal rc_msi_xor  : std_logic := '0';
  signal rc_msi_code : t_code;
  signal rc_msi_num  : std_logic_vector(c_num_bits-1 downto 0);
  signal rs_msi_xor1 : std_logic := '0';
  signal rs_msi_xor2 : std_logic := '0';
  signal rs_msi_xor3 : std_logic := '0';
  signal ss_msi_rdy  : std_logic;
  signal rs_msi_rdy  : std_logic := '0';
  signal rs_msi_code : t_code;
  signal rs_msi_num  : std_logic_vector(c_num_bits-1 downto 0);
  signal rs_ack_xor  : std_logic := '0';
  signal rc_ack_xor1 : std_logic := '0';
  signal rc_ack_xor2 : std_logic := '0';
  signal rc_ack_xor3 : std_logic := '0';
  signal sc_ack      : std_logic;

begin

  reset : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_wipe <= (others => '0');
    elsif rising_edge(clk_i) then
      r_wipe <= f_eca_add(r_wipe, not s_safe);
    end if;
  end process;
  s_safe <= r_wipe(r_wipe'high);

  channel : eca_tag_channel
    generic map(
      g_support_io     => g_support_io,
      g_never_delayed  => g_never_delayed,
      g_num_channels   => g_num_channels,
      g_log_size       => g_log_size,
      g_log_multiplier => g_log_multiplier,
      g_log_max_delay  => g_log_max_delay,
      g_log_latency    => g_log_latency)
    port map(
      clk_i      => clk_i,
      rst_n_i    => rst_n_i,
      time_i     => time_i,
      used_o     => s_used,
      overflow_o => s_overflow,
      channel_i  => s_channel_i,
      clr_i      => clr_i,
      set_i      => set_i,
      snoop_i    => r_req_idx,
      snoop_o    => s_snoop,
      snoop_ok_o => s_snoop_ok,
      free_i     => r_free_stb,
      index_i    => r_free_idx,
      stall_i    => stall_i,
      channel_o  => s_channel_o,
      index_o    => s_index,
      io_o       => io_o);
  
  channel_o <= s_channel_o;
  
  -- Block access until wipe is complete
  overflow_o           <= s_overflow or (channel_i.valid and not s_safe);
  s_channel_i.valid    <= channel_i.valid and s_safe;
  s_channel_i.delayed  <= channel_i.delayed;
  s_channel_i.conflict <= channel_i.conflict;
  s_channel_i.late     <= channel_i.late;
  s_channel_i.early    <= channel_i.early;
  s_channel_i.num      <= channel_i.num;
  s_channel_i.event    <= channel_i.event;
  s_channel_i.param    <= channel_i.param;
  s_channel_i.tag      <= channel_i.tag;
  s_channel_i.tef      <= channel_i.tef;
  s_channel_i.deadline <= channel_i.deadline;
  s_channel_i.executed <= (others => '0');
  
  -- Goal is to intercept error conditions and record their indices
  saved : eca_sdp
    generic map(
      g_addr_bits  => c_saved_bits,
      g_data_bits  => c_data_bits,
      g_bypass     => true,
      g_dual_clock => false)
    port map(
      r_clk_i  => clk_i,
      r_addr_i => s_ridx,
      r_data_o => s_data_o,
      w_clk_i  => clk_i,
      w_en_i   => s_wen,
      w_addr_i => s_widx,
      w_data_i => s_data_i);
  
  s_error <= s_channel_o.late or s_channel_o.early or s_channel_o.conflict or s_channel_o.delayed;
  s_final <= f_eca_mux(s_channel_o.valid, not stall_i, s_error); -- Index should be freed (if not stolen)
  s_busy  <= s_error and s_final; -- Is this the final time we see this error?
  s_steal <= r_busy and s_zero; -- Record this error for software diagnosis

  -- If the request was to free the error, do it on final ack
  s_late_free <= s_req_ack and s_error_field;
  
  -- r_final and s_late_free are mutually exclusive
  s_free_stb <= (r_final and not s_steal) or (s_late_free and f_eca_or(r_req_cnt));
  s_free_idx <= f_eca_mux(r_final, r_index, r_req_idx);
  
  -- code: 0=late, 1=early, 2=conflict, 3=delayed
  s_code(0) <= s_channel_o.early    or s_channel_o.delayed;
  s_code(1) <= s_channel_o.conflict or s_channel_o.delayed;
  
  s_num  <= s_channel_o.num(s_num'range);
  s_ridx <= f_eca_mux(s_busy, s_num & s_code, rc_req_num & rc_req_type);
  s_widx <= f_eca_mux(s_safe, r_ridx, r_wipe(s_widx'range));
  
  s_time_o  <= s_data_o(c_data_bits-1 downto g_log_size+c_count_bits);
  s_count_o <= s_data_o(g_log_size+c_count_bits-1 downto g_log_size);
  s_index_o <= s_data_o(g_log_size-1 downto 0);
  s_zero    <= not f_eca_or(s_count_o);
  
  -- Atomically read the current counter+index, while atomically wiping them out for new errors
  -- Note: for this to work, we need that the table is bypassed
  s_atom_free <= s_req_idx and s_error_field;

  s_wen     <= r_busy or s_atom_free or not s_safe; -- r_busy and s_atom_free are mutually exclusive
  s_count_i <= f_eca_mux(r_busy, f_increment(s_count_o), c_zero);
  s_index_i <= f_eca_mux(s_zero, r_index, s_index_o);
  s_time_i  <= f_eca_mux(s_zero, r_time,  s_time_o);
  s_data_i  <= s_time_i & s_count_i & s_index_i;
  
  -- Track the number of valid actions for each subchannel
  valid : eca_sdp
    generic map(
      g_addr_bits  => c_valid_bits,
      g_data_bits  => c_count_bits,
      g_bypass     => true,
      g_dual_clock => false)
    port map(
      r_clk_i  => clk_i,
      r_addr_i => s_val_ridx,
      r_data_o => s_val_data_o,
      w_clk_i  => clk_i,
      w_en_i   => s_val_wen,
      w_addr_i => s_val_widx,
      w_data_i => s_val_data_i);
  
  s_repeat <= s_channel_o.valid and stall_i;
  s_valid  <= s_channel_o.valid and not r_repeat;
  s_val_ridx <= f_eca_mux(s_valid, s_num, rc_req_num);
  s_val_widx <= f_eca_mux(s_safe, r_val_ridx, r_wipe(s_val_widx'range));
  s_val_wen  <= r_valid or s_valid_clear or not s_safe; -- r_valid and s_valid_clear are mutually exclusive
  s_val_data_i <= f_eca_mux(r_valid, f_increment(s_val_data_o), c_zero);
  
  -- Track the number of overflowing actions for each subchannel
  overflow : eca_sdp
    generic map(
      g_addr_bits  => c_valid_bits,
      g_data_bits  => c_count_bits,
      g_bypass     => true,
      g_dual_clock => false)
    port map(
      r_clk_i  => clk_i,
      r_addr_i => s_ovr_ridx,
      r_data_o => s_ovr_data_o,
      w_clk_i  => clk_i,
      w_en_i   => s_ovr_wen,
      w_addr_i => s_ovr_widx,
      w_data_i => s_ovr_data_i);
  
  s_ovr_ridx <= f_eca_mux(s_overflow, s_channel_i.num(rc_req_num'range), rc_req_num);
  s_ovr_widx <= f_eca_mux(s_safe, r_ovr_ridx, r_wipe(s_ovr_widx'range));
  s_ovr_wen  <= r_overflow or s_over_clear or not s_safe; -- r_overflow and s_over_clear are mutually exclusive
  s_ovr_data_i <= f_eca_mux(s_safe and r_overflow, f_increment(s_ovr_data_o), c_zero);
  
  -- Zero-extend r_req_cnt, s_ovr_data_o, s_val_data_o
  s_req_cnt(r_req_cnt'range) <= r_req_cnt;
  s_val_count(s_val_data_o'range) <= s_val_data_o;
  s_over_count(s_ovr_data_o'range) <= s_ovr_data_o;
  s_used_output(g_log_size+16 downto 16) <= r_used;
  s_used_output(g_log_size+ 0 downto  0) <= r_most_used;
  
  -- Fields with both high bits set are global channel parameters
  s_error_field <= f_eca_eq(rc_req_field, "1011");
  s_valid_field <= f_eca_eq(rc_req_field, "1100");
  s_over_field  <= f_eca_eq(rc_req_field, "1101");
  s_ack_field   <= f_eca_eq(rc_req_field, "1110");
  s_full_field  <= f_eca_eq(rc_req_field, "1111");
  
  with rc_req_field select
  s_req_dat <=
    s_snoop.event   (63 downto 32) when "0000",
    s_snoop.event   (31 downto  0) when "0001",
    s_snoop.param   (63 downto 32) when "0010",
    s_snoop.param   (31 downto  0) when "0011",
    s_snoop.tag     (31 downto  0) when "0100",
    s_snoop.tef     (31 downto  0) when "0101",
    s_snoop.deadline(63 downto 32) when "0110",
    s_snoop.deadline(31 downto  0) when "0111",
    r_req_time      (63 downto 32) when "1000",
    r_req_time      (31 downto  0) when "1001",
    -- reserved                    when "1010",
    s_req_cnt                      when "1011",
    s_val_count                    when "1100",
    s_over_count                   when "1101",
    s_used_output                  when "1110",
    s_used_output                  when "1111",
    (others => 'X') when others;
  
  with rc_req_field select
  s_req_val <=
    s_snoop_ok     when "0000",
    s_snoop_ok     when "0001",
    s_snoop_ok     when "0010",
    s_snoop_ok     when "0011",
    s_snoop_ok     when "0100",
    s_snoop_ok     when "0101",
    s_snoop_ok     when "0110",
    s_snoop_ok     when "0111",
    '1'            when "1000", -- exec0 (saved)
    '1'            when "1001", -- exec1 (saved)
    -- reserved    when "1010",
    '1'            when "1011", -- count (saved)
    not r_valid    when "1100", -- #exec (valid)
    not r_overflow when "1101", -- #overflow (reg)
    '1'            when "1110", -- #used (reg)
    '1'            when "1111", -- #used (reg)
    '1'            when others;
  
  -- Clock enable for registers going from req_clk => clk (req_clk side)
  s_req_in <= not r_req_old and req_stb_i;
  -- Clock enable for registers going from req_clk => clk (clk side)
  s_req_rok <= r_req_xor4 xor r_req_xor3;
  -- Clock enable for count register going from clk => req_clk (clk side)
  s_req_idx <= r_req_rok and not r_req_aok and not r_busy;
  -- Clock enable for data register going from clk => req_clk (clk side)
  s_req_dok <= s_req_val and r_req_sok and not r_req_dok;
  -- Only allow the request to complete when there's a slot we could potentially free in
  s_req_ack <= r_req_dok and not r_final and not r_req_ack;
  -- Clock enable for the registers going from clk => req_clk (req_clk side)
  s_req_ook <= r_req_xor8 xor r_req_xor7;
  
  s_valid_clear <= s_req_dok and s_valid_field;
  s_over_clear  <= s_req_dok and s_over_field;
  s_ack_clear   <= s_req_dok and s_ack_field;
  s_full_clear  <= s_req_dok and s_full_field;
  
  s_set_used <= f_eca_active_high(unsigned(s_used) > unsigned(r_most_used)) when f_eca_safe(s_used)='1' else 'X';
  
  stat_control : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_free_stb <= '0';
      r_busy     <= '0';
      r_final    <= '0';
      r_repeat   <= '0';
      r_valid    <= '0';
      r_overflow <= '0';
      r_most_used    <= (others => '0');
    elsif rising_edge(clk_i) then
      r_free_stb <= s_free_stb;
      r_busy     <= s_busy;
      r_final    <= s_final;
      r_repeat   <= s_repeat;
      r_valid    <= s_valid;
      r_overflow <= s_overflow;
      
      if (s_set_used or s_full_clear) = '1' then
        r_most_used <= s_used;
      end if;
    end if;
  end process;
  
  stat_bulk : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      r_ridx      <= s_ridx;
      r_val_ridx  <= s_val_ridx;
      r_ovr_ridx  <= s_ovr_ridx;
      r_index     <= s_index;
      r_free_idx  <= s_free_idx;
      
      -- delay s_used, so that r_used <= r_most_used ALWAYS
      r_used <= s_used;
      
      -- Delay time by one cycle, to match when the error came out
      -- Hold the time stationary in case there is a stall; record when we first reported it
      r_hold <= stall_i and s_channel_o.valid;
      if r_hold = '0' then
        r_time <= time_i;
      end if;
    end if;
  end process;
  
  in_control : process(req_clk_i, req_rst_n_i) is
  begin
    if req_rst_n_i = '0' then
      r_req_old  <= '0';
      r_req_xor1 <= '0';
    elsif rising_edge(req_clk_i) then
      if req_stb_i = '1' then
        r_req_old <= '1';
      end if;
      if r_req_out = '1' then
        r_req_old <= '0';
      end if;
      r_req_xor1 <= r_req_xor1 xor s_req_in;
    end if;
  end process;
  
  in_bulk : process(req_clk_i) is
  begin
    if rising_edge(req_clk_i) then
      if s_req_in = '1' then
        rs_req_num   <= req_num_i(rs_req_num'range);
        rs_req_type  <= req_type_i;
        rs_req_field <= req_field_i;
      end if;
    end if;
  end process;
  
  main_control : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_req_xor2 <= '0';
      r_req_xor3 <= '0';
      r_req_xor4 <= '0';
      r_req_rok  <= '1';
      r_req_aok  <= '1';
      r_req_sok  <= '1';
      r_req_dok  <= '1';
      r_req_ack  <= '1';
      r_req_xor5 <= '0';
    elsif rising_edge(clk_i) then
      r_req_xor2 <= r_req_xor1;
      r_req_xor3 <= r_req_xor2;
      r_req_xor4 <= r_req_xor3;
      
      r_req_rok <= '1';
      
      if s_req_idx = '1' then
        r_req_aok <= '1';
      end if;
      r_req_sok <= r_req_aok;
      
      if s_req_dok = '1' then
        r_req_dok <= '1';
      end if;
      
      if s_req_ack = '1' then
        r_req_ack <= '1';
      end if;
      
      if s_req_rok = '1' then
        r_req_rok <= '0';
        r_req_aok <= '0';
        r_req_sok <= '0';
        r_req_dok <= '0';
        r_req_ack <= '0';
      end if;
      
      r_req_xor5 <= r_req_xor5 xor s_req_ack;
    end if;
  end process;
  
  main_bulk : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      if s_req_rok = '1' then
        rc_req_num   <= rs_req_num;
        rc_req_type  <= rs_req_type;
        rc_req_field <= rs_req_field;
      end if;
      if s_req_idx = '1' then
        r_req_idx <= s_index_o;
        r_req_cnt <= s_count_o;
        r_req_time<= s_time_o;
      end if;
      if s_req_dok = '1' then
        rc_req_dat <= s_req_dat;
      end if;
    end if;
  end process;
  
  out_control : process(req_clk_i, req_rst_n_i) is
  begin
    if req_rst_n_i = '0' then
      r_req_xor6 <= '0';
      r_req_xor7 <= '0';
      r_req_xor8 <= '0';
      r_req_out  <= '0';
    elsif rising_edge(req_clk_i) then
      r_req_xor6 <= r_req_xor5;
      r_req_xor7 <= r_req_xor6;
      r_req_xor8 <= r_req_xor7;
      r_req_out  <= s_req_ook;
    end if;
  end process;
  
  out_bulk : process(req_clk_i) is
  begin
    if rising_edge(req_clk_i) then
      if s_req_ook = '1' then
        rs_req_dat <= rc_req_dat;
      end if;
    end if;
  end process;
  
  req_valid_o <= r_req_out;
  req_data_o  <= rs_req_dat;
  
  -- Calculate the MSI
  sc_msi_rdy <= f_eca_or(r_selected);
  msi_control : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_raise_err  <= (others => '0');
      r_raise_val  <= (others => '0');
      r_raise_over <= (others => '0');
      r_raise_full <= '0';
      r_mask_err   <= (others => '0');
      r_mask_val   <= (others => '0');
      r_mask_over  <= (others => '0');
      r_mask_full  <= '0';
      rc_msi_rdy   <= '0';
      rc_msi_xor   <= '0';
      rc_ack_xor1  <= '0';
      rc_ack_xor2  <= '0';
      rc_ack_xor3  <= '0';
    elsif rising_edge(clk_i) then
      -- Mask out an interrupt after we've selected it for delivery already
      if rc_msi_rdy = '0' then
        r_mask_err  <= r_mask_err  or s_select_err;
        r_mask_val  <= r_mask_val  or s_select_val;
        r_mask_over <= r_mask_over or s_select_over;
        r_mask_full <= r_mask_full or s_select_full;
        rc_msi_rdy  <= sc_msi_rdy;
        rc_msi_xor  <= rc_msi_xor xor sc_msi_rdy;
      else
        -- Clear the MSI once acked
        rc_msi_rdy <= rc_ack_xor3 xnor rc_ack_xor2;
      end if;
      
      -- Set/clear the error interrupt corresponding to written table entry
      if (r_busy or s_atom_free) = '1' then
        r_raise_err(to_integer(unsigned(s_widx))) <= r_busy;
      end if;
      -- When counters are cleared, demask the interrupt so it can be delivered again
      if s_atom_free = '1' then
        -- s_atom_free => s_req_idx => !r_busy => [s_widx = rc_req_type & rc_req_num]
        r_mask_err(to_integer(unsigned(s_widx))) <= '0';
      end if;
      
      -- Set/clear the valid interrupt
      if (r_valid or s_valid_clear) = '1' then
        r_raise_val(to_integer(unsigned(s_val_widx))) <= r_valid;
      end if;
      if s_valid_clear = '1' then
        -- s_valid_clear => s_req_dok => s_req_val => !r_valid => [s_val_widx = rc_req_num]
        r_mask_val(to_integer(unsigned(s_val_widx))) <= '0';
      end if;
      
      -- Overflow interrupt?
      if (r_overflow or s_over_clear) = '1' then
        r_raise_over(to_integer(unsigned(s_ovr_widx))) <= r_overflow;
      end if;
      if s_over_clear = '1' then
        -- s_over_clear => s_req_dok => s_req_val => !r_valid => [s_val_widx = rc_req_num]
        r_mask_over(to_integer(unsigned(s_ovr_widx))) <= '0';
      end if;
      
      -- Used interrupt?
      if s_ack_clear = '1' then
        r_raise_full <= '0';
      end if;
      if (s_set_used or s_full_clear) = '1' then -- priority over clear
        r_raise_full <= '1';
      end if;
      if (s_full_clear or s_ack_clear) = '1' then
        r_mask_full <= '0';
      end if;
      
      -- Sync the ack
      rc_ack_xor1 <= rs_ack_xor;
      rc_ack_xor2 <= rc_ack_xor1;
      rc_ack_xor3 <= rc_ack_xor2;
    end if;
  end process;
  
  -- Pack the interrupts into one vector; lowest index => highest priority
  s_raised <= r_raise_err & r_raise_full & r_raise_over & r_raise_val;
  s_masked <= r_mask_err  & r_mask_full  & r_mask_over  & r_mask_val;
  s_select_val  <= r_selected(g_num_channels-1 downto 0);
  s_select_over <= r_selected(g_num_channels*2-1 downto g_num_channels);
  s_select_full <= r_selected(g_num_channels*2);
  s_select_err  <= r_selected(r_selected'high downto r_selected'high-4*g_num_channels+1);
  
  -- Arbitrate the raised interrupts
  s_pending  <= s_raised and not s_masked;
  s_selected <= s_pending and f_eca_add(not s_pending, 1);
  
  -- Decode the 1hot state of the selected vector
  s_msi_err_num <= f_eca_1hot_decode(s_select_err);
  s_msi_val_num <= f_eca_1hot_decode(s_select_val);
  s_msi_ovr_num <= f_eca_1hot_decode(s_select_over);
  
  msi_decode : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      -- The 2-cycle recurrance between r_masked=>r_selected=>r_masked is safe,
      -- because rc_msi_rdy must transition =>1 and then =>0 before the next pop.
      r_selected <= s_selected;
      if rc_msi_rdy = '0' then
        -- Calculate the code as sea of ORs
        rc_msi_code <= 
          ('0' & s_msi_err_num(1 downto 0)) or
          (f_eca_or(s_select_val)  & '0' & '0') or
          (f_eca_or(s_select_over) & '0' & f_eca_or(s_select_over)) or
          (s_select_full & s_select_full & '0');
        -- If only 1 channel, avoid null range
        if g_num_channels = 1 then
          rc_msi_num <= s_msi_val_num or s_msi_ovr_num;
        else
          rc_msi_num <= s_msi_val_num or s_msi_ovr_num or
                        s_msi_err_num(s_msi_err_num'high downto 2);
        end if;
      end if;
    end if;
  end process;
  
  ss_msi_rdy <= rs_msi_xor2 xor rs_msi_xor3;
  msi_output : process(msi_clk_i, msi_rst_n_i)  is
  begin
    if msi_rst_n_i = '0' then
      rs_msi_xor1 <= '0';
      rs_msi_xor2 <= '0';
      rs_msi_xor3 <= '0';
      rs_msi_rdy  <= '0';
      rs_ack_xor  <= '0';
    elsif rising_edge(msi_clk_i) then
      rs_msi_xor1 <= rc_msi_xor;
      rs_msi_xor2 <= rs_msi_xor1;
      rs_msi_xor3 <= rs_msi_xor2;
      if ss_msi_rdy = '1' then
        rs_msi_rdy <= '1';
      elsif msi_ack_i = '1' then
        rs_msi_rdy <= '0';
      end if;
      rs_ack_xor <= rs_ack_xor xor msi_ack_i;
    end if;
  end process;
  
  msi_output_bulk : process(msi_clk_i) is
  begin
    if rising_edge(msi_clk_i) then
      if ss_msi_rdy = '1' then
        rs_msi_code <= rc_msi_code;
        rs_msi_num  <= rc_msi_num;
      end if;
    end if;
  end process;
  
  msi_stb_o  <= rs_msi_rdy;
  msi_code_o <= rs_msi_code;
  msi_num_o(msi_num_o'high downto rs_msi_num'high+1) <= (others => '0');
  msi_num_o(rs_msi_num'range) <= rs_msi_num;
  
end rtl;
