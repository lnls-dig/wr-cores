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

entity eca_tag_channel is
  generic(
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
    channel_i  : in  t_channel;
    stall_i    : in  std_logic;
    snoop_i    : in  std_logic_vector(g_log_size-1 downto 0);
    channel_o  : out t_channel;
    overflow_o : out std_logic);
end eca_tag_channel;

architecture rtl of eca_tag_channel is

  constant c_log_cal_size  : natural := g_log_latency - g_log_multiplier;
  constant c_log_scan_size : natural := c_log_cal_size - 1;
  constant c_log_calendars : natural := g_log_size - c_log_scan_size;
  constant c_calendars     : natural := 2**c_log_calendars;
  constant c_slots         : natural := 2**g_log_multiplier;
  constant c_list_bits     : natural := 2 + c_log_scan_size; -- Format: [code low-index]
  constant c_record_size   : natural := 1 + 2 + g_log_size;  -- Format: [next code index]
  constant c_pipeline_depth: natural := 7;
  
  constant c_code_empty    : std_logic_vector(1 downto 0) := "00";
  constant c_code_early    : std_logic_vector(1 downto 0) := "01";
  constant c_code_late     : std_logic_vector(1 downto 0) := "10";
  constant c_code_valid    : std_logic_vector(1 downto 0) := "11";
  
  type t_record_array is array(natural range <>) of std_logic_vector(c_list_bits-1 downto 0);
  type t_cal_valid    is array(natural range <>) of unsigned(c_calendars-1 downto 0);
  
  function f_idx_sb(s, b : natural) return natural is
  begin
    return s * c_list_bits + b;
  end f_idx_sb;
  
  -- Note: it is important that f_idx_sc(s,c) orders first by slot (all slot 0 before any slot 1)
  function f_idx_sc(s, c : natural) return natural is
  begin
    return s * c_calendars + c;
  end f_idx_sc;
  
  signal r_time         : t_time;
  signal s_free_full    : std_logic;
  signal s_free_alloc   : std_logic_vector(g_log_size-1 downto 0);
  signal s_free_stb     : std_logic;
  signal r_free_entry   : std_logic_vector(g_log_size-1 downto 0);
  signal s_data_index   : std_logic_vector(g_log_size-1 downto 0);
  signal s_data_accept  : std_logic;
  signal s_data_channel : t_channel;
  signal r_last_time    : std_logic := '0';
  signal r_cal_reset    : std_logic_vector(c_log_cal_size downto 0) := (others => '0');
  signal s_cal_ready    : std_logic;
  signal s_cal_a_en     : std_logic;
  signal s_cal_a_addr   : std_logic_vector(c_log_cal_size-1 downto 0);
  signal r_cal_a_en     : std_logic := '0';
  signal s_cal_valid    : t_cal_valid(c_slots-1 downto 0);
  signal s_cal_next     : t_cal_valid(c_slots-1 downto 0);
  signal s_list_addr    : std_logic_vector(c_log_scan_size-1 downto 0);
  signal s_list_mux     : t_record_array(c_calendars-1 downto 0);
  signal s_list_record  : std_logic_vector(c_list_bits-1 downto 0);
  signal s_list_code    : std_logic_vector(1 downto 0);
  signal s_fifo_push    : std_logic_vector(c_slots*c_calendars-1 downto 0);
  signal r_fifo_push    : std_logic_vector(c_slots*c_calendars-1 downto 0) := (others => '0');
  signal s_fifo_data_i  : t_eca_matrix(c_slots*c_calendars-1 downto 0, c_record_size-1 downto 0);
  signal r_fifo_data_i  : t_eca_matrix(c_slots*c_calendars-1 downto 0, c_record_size-1 downto 0);
  signal s_fifo_pop     : std_logic;
  signal s_fifo_valid   : std_logic;
  signal s_fifo_fresh   : std_logic;
  signal s_fifo_data_o  : std_logic_vector(c_record_size-1 downto 0);
  signal s_mux_valid    : std_logic;
  signal r_mux_valid    : std_logic := '0';
  signal s_mux_data_l   : std_logic_vector(c_record_size-1 downto 0);
  signal s_mux_data     : std_logic_vector(c_record_size-1 downto 0);
  signal r_mux_data     : std_logic_vector(c_record_size-1 downto 0);
  signal s_mux_next     : std_logic;
  signal s_mux_code     : std_logic_vector(1 downto 0);
  signal s_list         : std_logic;
  signal s_late         : std_logic;
  signal s_early        : std_logic;
  signal s_conflict     : std_logic;
  signal s_delayed      : std_logic;
  signal r_late         : std_logic := '0';
  signal r_early        : std_logic := '0';
  signal r_conflict     : std_logic := '0';
  signal r_delayed      : std_logic := '0';
  signal r_saw_valid    : std_logic := '0';
  signal s_valid        : std_logic;
  signal s_stall        : std_logic;
  
begin

  time : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      r_time <= f_eca_add(time_i, c_pipeline_depth*2**g_log_multiplier);
    end if;
  end process;

  free : eca_free
    generic map(
      g_log_size => g_log_size)
    port map(
      clk_i   => clk_i,
      rst_n_i => rst_n_i,
      full_o  => s_free_full,
      alloc_i => channel_i.valid,
      entry_o => s_free_alloc,
      free_i  => s_free_stb,
      entry_i => r_free_entry);
  
  s_data_accept <= channel_i.valid and not s_free_full and s_cal_ready;
  overflow_o    <= channel_i.valid and (s_free_full or not s_cal_ready);
  
  data : eca_data
    generic map(
      g_log_size => g_log_size)
    port map(
      clk_i   => clk_i,
      rst_n_i => rst_n_i,
      w_en_i  => s_data_accept,
      w_idx_i => s_free_alloc,
      w_dat_i => channel_i,
      r_idx_i => s_data_index,
      r_dat_o => s_data_channel);
  
  -- Only read+clear calendar if the clock does not skip
  -- This prevents blocking calendar port B for two cycles in succession
  s_cal_ready  <= r_cal_reset(r_cal_reset'high);
  s_cal_a_en   <= (r_time(g_log_multiplier) xor r_last_time) or not s_cal_ready;
  s_cal_a_addr <= 
    f_eca_mux(s_cal_ready, 
      r_time(g_log_latency-1 downto g_log_multiplier), 
      r_cal_reset(c_log_cal_size-1 downto 0));
  safe_time : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_cal_reset <= (others => '0');
      r_cal_a_en  <= '0';
      r_last_time <= '0';
    elsif rising_edge(clk_i) then
      r_cal_reset <= f_eca_add(r_cal_reset, not s_cal_ready);
      r_cal_a_en  <= s_cal_a_en and s_cal_ready;
      r_last_time <= r_time(g_log_multiplier);
    end if;
  end process;
  
  parallel_scan : for c in 0 to c_calendars-1 generate
    bl : block is
    
      signal r_scan_stb     : std_logic := '0';
      signal s_scan_stb     : std_logic;
      signal s_scan_we      : std_logic;
      signal s_scan_late    : std_logic;
      signal s_scan_early   : std_logic;
      signal r_scan_low     : std_logic_vector(g_log_latency-1 downto 0);
      signal s_scan_low     : std_logic_vector(g_log_latency-1 downto 0);
      signal s_scan_idx     : std_logic_vector(c_log_scan_size-1 downto 0);
      
      signal s_cal_a_data   : std_logic_vector(c_slots*c_list_bits-1 downto 0);
      signal s_cal_b_en     : std_logic;
      signal s_cal_b_ack    : std_logic;
      signal s_cal_b_addr   : std_logic_vector(c_log_cal_size-1 downto 0);
      signal s_cal_b_data   : std_logic_vector(c_slots*c_list_bits-1 downto 0);
      signal s_cal_b_mux    : std_logic_vector(c_slots*c_list_bits-1 downto 0);
      signal s_cal_code     : std_logic_vector(1 downto 0);
      signal s_cal_record   : std_logic_vector(c_list_bits-1 downto 0);
      signal r_cal_record   : std_logic_vector(c_list_bits-1 downto 0);
      
      signal s_list_we       : std_logic;
      signal s_list_dat      : std_logic_vector(c_list_bits-1 downto 0);
      
      type t_slot_array is array(c_list_bits-1 downto 0) of std_logic_vector(c_slots-1 downto 0);
      signal s_slot_mux     : t_slot_array;
      signal s_slot_select  : std_logic_vector(c_slots-1 downto 0);
      
    begin
    
      gt0 : if c_calendars > 1 generate
        s_scan_we <= s_data_accept and
          f_eca_eq(std_logic_vector(to_unsigned(c, c_log_calendars)), 
                   s_free_alloc(g_log_size-1 downto c_log_scan_size));
      end generate;
      
      eq0 : if c_calendars = 1 generate
        s_scan_we <= s_data_accept;
      end generate;

      scan : eca_scan
        generic map(
          g_ext_size       => 1,
          g_log_size       => c_log_scan_size,
          g_log_multiplier => g_log_multiplier,
          g_log_max_delay  => g_log_max_delay,
          g_log_latency    => g_log_latency)
        port map(
          clk_i        => clk_i,
          rst_n_i      => rst_n_i,
          time_i       => r_time,
          wen_i        => s_scan_we,
          stall_o      => open, -- always goes low before s_free_full goes low
          deadline_i   => channel_i.time,
          idx_i        => s_free_alloc(c_log_scan_size-1 downto 0),
          ext_i        => (others => '0'),
          scan_stb_o   => s_scan_stb,
          scan_late_o  => s_scan_late,
          scan_early_o => s_scan_early,
          scan_low_o   => s_scan_low,
          scan_idx_o   => s_scan_idx,
          scan_ext_o   => open);
      
      -- code: 00 = empty, 01 = early, 10 = late, 11 = valid
      s_cal_code(1) <= s_scan_stb and not s_scan_early;
      s_cal_code(0) <= s_scan_stb and not s_scan_late;
      s_cal_record <= s_cal_code & s_scan_idx;
      
      -- Pulse extend the calendar write to two cycles
      s_cal_b_en   <= s_scan_stb or r_scan_stb;
      s_cal_b_addr <= s_scan_low(g_log_latency-1 downto g_log_multiplier);
      
      control : process(clk_i, rst_n_i) is
      begin
        if rst_n_i = '0' then
          r_scan_stb <= '0';
        elsif rising_edge(clk_i) then
          r_scan_stb <= s_scan_stb;
        end if;
      end process;
      
      delay : process(clk_i) is
      begin
        if rising_edge(clk_i) then
          if s_scan_stb = '1' then
            r_scan_low   <= s_scan_low;
            r_cal_record <= s_cal_record;
          end if;
        end if;
      end process;
      
      -- Calculate which slot to put the action into
      sel_eq1 : if c_slots = 1 generate
        s_slot_select(0) <= '1';
      end generate;
      sel_gt1 : if c_slots > 1 generate
        slots : for s in 0 to c_slots-1 generate
          s_slot_select(s) <= f_eca_eq(
            std_logic_vector(to_unsigned(s, g_log_multiplier)),
            r_scan_low(g_log_multiplier-1 downto 0));
        end generate;
      end generate;
      
      -- Insert action into the correct calendar slot
      slots1 : for s in 0 to c_slots-1 generate
        bits : for b in 0 to c_list_bits-1 generate
          s_cal_b_mux(f_idx_sb(s,b)) <= 
            f_eca_mux(s_slot_select(s), r_cal_record(b), s_cal_b_data(f_idx_sb(s,b)));
        end generate;
      end generate;
      
      -- Save the slot being overwritten into the linked list
      s_list_we <= s_cal_b_ack;
      bits : for b in 0 to c_list_bits-1 generate
        slots : for s in 0 to c_slots-1 generate
          s_slot_mux(b)(s) <= s_cal_b_data(f_idx_sb(s, b)) and s_slot_select(s);
        end generate;
        s_list_dat(b) <= f_eca_or(s_slot_mux(b));
      end generate;
      
      -- Format: [code index]*c_slots
      calendar : eca_rmw
        generic map(
          g_addr_bits => c_log_cal_size,
          g_data_bits => c_slots*c_list_bits)
        port map(
          clk_i    => clk_i,
          rst_n_i  => rst_n_i,
          a_en_i   => s_cal_a_en,
          a_ack_o  => open,
          a_addr_i => s_cal_a_addr,
          a_data_o => s_cal_a_data,
          a_data_i => (others => '0'),
          b_en_i   => s_cal_b_en,
          b_ack_o  => s_cal_b_ack,
          b_addr_i => s_cal_b_addr,
          b_data_o => s_cal_b_data,
          b_data_i => s_cal_b_mux);
      
      -- No need to wipe on reset; it is only read if calendar pointed into it
      list : eca_sdp
        generic map(
          g_addr_bits  => c_log_scan_size,
          g_data_bits  => c_list_bits,
          g_bypass     => false,
          g_dual_clock => false)
        port map(
          r_clk_i  => clk_i,
          r_addr_i => s_list_addr(c_log_scan_size-1 downto 0),
          r_data_o => s_list_mux(c),
          w_clk_i  => clk_i,
          w_en_i   => s_list_we,
          w_addr_i => r_cal_record(c_log_scan_size-1 downto 0),
          w_data_i => s_list_dat);

      -- Note: it is important that f_idx_sc(s,c) orders first by slot
      slots2 : for s in 0 to c_slots-1 generate
        -- Is this calendar entry valid?
        s_cal_valid(s)(c) <= 
          s_cal_a_data(f_idx_sb(s,c_log_scan_size+0)) or
          s_cal_a_data(f_idx_sb(s,c_log_scan_size+1));
        -- Decode code => not-empty
        s_fifo_push(f_idx_sc(s,c)) <= r_cal_a_en and s_cal_valid(s)(c);
        -- FIFO record flags
        s_fifo_data_i(f_idx_sc(s,c),g_log_size+2) <= s_cal_next(s)(c);
        s_fifo_data_i(f_idx_sc(s,c),g_log_size+1) <= s_cal_a_data(f_idx_sb(s,c_log_scan_size+1));
        s_fifo_data_i(f_idx_sc(s,c),g_log_size+0) <= s_cal_a_data(f_idx_sb(s,c_log_scan_size+0));
        -- Fill high bits from calendar #
        high : if c_calendars > 1 generate
          bits : for b in 0 to c_log_calendars-1 generate
            s_fifo_data_i(f_idx_sc(s,c),b+c_log_scan_size) <= to_unsigned(c,c_log_calendars)(b);
          end generate;
        end generate;
        -- copy low bits
        bits : for b in 0 to c_log_scan_size-1 generate
          s_fifo_data_i(f_idx_sc(s,c),b) <= s_cal_a_data(f_idx_sb(s,b));
        end generate;
      end generate;
    end block;
  end generate; -- parallel_scan (c_calendars)
  
  -- Detect >1 actions in the same slots but from different calendars
  nexts : for s in 0 to c_slots-1 generate
    s_cal_next(s) <= s_cal_valid(s) and (s_cal_valid(s) - 1);
  end generate;
  
  fifo : eca_piso_fifo
    generic map(
      g_log_size  => g_log_size,
      g_log_ports => g_log_multiplier + c_log_calendars,
      g_width     => c_record_size)
    port map(
      clk_i   => clk_i,
      rst_n_i => rst_n_i,
      push_i  => r_fifo_push,
      data_i  => r_fifo_data_i,
      pop_i   => s_fifo_pop,
      valid_o => s_fifo_valid,
      fresh_o => s_fifo_fresh,
      data_o  => s_fifo_data_o);
  
  -- Fetch the record from the data table
  bits : for b in 0 to g_log_size-1 generate
    s_data_index(b) <= 
      f_eca_mux(s_stall,
        f_eca_mux(r_mux_valid, r_mux_data(b), snoop_i(b)),
        f_eca_mux(s_mux_valid, s_mux_data(b), snoop_i(b)));
  end generate;
  s_list_addr <= s_data_index(c_log_scan_size-1 downto 0);
  
  -- Mux out the record from the linked lists
  con_eq1 : if c_calendars = 1 generate
    s_list_record <= s_list_mux(0);
  end generate;
  con_gt1 : if c_calendars > 1 generate
    con : block is
      signal s_mux_idx : std_logic_vector(c_log_calendars-1 downto 0);
    begin
      s_mux_idx <= r_mux_data(c_log_calendars+c_log_scan_size-1 downto c_log_scan_size);
      s_list_record <= s_list_mux(to_integer(unsigned(s_mux_idx))) when f_eca_safe(s_mux_idx) = '1' else (others => 'X');
    end block;
  end generate;
  s_list_code <= s_list_record(c_log_scan_size+1 downto c_log_scan_size);
  
  -- Next record comes from linked list or fifo?
  s_list <= r_mux_valid and not f_eca_eq(s_list_code, c_code_empty);
  s_mux_valid <= s_list or s_fifo_valid;

  -- Pop the record if we're done with it
  s_fifo_pop <= s_fifo_valid and not (s_list or s_stall);
  
  -- Expand the list record entry to the same format as a fifo entry
  s_mux_data_l(c_record_size-1 downto c_record_size-3) <= '1' & s_list_code;
  s_mux_data_l(c_log_scan_size-1 downto 0) <= s_list_record(c_log_scan_size-1 downto 0);
  cal_gt1 : if c_calendars > 1 generate
    s_mux_data_l(g_log_size-1 downto c_log_scan_size) <= r_mux_data(g_log_size-1 downto c_log_scan_size);
  end generate;
  -- Select linked list in prefernce to FIFO data
  s_mux_data <= f_eca_mux(s_list, s_mux_data_l, s_fifo_data_o);
  s_mux_next <= s_mux_data(g_log_size+2);
  s_mux_code <= s_mux_data(g_log_size+1 downto g_log_size);
  
  -- Determine exceptional conditions
  s_late     <= s_mux_valid and f_eca_eq(s_mux_code, c_code_late);
  s_early    <= s_mux_valid and f_eca_eq(s_mux_code, c_code_early);
  s_conflict <= s_mux_valid and s_mux_next and r_saw_valid;
  s_delayed  <= s_list or (s_fifo_valid and not s_fifo_fresh);
  
  control : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_mux_valid <= '0';
      r_late      <= '0';
      r_early     <= '0';
      r_delayed   <= '0';
      r_conflict  <= '0';
      r_saw_valid <= '0';
      r_fifo_push <= (others => '0');
    elsif rising_edge(clk_i) then
      r_fifo_push <= s_fifo_push;
      if s_stall = '0' then
        r_mux_valid <= s_mux_valid;
        -- late/early/conflict/delayed are mutually exclusive; most severe first
        r_late      <= s_late;
        r_early     <= s_early;
        r_conflict  <= s_conflict and not (s_late or s_early);
        r_delayed   <= s_delayed  and not (s_late or s_early or s_conflict);
        r_saw_valid <= (s_mux_next and r_saw_valid) or not (s_late or s_early);
      end if;
    end if;
  end process;
  
  bulk : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      r_fifo_data_i<= s_fifo_data_i;
      if s_stall = '0' then
        r_mux_data   <= s_mux_data;
        r_free_entry <= s_data_index;
      end if;
    end if;
  end process;

  -- Don't free it if we're stalled
  s_free_stb <= r_mux_valid and not s_stall;
  
  -- Only valid if the errors are accepted by the condition rule
  s_valid <= r_mux_valid and
    (not r_delayed  or s_data_channel.delayed)  and
    (not r_conflict or s_data_channel.conflict) and
    (not r_late     or s_data_channel.late)     and
    (not r_early    or s_data_channel.early);
  s_stall <= stall_i and s_valid; -- Stall if we are reporting something not accepted
  
  -- We're done!
  channel_o.valid    <= s_valid;
  channel_o.delayed  <= r_delayed;
  channel_o.conflict <= r_conflict;
  channel_o.late     <= r_late;
  channel_o.early    <= r_early;
  channel_o.event    <= s_data_channel.event;
  channel_o.param    <= s_data_channel.param;
  channel_o.tag      <= s_data_channel.tag;
  channel_o.tef      <= s_data_channel.tef;
  channel_o.time     <= s_data_channel.time;
  
end rtl;
