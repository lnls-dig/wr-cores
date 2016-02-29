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
    g_log_latency    : natural := 12);-- 2**g_log_latency    = ticks of calendar delay
  port(
    clk_i      : in  std_logic;
    rst_n_i    : in  std_logic;
    -- Timestamps used for pipeline stages
    time_i     : in  t_time;
    -- Push a record to the queue
    channel_i  : in  t_channel;
    stall_i    : in  std_logic;
    channel_o  : out t_channel;
    overflow_o : out std_logic;
    -- Peek into the buffer
    addr_i     : in  std_logic_vector(g_log_size-1 downto 0);
    inspect_o  : out t_channel);
end eca_tag_channel;

architecture rtl of eca_tag_channel is

  constant c_log_cal_size  : natural := g_log_latency - g_log_multiplier;
  constant c_log_scan_size : natural := c_log_cal_size - 1;
  constant c_log_calendars : natural := g_log_size - c_log_scan_size;
  constant c_calendars     : natural := 2**c_log_calendars;
  constant c_slots         : natural := 2**g_log_multiplier;
  constant c_bits          : natural := 2 + c_log_scan_size;               -- Format: [code index]
  constant c_record_size   : natural := 2 + g_log_multiplier + g_log_size; -- Format: [code slot index]
  
  type t_code_array   is array(natural range <>) of std_logic_vector(1 downto 0);
  type t_record_array is array(natural range <>) of std_logic_vector(c_bits-1 downto 0);
  type t_low_array    is array(natural range <>) of std_logic_vector(g_log_latency-1 downto 0);
  type t_scan_array   is array(natural range <>) of std_logic_vector(c_log_scan_size-1 downto 0);
  type t_cal_array    is array(natural range <>) of std_logic_vector(c_log_cal_size-1 downto 0);
  type t_matrix_array is array(natural range <>) of std_logic_vector(c_slots*c_bits-1 downto 0);
  
  function f_idx_sb(s, b : natural) return natural is
  begin
    return s * c_bits + b;
  end f_idx_sb;
  
  -- Note: it is important that f_idx_sc(s,c) orders first by slot (all slot 0 before any slot 1)
  function f_idx_sc(s, c : natural) return natural is
  begin
    return s * c_calendars + c;
  end f_idx_sc;
  
  signal s_free_full    : std_logic;
  signal s_free_alloc   : std_logic_vector(g_log_size-1 downto 0);
  signal s_free_stb     : std_logic;
  signal r_free_entry   : std_logic_vector(g_log_size-1 downto 0);
  signal s_data_index   : std_logic_vector(g_log_size-1 downto 0);
  signal s_data_accept  : std_logic;
  signal s_data_channel : t_channel;
  signal r_last_time    : std_logic;
  signal s_scan_we      : std_logic_vector(c_calendars-1 downto 0);
  signal s_scan_stb     : std_logic_vector(c_calendars-1 downto 0);
  signal s_scan_late    : std_logic_vector(c_calendars-1 downto 0);
  signal s_scan_early   : std_logic_vector(c_calendars-1 downto 0);
  signal s_scan_low     : t_low_array(c_calendars-1 downto 0);
  signal s_scan_idx     : t_scan_array(c_calendars-1 downto 0);
  signal r_scan_stb     : std_logic_vector(c_calendars-1 downto 0);
  signal r_scan_low     : t_low_array(c_calendars-1 downto 0);
  signal s_cal_a_en     : std_logic;
  signal s_cal_a_data   : t_matrix_array(c_calendars-1 downto 0);
  signal s_cal_b_en     : std_logic_vector(c_calendars-1 downto 0);
  signal s_cal_b_addr   : t_cal_array(c_calendars-1 downto 0);
  signal s_cal_b_data   : t_matrix_array(c_calendars-1 downto 0);
  signal s_cal_b_mux    : t_matrix_array(c_calendars-1 downto 0);
  signal s_cal_code     : t_code_array(c_calendars-1 downto 0);
  signal s_cal_record   : t_record_array(c_calendars-1 downto 0);
  signal r_cal_record   : t_record_array(c_calendars-1 downto 0);
  signal s_com_valid_i  : std_logic_vector(c_slots*c_calendars-1 downto 0);
  signal s_com_data_i   : t_eca_matrix(c_slots*c_calendars-1 downto 0, c_record_size-1 downto 0);
  signal s_com_valid_o  : std_logic_vector(c_slots*c_calendars-1 downto 0);
  signal s_com_data_o   : t_eca_matrix(c_slots*c_calendars-1 downto 0, c_record_size-1 downto 0);
  signal s_fifo_pop     : std_logic;
  signal s_fifo_valid   : std_logic;
  signal s_fifo_fresh   : std_logic;
  signal s_fifo_data    : t_eca_matrix(c_slots*c_calendars-1 downto 0, c_record_size-1 downto 0);
  signal s_mux_valid    : std_logic;
  signal s_mux_data     : t_eca_matrix(c_slots*c_calendars-1 downto 0, c_record_size-1 downto 0);
  signal r_mux_data     : t_eca_matrix(c_slots*c_calendars-1 downto 0, c_record_size-1 downto 0);
  signal s_repeat       : std_logic;
  signal r_repeat       : std_logic := '0';
  signal s_stall        : std_logic;
  signal s_valid        : std_logic;
  signal r_valid        : std_logic := '0';
  signal r_late         : std_logic;
  signal r_early        : std_logic;
  signal r_conflict     : std_logic;
  signal r_delayed      : std_logic;
  signal s_slot         : std_logic_vector(g_log_multiplier downto 0) := (others => '0');
  signal r_slot         : std_logic_vector(g_log_multiplier downto 0);
  
begin

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
  
  s_data_accept <= channel_i.valid and not s_free_full;
  overflow_o <= channel_i.valid and s_free_full;
  
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
  s_cal_a_en <= time_i(g_log_multiplier) xor r_last_time;
  safe_time : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      r_last_time <= time_i(g_log_multiplier);
    end if;
  end process;
  
  parallel_scan : for c in 0 to c_calendars-1 generate
  
    gt1 : if c_calendars > 1 generate
      s_scan_we(c) <= s_data_accept when to_unsigned(c, c_log_calendars) = unsigned(s_free_alloc(g_log_size-1 downto c_log_scan_size)) else '0';
    end generate;
    
    eq0 : if c_calendars = 0 generate
      s_scan_we(c) <= s_data_accept;
    end generate;

    scan : eca_scan
      generic map(
        g_ext_size       => 1,
        g_log_size       => c_log_scan_size,
        g_log_multiplier => g_log_multiplier,
        g_log_latency    => g_log_latency)
      port map(
        clk_i        => clk_i,
        rst_n_i      => rst_n_i,
        time_i       => time_i,
        wen_i        => s_scan_we(c),
        deadline_i   => channel_i.time,
        idx_i        => s_free_alloc(c_log_scan_size-1 downto 0),
        ext_i        => (others => '0'),
        scan_stb_o   => s_scan_stb(c),
        scan_late_o  => s_scan_late(c),
        scan_early_o => s_scan_early(c),
        scan_low_o   => s_scan_low(c),
        scan_idx_o   => s_scan_idx(c),
        scan_ext_o   => open);
    
    -- code: 00 = empty, 01 = early, 10 = late, 11 = valid
    s_cal_code(c)(1) <= s_scan_stb(c) and not s_scan_early(c);
    s_cal_code(c)(0) <= s_scan_stb(c) and not s_scan_late(c);
    s_cal_record(c)  <= s_cal_code(c) & s_scan_idx(c);
    
    -- Pulse extend the calendar write to two cycles
    s_cal_b_en(c)   <= s_scan_stb(c) or r_scan_stb(c);
    s_cal_b_addr(c) <= s_scan_low(c)(g_log_latency-1 downto g_log_multiplier);
    
    extend : process(clk_i) is
    begin
      if rising_edge(clk_i) then
        r_scan_stb(c) <= s_scan_stb(c);
        if s_scan_stb(c) = '1' then
          r_scan_low(c)   <= s_scan_low(c);
          r_cal_record(c) <= s_cal_record(c);
        end if;
      end if;
    end process;
    
    -- Insert action into the correct calendar slot
    slots1 : for s in 0 to c_slots-1 generate
      bits : for b in 0 to c_bits-1 generate
        gt0 : if g_log_multiplier > 0 generate
          s_cal_b_mux(c)(f_idx_sb(s,b)) <= r_cal_record(c)(b) 
            when to_unsigned(s, g_log_multiplier) = unsigned(r_scan_low(c)(g_log_multiplier-1 downto 0))
            else s_cal_b_data(c)(f_idx_sb(s,b));
        end generate;
        eq0 : if g_log_multiplier = 0 generate
          s_cal_b_mux(c)(f_idx_sb(s,b)) <= r_cal_record(c)(b);
        end generate;
      end generate;
    end generate;
    
    -- Format: [code index]*c_slots
    calendar : eca_rmw
      generic map(
        g_addr_bits => c_log_cal_size,
        g_data_bits => c_slots*c_bits)
      port map(
        clk_i    => clk_i,
        a_en_i   => s_cal_a_en,
        a_ack_o  => open,
        a_addr_i => time_i(g_log_latency-1 downto g_log_multiplier),
        a_data_o => s_cal_a_data(c),
        a_data_i => (others => '0'),
        b_en_i   => s_cal_b_en(c),
        b_ack_o  => open, -- use to trigger conflict write?
        b_addr_i => s_cal_b_addr(c),
        b_data_o => s_cal_b_data(c),
        b_data_i => s_cal_b_mux(c));
    
    -- [code, idx] entries
    -- !!! resolve conflict list
--    conflicts : eca_sdp
--      generic map(
--        g_addr_bits  =>
--        g_data_bits  =>
--        g_bypass     => false,
--        g_dual_clock => false)
--      port map(
--        r_clk_i  => clk_i,
--        r_addr_i => 
--        r_data_o => 
--        w_clk_i  => clk_i,
--        w_en_i   => 
--        w_addr_i => 
--        w_data_i => );

    -- Note: it is important that f_idx_sc(s,c) orders first by slot
    slots2 : for s in 0 to c_slots-1 generate
      s_com_valid_i(f_idx_sc(s,c)) <= 
        s_cal_a_data(c)(f_idx_sb(s,c_log_scan_size)) or
        s_cal_a_data(c)(f_idx_sb(s,c_log_scan_size+1));
      -- copy code to wider field
      s_com_data_i(f_idx_sc(s,c),g_log_size+g_log_multiplier+1) <= s_cal_a_data(c)(f_idx_sb(s,c_log_scan_size+1));
      s_com_data_i(f_idx_sc(s,c),g_log_size+g_log_multiplier+0) <= s_cal_a_data(c)(f_idx_sb(s,c_log_scan_size+0));
      -- inject slot bits (to detect conflict)
      mid : if c_slots > 1 generate
        bits : for b in 0 to g_log_multiplier-1 generate
          s_com_data_i(f_idx_sc(s,c),b+g_log_size) <= to_unsigned(s,g_log_multiplier)(b);
        end generate;
      end generate;
      -- restore high bits
      high : if c_calendars > 1 generate
        bits : for b in 0 to c_log_calendars-1 generate
          s_com_data_i(f_idx_sc(s,c),b+c_log_scan_size) <= to_unsigned(c,c_log_calendars)(b);
        end generate;
      end generate;
      -- copy low bits
      bits : for b in 0 to c_log_scan_size-1 generate
        s_com_data_i(f_idx_sc(s,c),b) <= s_cal_a_data(c)(f_idx_sb(s,b));
      end generate;
    end generate;
  end generate;
  
  compact : eca_compact
    generic map(
      g_size => c_slots*c_calendars,
      g_wide => c_bits)
    port map(
      clk_i   => clk_i,
      rst_n_i => rst_n_i,
      valid_i => s_com_valid_i,
      valid_o => s_com_valid_o,
      data_i  => s_com_data_i,
      data_o  => s_com_data_o);
  
  fifo : eca_fifo
    generic map(
      g_log_size => g_log_size,
      g_rows     => c_slots*c_calendars,
      g_cols     => c_bits)
    port map(
      clk_i   => clk_i,
      rst_n_i => rst_n_i,
      push_i  => s_com_valid_o(0),
      data_i  => s_com_data_o,
      pop_i   => s_fifo_pop,
      valid_o => s_fifo_valid,
      fresh_o => s_fifo_fresh,
      data_o  => s_fifo_data);

  -- Resolve multiple actions in one cycle
  s_mux_valid <= s_fifo_valid when r_repeat='0' else '1';
  s_mux_data  <= s_fifo_data  when r_repeat='0' else r_mux_data;
  
  -- Pop the record if we're done with it
  s_fifo_pop <= s_fifo_valid and not s_repeat and not s_stall;
  
  eq1 : if c_slots*c_calendars = 1 generate
    s_repeat <= '0';
  end generate;
  gt1 : if c_slots*c_calendars > 1 generate
    s_repeat <= s_mux_valid and (s_fifo_data(1,g_log_size+g_log_multiplier+0) or 
                                 s_fifo_data(1,g_log_size+g_log_multiplier+1));
  end generate;
  
  control : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_repeat <= '0';
      r_valid  <= '0';
    elsif rising_edge(clk_i) then
      if s_stall = '0' then
        r_repeat <= s_repeat;
        r_valid  <= s_mux_valid;
      end if;
    end if;
  end process;
  
  shift : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      if s_stall = '0' then
         for b in 0 to c_record_size-1 loop
           r_mux_data(c_slots*c_calendars-1, b) <= '0';
           if c_slots*c_calendars > 1 then
              for i in 0 to c_slots*c_calendars-2 loop
                r_mux_data(i,b) <= s_mux_data(i+1,b);
              end loop;
            end if;
         end loop;
      end if;
    end if;
  end process;
  
  -- Fetch the record from the data table
  bits : for b in 0 to g_log_size-1 generate
    s_data_index(b) <= s_mux_data(0,b) when s_stall='0' else r_free_entry(b);
  end generate;
  -- Fetch slot index
  eq0 : if g_log_multiplier > 0 generate
    bits : for b in 0 to g_log_multiplier-1 generate
      s_slot(b) <= s_mux_data(0,b+g_log_size);
    end generate;
  end generate;
  
  output : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      if s_stall = '0' then
        r_late     <= s_mux_valid and not s_mux_data(0,g_log_size+g_log_multiplier+0);
        r_early    <= s_mux_valid and not s_mux_data(0,g_log_size+g_log_multiplier+1);
        r_delayed  <= (s_fifo_valid and not s_fifo_fresh) or r_repeat;
        r_conflict <= r_repeat and f_eca_eq(r_slot, s_slot);
        r_slot     <= s_slot;
        r_free_entry <= s_data_index;
      end if;
    end if;
  end process;
  
  -- Don't free it if we're stalled
  s_free_stb <= r_valid and not s_stall;
  
  -- Only valid if the errors are accepted by the condition rule
  s_valid <= r_valid and
    (not r_delayed  or s_data_channel.delayed)  and
    (not r_conflict or s_data_channel.conflict) and
    (not r_late     or s_data_channel.late)     and
    (not r_early    or s_data_channel.early);
  s_stall <= stall_i and s_valid; -- Stall if we are reporting something not accepted
  
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
