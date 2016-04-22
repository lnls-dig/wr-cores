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
    -- Inspect the action while idle
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
end eca_tag_channel;

architecture rtl of eca_tag_channel is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  constant c_log_channels  : natural := f_eca_log2(g_num_channels);
  constant c_log_cal_size  : natural := g_log_latency - g_log_multiplier;
  constant c_log_scan_size : natural := c_log_cal_size - 1;
  constant c_log_calendars : natural := g_log_size - c_log_scan_size;
  constant c_calendars     : natural := 2**c_log_calendars;
  constant c_slots         : natural := 2**g_log_multiplier;
  constant c_ext_bits      : natural := c_log_channels + 4;                   -- Format: [num early late set clear]
  constant c_list_bits     : natural := 2 + c_log_scan_size;                  -- Format: [code low-index]
  constant c_fifo_bits     : natural := 1 + 2 + g_log_size;                   -- Format: [next code index]
  constant c_pipeline_depth: natural := 8;
  
  constant c_code_bits     : natural := 2;
  constant c_code_empty    : std_logic_vector(1 downto 0) := "00";
  constant c_code_early    : std_logic_vector(1 downto 0) := "01";
  constant c_code_late     : std_logic_vector(1 downto 0) := "10";
  constant c_code_valid    : std_logic_vector(1 downto 0) := "11";
  
  constant c_ext_flip      : natural := 0;
  constant c_ext_late      : natural := 2;
  constant c_ext_early     : natural := 3;
  constant c_ext_channel   : natural := 4;
  
  constant c_list_index    : natural := 0;
  constant c_list_code     : natural := c_list_index + c_log_scan_size;
  
  constant c_fifo_index    : natural := 0;
  constant c_fifo_code     : natural := c_fifo_index + g_log_size;
  constant c_fifo_next     : natural := c_fifo_code + 2;
  
  type t_record_array is array(natural range <>) of std_logic_vector(c_list_bits-1 downto 0);
  type t_cal_valid    is array(natural range <>) of unsigned(c_calendars-1 downto 0);
  type t_gpio_matrix  is array(natural range <>) of std_logic_vector(c_slots*2*g_num_channels-1 downto 0);
  
  -- Note: it is important that f_idx_sc(s,c) orders first by slot (all slot 0 before any slot 1)
  function f_idx_sc(s, c : natural) return natural is
  begin
    return s * c_calendars + c;
  end f_idx_sc;
  
  function f_idx_si(s, i : natural) return natural is
  begin
    return (s * g_num_channels + i) * 2;
  end f_idx_si;
  
  signal r_time         : t_time;
  signal s_free_full    : std_logic;
  signal s_free_alloc   : std_logic_vector(g_log_size-1 downto 0);
  signal s_data_index   : std_logic_vector(g_log_size-1 downto 0);
  signal r_data_index1  : std_logic_vector(g_log_size-1 downto 0);
  signal r_data_index2  : std_logic_vector(g_log_size-1 downto 0);
  signal s_data_accept  : std_logic;
  signal s_ext          : std_logic_vector(c_ext_bits-1 downto 0);
  signal s_data_channel : t_channel;
  signal r_skid_channel : t_channel;
  signal s_mux_channel  : t_channel;
  signal r_channel      : t_channel := c_idle_channel;
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
  signal s_fifo_data_i  : t_eca_matrix(c_slots*c_calendars-1 downto 0, c_fifo_bits-1 downto 0);
  signal r_fifo_data_i  : t_eca_matrix(c_slots*c_calendars-1 downto 0, c_fifo_bits-1 downto 0);
  signal s_fifo_pop     : std_logic;
  signal s_fifo_valid   : std_logic;
  signal s_fifo_fresh   : std_logic;
  signal s_fifo_data_o  : std_logic_vector(c_fifo_bits-1 downto 0);
  signal s_mux_valid    : std_logic;
  signal r_mux_valid    : std_logic := '0';
  signal s_mux_data_l   : std_logic_vector(c_fifo_bits-1 downto 0);
  signal s_mux_data     : std_logic_vector(c_fifo_bits-1 downto 0);
  signal r_mux_data     : std_logic_vector(c_fifo_bits-1 downto 0);
  signal s_mux_next     : std_logic;
  signal s_mux_code     : std_logic_vector(1 downto 0);
  signal s_mux_num      : std_logic_vector(f_eca_log2_min1(g_num_channels)-1 downto 0);
  signal r_mux_ontime   : std_logic;
  signal r_mux_late     : std_logic;
  signal r_mux_early    : std_logic;
  signal r_mux_next     : std_logic;
  signal r_mux_delay    : std_logic;
  signal s_list         : std_logic;
  signal s_late         : std_logic;
  signal s_early        : std_logic;
  signal s_conflict     : std_logic;
  signal s_delayed      : std_logic;
  signal r_saw_valid    : std_logic_vector(g_num_channels-1 downto 0) := (others => '0');
  signal s_saw_valid    : std_logic;
  signal s_valid        : std_logic;
  signal s_stall1       : std_logic;
  signal s_stall2       : std_logic;
  signal r_stall        : std_logic := '0';
  signal r_gpio_matrix  : t_gpio_matrix(c_calendars-1 downto 0);
  
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
      used_o  => used_o,
      full_o  => s_free_full,
      alloc_i => channel_i.valid,
      entry_o => s_free_alloc,
      free_i  => free_i,
      entry_i => index_i);
  
  s_data_accept <= channel_i.valid and not s_free_full and s_cal_ready;
  overflow_o    <= channel_i.valid and (s_free_full or not s_cal_ready);
  
  -- The extended record for scanner
  s_ext(c_ext_flip+1 downto c_ext_flip) <= clr_i & set_i;
  s_ext(c_ext_early) <= channel_i.early;
  s_ext(c_ext_late)  <= channel_i.late;
  ext_gt1 : if g_num_channels > 1 generate
    s_ext(c_ext_channel+c_log_channels-1 downto c_ext_channel) <= 
      channel_i.num(c_log_channels-1 downto 0);
  end generate;
  
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
    
      constant c_list_wide : natural := c_slots*c_list_bits;
      constant c_gpio_wide : natural := c_slots*2*g_num_channels;
    
      function f_mem_wide return natural is
      begin
        if g_support_io then
          return c_list_wide + c_gpio_wide;
        else
          return c_list_wide;
        end if;
      end f_mem_wide;
      
      constant c_data_wide : natural := f_mem_wide;
      
      signal r_scan_stb     : std_logic := '0';
      signal s_scan_stb     : std_logic;
      signal s_scan_we      : std_logic;
      signal s_scan_late    : std_logic;
      signal s_scan_early   : std_logic;
      signal r_scan_low     : std_logic_vector(g_log_latency-1 downto 0);
      signal s_scan_low     : std_logic_vector(g_log_latency-1 downto 0);
      signal s_scan_idx     : std_logic_vector(c_log_scan_size-1 downto 0);
      signal s_scan_ext     : std_logic_vector(c_ext_bits-1 downto 0);
      signal r_scan_ext     : std_logic_vector(c_ext_bits-1 downto 0);
      
      signal s_cal_a_data_o : std_logic_vector(c_data_wide-1 downto 0);
      signal s_cal_a_list_o : std_logic_vector(c_list_wide-1 downto 0);
      signal s_cal_b_en     : std_logic;
      signal s_cal_b_ack    : std_logic;
      signal s_cal_b_addr   : std_logic_vector(c_log_cal_size-1 downto 0);
      signal s_cal_b_data_o : std_logic_vector(c_data_wide-1 downto 0);
      signal s_cal_b_data_i : std_logic_vector(c_data_wide-1 downto 0);
      signal s_cal_b_list_o : std_logic_vector(c_list_wide-1 downto 0);
      signal s_cal_b_list_i : std_logic_vector(c_list_wide-1 downto 0);
      signal s_cal_record   : std_logic_vector(c_list_bits-1 downto 0);
      signal r_cal_record   : std_logic_vector(c_list_bits-1 downto 0);
      
      signal s_list_we      : std_logic;
      signal s_list_dat     : std_logic_vector(c_list_bits-1 downto 0);
      
      type t_slot_array is array(c_list_bits-1 downto 0) of std_logic_vector(c_slots-1 downto 0);
      signal s_slot_mux     : t_slot_array;
      signal s_slot_select  : std_logic_vector(c_slots-1 downto 0);
      
      -- Used to compute indexes into s_cal_[ab]_list_[io]
      function f_idx_sb(s, b : natural) return natural is
      begin
        return s * c_list_bits + b;
      end f_idx_sb;
      
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
          g_ext_size       => c_ext_bits,
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
          deadline_i   => channel_i.deadline,
          idx_i        => s_free_alloc(c_log_scan_size-1 downto 0),
          ext_i        => s_ext,
          scan_stb_o   => s_scan_stb,
          scan_late_o  => s_scan_late,
          scan_early_o => s_scan_early,
          scan_low_o   => s_scan_low,
          scan_idx_o   => s_scan_idx,
          scan_ext_o   => s_scan_ext);
      
      s_cal_record(c_list_code+1) <= not s_scan_early;
      s_cal_record(c_list_code+0) <= not s_scan_late;
      s_cal_record(c_log_scan_size+c_list_index-1 downto c_list_index) <= s_scan_idx;
      
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
      sel_slot_eq1 : if c_slots = 1 generate
        s_slot_select(0) <= '1';
      end generate;
      sel_slot_gt1 : if c_slots > 1 generate
        slots : for s in 0 to c_slots-1 generate
          s_slot_select(s) <= f_eca_eq(
            std_logic_vector(to_unsigned(s, g_log_multiplier)),
            r_scan_low(g_log_multiplier-1 downto 0));
        end generate;
      end generate;
      
      -- Insert action into the correct calendar slot
      slots1 : for s in 0 to c_slots-1 generate
        bits : for b in 0 to c_list_bits-1 generate
          s_cal_b_list_i(f_idx_sb(s,b)) <= 
            f_eca_mux(s_slot_select(s), r_cal_record(b), s_cal_b_list_o(f_idx_sb(s,b)));
        end generate;
      end generate;
      
      -- Save the slot being overwritten into the linked list
      s_list_we <= s_cal_b_ack;
      bits : for b in 0 to c_list_bits-1 generate
        slots : for s in 0 to c_slots-1 generate
          s_slot_mux(b)(s) <= s_cal_b_list_o(f_idx_sb(s, b)) and s_slot_select(s);
        end generate;
        s_list_dat(b) <= f_eca_or(s_slot_mux(b));
      end generate;
      
      -- The above muxes puts the most recently discovered action at the head of the list
      -- Thus, intra-bank tag conflict resolution prefers the latest action scanned
      
      -- Format: [code low-index]*c_slots
      calendar : eca_rmw
        generic map(
          g_addr_bits => c_log_cal_size,
          g_data_bits => c_data_wide)
        port map(
          clk_i    => clk_i,
          rst_n_i  => rst_n_i,
          a_en_i   => s_cal_a_en,
          a_ack_o  => open,
          a_addr_i => s_cal_a_addr,
          a_data_o => s_cal_a_data_o,
          a_data_i => (others => '0'),
          b_en_i   => s_cal_b_en,
          b_ack_o  => s_cal_b_ack,
          b_addr_i => s_cal_b_addr,
          b_data_o => s_cal_b_data_o,
          b_data_i => s_cal_b_data_i);
      
      s_cal_a_list_o <= s_cal_a_data_o(c_list_wide-1 downto 0);
      s_cal_b_list_o <= s_cal_b_data_o(c_list_wide-1 downto 0);
      s_cal_b_data_i(c_list_wide-1 downto 0) <= s_cal_b_list_i;
      
      -- No need to wipe on reset; it is only read if calendar pointed into it
      -- Format: [code low-index]
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
          w_addr_i => r_cal_record(c_list_index+c_log_scan_size-1 downto c_list_index),
          w_data_i => s_list_dat);

      -- Note: it is important that f_idx_sc(s,c) orders first by slot
      slots2 : for s in 0 to c_slots-1 generate
        -- Is this calendar entry valid?
        s_cal_valid(s)(c) <= 
          s_cal_a_list_o(f_idx_sb(s,c_log_scan_size+0)) or
          s_cal_a_list_o(f_idx_sb(s,c_log_scan_size+1));
        -- Decode code => not-empty
        s_fifo_push(f_idx_sc(s,c)) <= r_cal_a_en and s_cal_valid(s)(c);
        -- FIFO record flags
        s_fifo_data_i(f_idx_sc(s,c),c_fifo_next)   <= s_cal_next(s)(c);
        s_fifo_data_i(f_idx_sc(s,c),c_fifo_code+1) <= s_cal_a_list_o(f_idx_sb(s,c_list_code+1));
        s_fifo_data_i(f_idx_sc(s,c),c_fifo_code+0) <= s_cal_a_list_o(f_idx_sb(s,c_list_code+0));
        -- Fill high bits from calendar #
        high : if c_calendars > 1 generate
          bits : for b in 0 to c_log_calendars-1 generate
            s_fifo_data_i(f_idx_sc(s,c),c_fifo_index+c_log_scan_size+b) <= to_unsigned(c,c_log_calendars)(b);
          end generate;
        end generate;
        -- copy low bits
        bits : for b in 0 to c_log_scan_size-1 generate
          s_fifo_data_i(f_idx_sc(s,c),c_fifo_index+b) <= s_cal_a_list_o(f_idx_sb(s,c_list_index+b));
        end generate;
      end generate;
      
      -- Handle the extra IO calendar bits
      io_yes : if g_support_io generate
        bl : block is
          signal s_chan_select  : std_logic_vector(g_num_channels-1 downto 0);
          signal r_io_select    : std_logic;
          
          signal s_cal_a_gpio_o : std_logic_vector(c_gpio_wide-1 downto 0);
          signal s_cal_b_gpio_o : std_logic_vector(c_gpio_wide-1 downto 0);
          signal s_cal_b_gpio_i : std_logic_vector(c_gpio_wide-1 downto 0);
        begin
        
          input : process(clk_i) is
          begin
            if rising_edge(clk_i) then
              if s_scan_stb = '1' then
                r_scan_ext   <= s_scan_ext;
                r_io_select  <= (not s_scan_late  or s_scan_ext(c_ext_late)) and 
                                (not s_scan_early or s_scan_ext(c_ext_early));
              end if;
            end if;
          end process;
          
          -- Calculate which slot to put the action into
          sel_chan_eq1 : if g_num_channels <= 1 generate
            s_chan_select(0) <= '1';
          end generate;
          sel_chan_gt1 : if g_num_channels > 1 generate
            chans : for i in 0 to g_num_channels-1 generate
              s_chan_select(i) <= f_eca_eq(
                std_logic_vector(to_unsigned(i, c_log_channels)),
                r_scan_ext(c_ext_channel+c_log_channels-1 downto c_ext_channel));
            end generate;
          end generate;
          
          -- Insert action into the correct gpio slot
          chans : for i in 0 to g_num_channels-1 generate
            slots : for s in 0 to c_slots-1 generate
              s_cal_b_gpio_i(f_idx_si(s,i)+0) <=
                f_eca_mux(s_slot_select(s) and s_chan_select(i) and r_io_select,
                  f_eca_mux(s_cal_b_gpio_o(f_idx_si(s,i)+0), not r_scan_ext(1), r_scan_ext(0)),
                  s_cal_b_gpio_o(f_idx_si(s,i)+0));
              s_cal_b_gpio_i(f_idx_si(s,i)+1) <=
                f_eca_mux(s_slot_select(s) and s_chan_select(i) and r_io_select,
                  f_eca_mux(s_cal_b_gpio_o(f_idx_si(s,i)+1), not r_scan_ext(0), r_scan_ext(1)),
                  s_cal_b_gpio_o(f_idx_si(s,i)+1));
            end generate;
          end generate;
          
          -- The above muxes apply the action's clear/set to the output.
          -- In the case that another action already modified the state, it is overridden
          -- Thus, intra-bank io conflict resolution prefers the latest action scanned
          
          s_cal_a_gpio_o <= s_cal_a_data_o(c_data_wide-1 downto c_list_wide);
          s_cal_b_gpio_o <= s_cal_b_data_o(c_data_wide-1 downto c_list_wide);
          s_cal_b_data_i(c_data_wide-1 downto c_list_wide) <= s_cal_b_gpio_i;
          
          output : process(clk_i) is
          begin
            if rising_edge(clk_i) then
              r_gpio_matrix(c) <= s_cal_a_gpio_o;
            end if;
          end process;
        end block;
      end generate;
    end block;
  end generate; -- parallel_scan (c_calendars)
  
  -- Detect >1 actions in the same slots but from different calendars
  nexts : for s in 0 to c_slots-1 generate
    s_cal_next(s) <= s_cal_valid(s) and (s_cal_valid(s) - 1);
  end generate;
  
  -- Format: [next code index]
  fifo : eca_piso_fifo
    generic map(
      g_log_size  => g_log_size,
      g_log_ports => g_log_multiplier + c_log_calendars,
      g_width     => c_fifo_bits)
    port map(
      clk_i   => clk_i,
      rst_n_i => rst_n_i,
      push_i  => r_fifo_push,
      data_i  => r_fifo_data_i,
      pop_i   => s_fifo_pop,
      valid_o => s_fifo_valid,
      fresh_o => s_fifo_fresh,
      data_o  => s_fifo_data_o);
  
  -- The FIFO compacts inputs and lowest indexes go first.
  -- Thus, inter-bank tag conflict resolution prefers the lowest calendar's action.
  
  -- Fetch the record from the data table, snooping whenever possible
  s_data_index <= 
    f_eca_mux(not s_mux_valid or s_stall1, snoop_i,
      s_mux_data(c_fifo_index+g_log_size-1 downto c_fifo_index));
  
  -- Grab the follow-up action from the list
  s_list_addr <= 
    f_eca_mux(s_stall1, 
      r_mux_data(c_fifo_index+c_log_scan_size-1 downto c_fifo_index),
      s_mux_data(c_fifo_index+c_log_scan_size-1 downto c_fifo_index));
  
  -- Mux out the record from the linked lists
  con_eq1 : if c_calendars = 1 generate
    s_list_record <= s_list_mux(0);
  end generate;
  con_gt1 : if c_calendars > 1 generate
    con : block is
      signal s_mux_idx : std_logic_vector(c_log_calendars-1 downto 0);
    begin
      s_mux_idx <= r_mux_data(c_fifo_index+g_log_size-1 downto c_fifo_index+c_log_scan_size);
      s_list_record <= s_list_mux(to_integer(unsigned(s_mux_idx))) when f_eca_safe(s_mux_idx) = '1' else (others => 'X');
    end block;
  end generate;
  s_list_code <= s_list_record(c_list_code+1 downto c_list_code);
  
  -- Next record comes from linked list or fifo?
  s_list <= r_mux_valid and not f_eca_eq(s_list_code, c_code_empty);
  s_mux_valid <= s_list or s_fifo_valid;

  -- Pop the record if we're done with it
  s_fifo_pop <= s_fifo_valid and not (s_list or s_stall1);
  
  -- Expand the list record entry to the same format as a fifo entry
  s_mux_data_l(c_fifo_next) <= '1';
  s_mux_data_l(c_fifo_code+1 downto c_fifo_code) <= s_list_code;
  s_mux_data_l(c_fifo_index+c_log_scan_size-1 downto c_fifo_index) <=
    s_list_record(c_list_index+c_log_scan_size-1 downto c_list_index);
  cal_gt1 : if c_calendars > 1 generate
    s_mux_data_l(c_fifo_index+g_log_size-1 downto c_fifo_index+c_log_scan_size) <= 
      r_mux_data(c_fifo_index+g_log_size-1 downto c_fifo_index+c_log_scan_size);
  end generate;
  -- Select linked list in prefernce to FIFO data
  s_mux_data <= f_eca_mux(s_list, s_mux_data_l, s_fifo_data_o);
  s_mux_next <= s_mux_data(c_fifo_next);
  s_mux_code <= s_mux_data(c_fifo_code+1 downto c_fifo_code);

  -- Which number is this?
  num_le1 : if g_num_channels <= 1 generate
    s_mux_num <= "0";
  end generate;
  num_gt1 : if g_num_channels > 1 generate
    s_mux_num <= s_mux_channel.num(c_log_channels-1 downto 0);
  end generate;
  
  -- Was something on this time slot already for that number?
  s_saw_valid <= r_saw_valid(to_integer(unsigned(s_mux_num))) when f_eca_safe(s_mux_num)='1' else 'X';
  
  control : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_mux_valid <= '0';
      r_stall     <= '0';
      r_saw_valid <= (others => '0');
      r_fifo_push <= (others => '0');
    elsif rising_edge(clk_i) then
      r_stall <= s_stall1;
      r_fifo_push <= s_fifo_push;
      if s_stall1 = '0' then
        r_mux_valid <= s_mux_valid;
        -- Record which channel numbers have had a valid/delayed action
        if r_mux_next = '0' then
          r_saw_valid <= (others => '0');
        end if;
        if (r_mux_valid and r_mux_ontime) = '1' then
          r_saw_valid(to_integer(unsigned(s_mux_num))) <= '1';
        end if;
      end if;
    end if;
  end process;
  
  bulk : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      r_fifo_data_i<= s_fifo_data_i;
      if s_stall1 = '0' then
        r_mux_ontime<= f_eca_eq(s_mux_code, c_code_valid);
        r_mux_late  <= f_eca_eq(s_mux_code, c_code_late);
        r_mux_early <= f_eca_eq(s_mux_code, c_code_early);
        r_mux_next  <= s_mux_next;
        r_mux_delay <= s_list or not s_fifo_fresh;
        r_mux_data    <= s_mux_data;
        r_data_index1 <= s_data_index;
      end if;
      if s_stall2 = '0' then
        r_data_index2 <= r_data_index1;
      end if;
      if r_stall = '0' then
        r_skid_channel <= s_data_channel;
      end if;
    end if;
  end process;
  
  -- Implement a skidpad to allow us to snoop while stalled
  --   => this is necessary so we can make forward progress reading errors in saftlib
  --      otherwise, a receiving component could hang the channel and thus hang wishbone
  -- This bypass is what makes it safe to set s_data_index to snoop_i
  s_mux_channel.valid    <= '1';
  s_mux_channel.delayed  <= f_eca_mux(r_stall, r_skid_channel.delayed,  s_data_channel.delayed);
  s_mux_channel.conflict <= f_eca_mux(r_stall, r_skid_channel.conflict, s_data_channel.conflict);
  s_mux_channel.late     <= f_eca_mux(r_stall, r_skid_channel.late,     s_data_channel.late);
  s_mux_channel.early    <= f_eca_mux(r_stall, r_skid_channel.early,    s_data_channel.early);
  s_mux_channel.num      <= f_eca_mux(r_stall, r_skid_channel.num,      s_data_channel.num);
  s_mux_channel.event    <= f_eca_mux(r_stall, r_skid_channel.event,    s_data_channel.event);
  s_mux_channel.param    <= f_eca_mux(r_stall, r_skid_channel.param,    s_data_channel.param);
  s_mux_channel.tag      <= f_eca_mux(r_stall, r_skid_channel.tag,      s_data_channel.tag);
  s_mux_channel.tef      <= f_eca_mux(r_stall, r_skid_channel.tef,      s_data_channel.tef);
  s_mux_channel.deadline <= f_eca_mux(r_stall, r_skid_channel.deadline, s_data_channel.deadline);
  s_mux_channel.executed <= (others => '0');

  -- Determine exceptional conditions
  -- late/early/conflict/delayed are mutually exclusive; most severe first
  s_late     <= r_mux_valid and r_mux_late;
  s_early    <= r_mux_valid and r_mux_early;
  s_conflict <= (r_mux_valid and r_mux_next and s_saw_valid) and not (s_late or s_early);
  s_delayed  <= (r_mux_valid and (r_mux_delay or r_stall))   and not (s_late or s_early or s_conflict)
                and not f_eca_active_high(g_never_delayed);
  
  -- Only valid if the errors are accepted by the condition rule
  s_valid <= r_mux_valid and
    (not s_delayed  or s_mux_channel.delayed)  and
    (not s_conflict or s_mux_channel.conflict) and
    (not s_late     or s_mux_channel.late)     and
    (not s_early    or s_mux_channel.early);
  
  -- Stall if we are reporting something not accepted
  s_stall2 <= stall_i and r_channel.valid;
  s_stall1 <= s_stall2 and r_mux_valid;
  
  -- Register the outputs
  output_control : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_channel.valid    <= '0';
      r_channel.delayed  <= '0';
      r_channel.conflict <= '0';
      r_channel.late     <= '0';
      r_channel.early    <= '0';
    elsif rising_edge(clk_i) then
      if s_stall2 = '0' then
        r_channel.valid    <= s_valid;
        r_channel.delayed  <= s_delayed;
        r_channel.conflict <= s_conflict;
        r_channel.late     <= s_late;
        r_channel.early    <= s_early;
      end if;
    end if;
  end process;
  
  output_bulk : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      if s_stall2 = '0' then
        r_channel.num      <= s_mux_channel.num;
        r_channel.event    <= s_mux_channel.event;
        r_channel.param    <= s_mux_channel.param;
        r_channel.tag      <= s_mux_channel.tag;
        r_channel.tef      <= s_mux_channel.tef;
        r_channel.deadline <= s_mux_channel.deadline;
        r_channel.executed <= f_eca_add(time_i, 2**g_log_multiplier);
      end if;
    end if;
  end process;
  
  -- We're done!
  channel_o <= r_channel;
  index_o   <= r_data_index2;
  
  -- Report snooped action
  snoop_o    <= s_data_channel;
  snoop_ok_o <= r_stall or not r_mux_valid;
  
  -- Combine all the toggles together to form the GPIO output
  io_yes : if g_support_io generate
    bl : block is
      signal r_gpio_valid   : std_logic := '0';
      signal r_gpio         : std_logic_vector(g_num_channels-1 downto 0) := (others => '0');
    begin
      gpios : process(clk_i, rst_n_i) is
        variable val : std_logic;
      begin
        if rst_n_i = '0' then
          r_gpio_valid <= '0';
          r_gpio <= (others => '0');
          io_o   <= (others => (others => '0'));
        elsif rising_edge(clk_i) then
          r_gpio_valid <= r_cal_a_en;
          if r_gpio_valid = '1' then
            for i in 0 to g_num_channels-1 loop
              val := r_gpio(i);
              for s in 0 to c_slots-1 loop
                for c in c_calendars-1 downto 0 loop
                  val := f_eca_mux(val, not r_gpio_matrix(c)(f_idx_si(s,i)+1), r_gpio_matrix(c)(f_idx_si(s,i)+0));
                end loop;
                io_o(i,s) <= val;
              end loop;
              r_gpio(i) <= val;
            end loop;
          end if;
        end if;
      end process;
    end block;
  end generate;
  
  -- The muxes iterate over calendars from largest to smallest, smallest overwrites largest
  -- Thus, inter-bank io conflict resolution prefers the lowest calendar's action.
  
  io_no : if not g_support_io generate
    io_o <= (others => (others => '0'));
  end generate;

end rtl;
