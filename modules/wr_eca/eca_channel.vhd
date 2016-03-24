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

entity eca_channel is
  generic(
    g_support_io     : boolean := false; -- Should io_o be driven?
    g_num_channels   : natural :=  1; -- Number of channels emulated by this instance
    g_log_size       : natural :=  8; -- 2**g_log_size = maximum number of pending actions
    g_log_multiplier : natural :=  3; -- 2**g_log_multiplier = ticks per cycle
    g_log_max_delay  : natural := 32; -- 2**g_log_max_delay  = maximum delay before executed as early
    g_log_latency    : natural := 12; -- 2**g_log_latency    = ticks of calendar delay
    g_log_counter    : natural := 20; -- number of bits in the counters reported
    g_log_crossing   : natural := 4); -- snoop_clk_i and clk_i must be within factor 2**g_log_crossing
  port(
    clk_i      : in  std_logic;
    rst_n_i    : in  std_logic;
    -- Timestamps used for pipeline stages
    time_i     : in  t_time;
    -- Push a record to the queue
    overflow_o : out std_logic;
    channel_i  : in  t_channel;
    clr_i      : in  std_logic;
    set_i      : in  std_logic;
    num_i      : in  std_logic_vector(f_eca_log2_min1(g_num_channels)-1 downto 0);
    -- Read-out port for failed actions
    snoop_clk_i   : in  std_logic;
    snoop_rst_n_i : in  std_logic;
    snoop_stb_i   : in  std_logic; -- positive edge triggered
    snoop_free_i  : in  std_logic; -- record should be released by this read
    snoop_num_i   : in  std_logic_vector(f_eca_log2_min1(g_num_channels)-1 downto 0);
    snoop_type_i  : in  std_logic_vector(1 downto 0); -- 0=late, 1=early, 2=conflict, 3=delayed
    snoop_field_i : in  std_logic_vector(2 downto 0); -- 0+1=event, 2+3=param, 4=tag, 5=tef, 6+7=time
    snoop_valid_o : out std_logic;
    snoop_data_o  : out std_logic_vector(31 downto 0);
    snoop_count_o : out std_logic_vector(g_log_counter-1 downto 0); -- saturates if not freed
    num_overflow_o: out std_logic_vector(g_log_counter-1 downto 0); -- wraps around (also snoop_clk_i)
    num_valid_o   : out std_logic_vector(g_log_counter-1 downto 0); -- ditto
    msi_ack_i     : in  std_logic;
    msi_stb_o     : out std_logic;
    msi_dat_o     : out std_logic_vector(15 downto 0);
    -- Output of the channel
    stall_i    : in  std_logic;
    channel_o  : out t_channel;
    num_o      : out std_logic_vector(f_eca_log2_min1(g_num_channels)-1 downto 0);
    io_o       : out t_eca_matrix(g_num_channels-1 downto 0, 2**g_log_multiplier-1 downto 0));
end eca_channel;

architecture rtl of eca_channel is

  constant c_count_bits : natural := g_log_counter;
  constant c_addr_bits  : natural := 2 + f_eca_log2_min1(g_num_channels);
  constant c_data_bits  : natural := g_log_size + c_count_bits;
  constant c_num_bits   : natural := f_eca_log2_min1(g_num_channels);
  
  constant c_zero : std_logic_vector(c_count_bits-1 downto 0) := (others => '0');
  
  signal s_channel  : t_channel;
  signal s_snoop    : t_channel;
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

  signal s_wen      : std_logic;
  signal s_ridx     : std_logic_vector(c_addr_bits-1 downto 0);
  signal r_ridx     : std_logic_vector(c_addr_bits-1 downto 0);
  signal s_widx     : std_logic_vector(c_addr_bits-1 downto 0);
  signal s_data_i   : std_logic_vector(c_data_bits-1 downto 0);
  signal s_data_o   : std_logic_vector(c_data_bits-1 downto 0);
  signal s_count_o  : std_logic_vector(c_count_bits-1 downto 0);
  signal s_count_i  : std_logic_vector(c_count_bits-1 downto 0);
  signal s_index_o  : std_logic_vector(g_log_size-1 downto 0);
  signal s_index_i  : std_logic_vector(g_log_size-1 downto 0);
  signal s_zero     : std_logic;
  
  -- Latched by snoop_clk_i, but held until request is complete
  signal s_req_in    : std_logic;
  signal rs_req_free : std_logic;
  signal rs_req_num  : std_logic_vector(c_num_bits-1 downto 0);
  signal rs_req_type : std_logic_vector(1 downto 0);
  signal rs_req_field: std_logic_vector(2 downto 0);
  signal s_req_rok   : std_logic;
  signal r_req_rok   : std_logic := '1';
  signal rc_req_free : std_logic;
  signal rc_req_num  : std_logic_vector(c_num_bits-1 downto 0);
  signal rc_req_type : std_logic_vector(1 downto 0);
  signal rc_req_field: std_logic_vector(2 downto 0);
  
  -- Request signalling
  signal r_req_old  : std_logic := '0'; -- snoop_clk
  signal r_req_xor1 : std_logic := '0'; -- snoop_clk
  signal r_req_xor2 : std_logic := '0'; -- clk
  signal r_req_xor3 : std_logic := '0'; -- clk
  signal r_req_xor4 : std_logic := '0'; -- clk
  
  -- Remember the index requested
  signal r_req_aok : std_logic := '1'; -- address ok
  signal r_req_sok : std_logic := '1'; -- snoop ok
  signal s_req_idx : std_logic;
  signal r_req_idx : std_logic_vector(g_log_size-1 downto 0);
  signal rc_req_cnt: std_logic_vector(c_count_bits-1 downto 0);
  
  -- Remember the data requested
  signal s_req_dok : std_logic;
  signal r_req_dok : std_logic := '1';
  signal s_req_dat : std_logic_vector(31 downto 0);
  signal rc_req_dat: std_logic_vector(31 downto 0);
  
  -- Synchronize the output strobe
  signal s_req_ack  : std_logic;
  signal r_req_ack  : std_logic := '1';
  signal r_req_xor5 : std_logic := '0'; -- clk
  signal r_req_xor6 : std_logic := '0'; -- snoop_clk
  signal r_req_xor7 : std_logic := '0'; -- snoop_clk
  signal r_req_xor8 : std_logic := '0'; -- snoop_clk
  signal r_req_out  : std_logic := '0'; -- snoop_clk
  
  -- Final output registers
  signal s_req_ook  : std_logic;
  signal rs_req_cnt : std_logic_vector(c_count_bits-1 downto 0);
  signal rs_req_dat : std_logic_vector(31 downto 0);
  
  -- Saturated increment
  function f_increment(x : std_logic_vector) return std_logic_vector is
  begin
    return f_eca_mux(f_eca_and(x), x, f_eca_add(x, 1));
  end f_increment;
  
  -- Interrupt vector processing and crossing
  signal r_raised    : std_logic_vector(g_num_channels*4-1 downto 0) := (others => '0');
  signal r_masked    : std_logic_vector(g_num_channels*4-1 downto 0) := (others => '0');
  signal s_pending   : std_logic_vector(g_num_channels*4-1 downto 0);
  signal s_selected  : std_logic_vector(g_num_channels*4-1 downto 0);
  signal r_selected  : std_logic_vector(g_num_channels*4-1 downto 0);
  signal sc_msi_rdy  : std_logic;
  signal rc_msi_rdy  : std_logic := '0';
  signal rc_msi_xor  : std_logic := '0';
  signal rc_msi_out  : std_logic_vector(f_eca_log2(g_num_channels)+1 downto 0);
  signal rs_msi_xor1 : std_logic := '0';
  signal rs_msi_xor2 : std_logic := '0';
  signal rs_msi_xor3 : std_logic := '0';
  signal ss_msi_rdy  : std_logic;
  signal rs_msi_rdy  : std_logic := '0';
  signal rs_msi_out  : std_logic_vector(f_eca_log2(g_num_channels)+1 downto 0);
  signal rs_ack_xor  : std_logic := '0';
  signal rc_ack_xor1 : std_logic := '0';
  signal rc_ack_xor2 : std_logic := '0';
  signal rc_ack_xor3 : std_logic := '0';
  signal sc_ack      : std_logic;
  
  -- Cross simple counters across the domains
  signal sc_overflow       : std_logic;
  signal rc_overflow_sum   : std_logic_vector(g_log_crossing-1 downto 0) := (others => '0');
  signal rc_overflow_gray  : std_logic_vector(g_log_crossing-1 downto 0);
  signal rs0_overflow_gray : std_logic_vector(g_log_crossing-1 downto 0);
  signal rs1_overflow_gray : std_logic_vector(g_log_crossing-1 downto 0);
  signal rs_overflow_sum   : std_logic_vector(g_log_crossing-1 downto 0);
  signal rs_overflow_done  : std_logic_vector(g_log_crossing-1 downto 0);
  signal rs_overflow       : std_logic_vector(g_log_counter- 1 downto 0) := (others => '0');
  signal sc_valid          : std_logic;
  signal rc_valid_sum      : std_logic_vector(g_log_crossing-1 downto 0) := (others => '0');
  signal rc_valid_gray     : std_logic_vector(g_log_crossing-1 downto 0);
  signal rs0_valid_gray    : std_logic_vector(g_log_crossing-1 downto 0);
  signal rs1_valid_gray    : std_logic_vector(g_log_crossing-1 downto 0);
  signal rs_valid_sum      : std_logic_vector(g_log_crossing-1 downto 0);
  signal rs_valid_done     : std_logic_vector(g_log_crossing-1 downto 0);
  signal rs_valid          : std_logic_vector(g_log_counter -1 downto 0) := (others => '0');

begin

  channel : eca_tag_channel
    generic map(
      g_support_io     => g_support_io,
      g_num_channels   => g_num_channels,
      g_log_size       => g_log_size,
      g_log_multiplier => g_log_multiplier,
      g_log_max_delay  => g_log_max_delay,
      g_log_latency    => g_log_latency)
    port map(
      clk_i      => clk_i,
      rst_n_i    => rst_n_i,
      time_i     => time_i,
      overflow_o => sc_overflow,
      channel_i  => channel_i,
      clr_i      => clr_i,
      set_i      => set_i,
      num_i      => num_i,
      snoop_i    => r_req_idx,
      snoop_o    => s_snoop,
      snoop_ok_o => s_snoop_ok,
      free_i     => r_free_stb,
      index_i    => r_free_idx,
      stall_i    => stall_i,
      channel_o  => s_channel,
      num_o      => s_num,
      index_o    => s_index,
      io_o       => io_o);
  
  channel_o <= s_channel;
  num_o     <= s_num;
  
  -- Goal is to intercept error conditions and record their indices
  saved : eca_sdp
    generic map(
      g_addr_bits  => c_addr_bits,
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
  
  s_error <= s_channel.late or s_channel.early or s_channel.conflict or s_channel.delayed;
  s_final <= f_eca_mux(s_channel.valid, not stall_i, s_error); -- Index should be freed (if not stolen)
  s_busy  <= s_error and s_final; -- Is this the final time we see this error?
  s_steal <= r_busy and s_zero; -- Record this error for software diagnosis

  -- If the request was to free the error, do it on final ack
  s_late_free <= s_req_ack and rc_req_free;
  
  -- r_final and s_late_free are mutually exclusive
  s_free_stb <= (r_final and not s_steal) or (s_late_free and f_eca_or(rc_req_cnt));
  s_free_idx <= f_eca_mux(r_final, r_index, r_req_idx);
  
  -- code: 0=late, 1=early, 2=conflict, 3=delayed
  s_code(0) <= s_channel.early    or s_channel.delayed;
  s_code(1) <= s_channel.conflict or s_channel.delayed;
  
  s_ridx <= f_eca_mux(s_busy, s_num & s_code, rc_req_num & rc_req_type);
  s_widx <= r_ridx;
  
  s_count_o <= s_data_o(c_data_bits-1 downto g_log_size);
  s_index_o <= s_data_o(g_log_size-1 downto 0);
  s_zero    <= not f_eca_or(s_count_o);
  
  -- Atomically read the current counter+index, while atomically wiping them out for new errors
  -- Note: for this to work, we need that the table is bypassed
  s_atom_free <= s_req_idx and rc_req_free;

  s_wen     <= r_busy or s_atom_free; -- r_busy and s_atom_free are mutually exclusive
  s_count_i <= f_eca_mux(r_busy, f_increment(s_count_o), c_zero);
  s_index_i <= f_eca_mux(s_zero, r_index, s_index_o);
  s_data_i  <= s_count_i & s_index_i;
  
  with rc_req_field select
  s_req_dat <=
    s_snoop.event(63 downto 32) when "000",
    s_snoop.event(31 downto  0) when "001",
    s_snoop.param(63 downto 32) when "010",
    s_snoop.param(31 downto  0) when "011",
    s_snoop.tag  (31 downto  0) when "100",
    s_snoop.tef  (31 downto  0) when "101",
    s_snoop.time (63 downto 32) when "110",
    s_snoop.time (31 downto  0) when "111",
    (others => 'X') when others;
  
  -- Clock enable for registers going from snoop_clk => clk (snoop_clk side)
  s_req_in <= not r_req_old and snoop_stb_i;
  -- Clock enable for registers going from snoop_clk => clk (clk side)
  s_req_rok <= r_req_xor4 xor r_req_xor3;
  -- Clock enable for count register going from clk => snoop_clk (clk side)
  s_req_idx <= r_req_rok and not r_req_aok and not r_busy;
  -- Clock enable for data register going from clk => snoop_clk (clk side)
  s_req_dok <= s_snoop_ok and r_req_sok and not r_req_dok;
  -- Only allow the request to complete when there's a slot we could potentially free in
  s_req_ack <= r_req_dok and not r_final and not r_req_ack;
  -- Clock enable for the registers going from clk => snoop_clk (snoop_clk side)
  s_req_ook <= r_req_xor8 xor r_req_xor7;
  
  stat_control : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_free_stb <= '0';
      r_busy     <= '0';
      r_final    <= '0';
    elsif rising_edge(clk_i) then
      r_free_stb <= s_free_stb;
      r_busy     <= s_busy;
      r_final    <= s_final;
    end if;
  end process;
  
  stat_bulk : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      r_ridx      <= s_ridx;
      r_index     <= s_index;
      r_free_idx  <= s_free_idx;
    end if;
  end process;
  
  in_control : process(snoop_clk_i, snoop_rst_n_i) is
  begin
    if snoop_rst_n_i = '0' then
      r_req_old  <= '0';
      r_req_xor1 <= '0';
    elsif rising_edge(snoop_clk_i) then
      if snoop_stb_i = '1' then
        r_req_old <= '1';
      end if;
      if r_req_out = '1' then
        r_req_old <= '0';
      end if;
      r_req_xor1 <= r_req_xor1 xor s_req_in;
    end if;
  end process;
  
  in_bulk : process(snoop_clk_i) is
  begin
    if rising_edge(snoop_clk_i) then
      if s_req_in = '1' then
        rs_req_free  <= snoop_free_i;
        rs_req_num   <= snoop_num_i;
        rs_req_type  <= snoop_type_i;
        rs_req_field <= snoop_field_i;
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
        rc_req_free  <= rs_req_free;
        rc_req_num   <= rs_req_num;
        rc_req_type  <= rs_req_type;
        rc_req_field <= rs_req_field;
      end if;
      if s_req_idx = '1' then
        r_req_idx  <= s_index_o;
        rc_req_cnt <= s_count_o;
      end if;
      if s_req_dok = '1' then
        rc_req_dat <= s_req_dat;
      end if;
    end if;
  end process;
  
  out_control : process(snoop_clk_i, snoop_rst_n_i) is
  begin
    if snoop_rst_n_i = '0' then
      r_req_xor6 <= '0';
      r_req_xor7 <= '0';
      r_req_xor8 <= '0';
      r_req_out  <= '0';
    elsif rising_edge(snoop_clk_i) then
      r_req_xor6 <= r_req_xor5;
      r_req_xor7 <= r_req_xor6;
      r_req_xor8 <= r_req_xor7;
      r_req_out  <= s_req_ook;
    end if;
  end process;
  
  out_bulk : process(snoop_clk_i) is
  begin
    if rising_edge(snoop_clk_i) then
      if s_req_ook = '1' then
        rs_req_cnt <= rc_req_cnt;
        rs_req_dat <= rc_req_dat;
      end if;
    end if;
  end process;
  
  snoop_valid_o <= r_req_out;
  snoop_data_o  <= rs_req_dat;
  snoop_count_o <= rs_req_cnt;
  
  -- Calculate the MSI
  sc_msi_rdy <= f_eca_or(r_selected);
  msi_control : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_raised    <= (others => '0');
      r_masked    <= (others => '0');
      rc_msi_rdy  <= '0';
      rc_msi_xor  <= '0';
      rc_ack_xor1 <= '0';
      rc_ack_xor2 <= '0';
      rc_ack_xor3 <= '0';
    elsif rising_edge(clk_i) then
      -- Set/clear the interrupt corresponding to written table entry
      if s_wen = '1' then
        r_raised(to_integer(unsigned(s_widx))) <= r_busy;
      end if;
      -- Mask out an interrupt after we've selected it for delivery already
      if rc_msi_rdy = '0' then
        r_masked   <= r_masked or r_selected;
        rc_msi_rdy <= sc_msi_rdy;
        rc_msi_xor <= rc_msi_xor xor sc_msi_rdy;
      else
        rc_msi_rdy <= rc_ack_xor3 xnor rc_ack_xor2;
      end if;
      -- When counters are cleared, demask the interrupt so it can be delivered again
      if s_atom_free = '1' then
        r_masked(to_integer(unsigned(s_widx))) <= '0';
      end if;
      -- Sync the ack
      rc_ack_xor1 <= rs_ack_xor;
      rc_ack_xor2 <= rc_ack_xor1;
      rc_ack_xor3 <= rc_ack_xor2;
    end if;
  end process;
  
  -- Arbitrate the raised interrupts
  s_pending  <= r_raised and not r_masked;
  s_selected <= s_pending and f_eca_add(not s_pending, 1);
  
  msi_decode : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      -- The 2-cycle recurrance between r_masked=>r_selected=>r_masked is safe,
      -- because rc_msi_rdy must transition =>1 and then =>0 before the next pop.
      r_selected <= s_selected;
      if rc_msi_rdy = '0' then
        rc_msi_out <= f_eca_1hot_decode(r_selected);
      end if;
    end if;
  end process;
  
  ss_msi_rdy <= rs_msi_xor2 xor rs_msi_xor3;
  msi_output : process(snoop_clk_i, snoop_rst_n_i)  is
  begin
    if snoop_rst_n_i = '0' then
      rs_msi_xor1 <= '0';
      rs_msi_xor2 <= '0';
      rs_msi_xor3 <= '0';
      rs_msi_rdy  <= '0';
      rs_ack_xor  <= '0';
    elsif rising_edge(snoop_clk_i) then
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
  
  msi_output_bulk : process(snoop_clk_i) is
  begin
    if rising_edge(snoop_clk_i) then
      if ss_msi_rdy = '1' then
        rs_msi_out <= rc_msi_out;
      end if;
    end if;
  end process;
  
  msi_stb_o <= rs_msi_rdy;
  msi_dat_o(msi_dat_o'high downto rs_msi_out'high+1) <= (others => '0');
  msi_dat_o(rs_msi_out'range) <= rs_msi_out;
  
  -- Number of output actions
  sc_valid <= s_channel.valid and not stall_i;
  
  counters_control : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      rc_overflow_sum <= (others => '0');
      rc_valid_sum    <= (others => '0');
    elsif rising_edge(clk_i) then
      rc_overflow_sum <= f_eca_add(rc_overflow_sum, sc_overflow);
      rc_valid_sum    <= f_eca_add(rc_valid_sum,    sc_valid);
    end if;
  end process;
  
  counters_main : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      rc_overflow_gray <= f_eca_gray_encode(rc_overflow_sum);
      rc_valid_gray    <= f_eca_gray_encode(rc_valid_sum);
    end if;
  end process;
  
  counters_out : process(snoop_clk_i) is
  begin
    if rising_edge(snoop_clk_i) then
      rs0_overflow_gray <= rc_overflow_gray;
      rs1_overflow_gray <= rs0_overflow_gray;
      rs0_valid_gray    <= rc_valid_gray;
      rs1_valid_gray    <= rs0_valid_gray;
      rs_overflow_sum   <= f_eca_gray_decode(rs1_overflow_gray, 1);
      rs_valid_sum      <= f_eca_gray_decode(rs1_valid_gray, 1);
      rs_overflow_done  <= rs_overflow_sum;
      rs_valid_done     <= rs_valid_sum;
    end if;
  end process;
  
  counters_out_control : process(snoop_clk_i, snoop_rst_n_i) is
  begin
    if snoop_rst_n_i = '0' then
      rs_overflow <= (others => '0');
      rs_valid    <= (others => '0');
    elsif rising_edge(snoop_clk_i) then
      rs_overflow <= f_eca_delta(rs_overflow, rs_overflow_done, rs_overflow_sum);
      rs_valid    <= f_eca_delta(rs_valid,    rs_valid_done,    rs_valid_sum);
    end if;
  end process;
  
  overflow_o     <= sc_overflow;
  num_overflow_o <= rs_overflow;
  num_valid_o    <= rs_valid;

end rtl;
