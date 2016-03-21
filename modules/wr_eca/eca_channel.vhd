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
    channel_i  : in  t_channel;
    clr_i      : in  std_logic;
    set_i      : in  std_logic;
    num_i      : in  std_logic_vector(f_eca_log2_min1(g_num_channels)-1 downto 0);
    -- Inspect the action while idle
    --snoop_free_i  : in  std_logic; -- fetches counter, and clears it
    -- num&type affect index choice
    snoop_num_i   : in  std_logic_vector(f_eca_log2_min1(g_num_channels)-1 downto 0);
    snoop_type_i  : in  std_logic_vector(1 downto 0); -- 0=late, 1=early, 2=conflict, 3=delayed
    snoop_field_i : in  std_logic_vector(2 downto 0); -- 0+1=event, 2+3=param, 4=tag, 5=tef, 6+7=time
    snoop_valid_o : out std_logic;
    snoop_data_o  : out std_logic_vector(31 downto 0);
    -- snoop_count_o : out std_logic_vector(19 downto 0);
    --msi_stb_o  : out std_logic;
    --msi_ack_i  : in  std_logic;
    --msi_low_o  : out std_logic_vector(15 downto 0); -- # (type) ... (num)
    -- Output of the channel
    stall_i    : in  std_logic;
    channel_o  : out t_channel;
    num_o      : out std_logic_vector(f_eca_log2_min1(g_num_channels)-1 downto 0);
    io_o       : out t_eca_matrix(g_num_channels-1 downto 0, 2**g_log_multiplier-1 downto 0));
end eca_channel;

architecture rtl of eca_channel is

  constant c_count_bits : natural := 20;
  constant c_addr_bits  : natural := 2 + f_eca_log2_min1(g_num_channels);
  constant c_data_bits  : natural := g_log_size + c_count_bits;
  
  constant c_zero : std_logic_vector(c_count_bits-1 downto 0) := (others => '0');
  
  signal s_channel  : t_channel;
  signal s_snoop    : t_channel;
  signal s_num      : std_logic_vector(f_eca_log2_min1(g_num_channels)-1 downto 0);
  signal s_snoop_ok : std_logic;
  signal s_free     : std_logic;
  signal s_index    : std_logic_vector(g_log_size-1 downto 0);
  signal r_index    : std_logic_vector(g_log_size-1 downto 0);
  
  signal s_error    : std_logic;
  signal r_error    : std_logic;
  signal s_final    : std_logic;
  signal r_final    : std_logic;
  signal s_busy     : std_logic;
  signal r_busy     : std_logic := '0';
  signal rr_busy    : std_logic;
  signal s_steal    : std_logic;
  signal s_code     : std_logic_vector(1 downto 0);

  signal s_wen      : std_logic;
  signal s_ridx     : std_logic_vector(c_addr_bits-1 downto 0);
  signal r_ridx     : std_logic_vector(c_addr_bits-1 downto 0);
  signal s_widx     : std_logic_vector(c_addr_bits-1 downto 0);
  signal s_data_i   : std_logic_vector(c_data_bits-1 downto 0);
  signal s_data_o   : std_logic_vector(c_data_bits-1 downto 0);
  
  signal s_count_o  : std_logic_vector(c_count_bits-1 downto 0);
  signal s_zero     : std_logic;
  
  signal r_snoop_field1 : std_logic_vector(2 downto 0);
  signal r_snoop_field2 : std_logic_vector(2 downto 0);
  
begin

  channel : eca_tag_channel
    generic map(
      g_num_channels   => g_num_channels,
      g_log_size       => g_log_size,
      g_log_multiplier => g_log_multiplier,
      g_log_max_delay  => g_log_max_delay,
      g_log_latency    => g_log_latency)
    port map(
      clk_i      => clk_i,
      rst_n_i    => rst_n_i,
      time_i     => time_i,
      overflow_o => overflow_o,
      channel_i  => channel_i,
      clr_i      => clr_i,
      set_i      => set_i,
      num_i      => num_i,
      snoop_i    => s_data_o(g_log_size-1 downto 0),
      snoop_o    => s_snoop,
      snoop_ok_o => s_snoop_ok,
      free_i     => s_free,
      index_i    => r_index,
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
  
  -- !!! probably a good idea to register this
  s_free  <= r_final and not s_steal;
  
  -- code: 0=late, 1=early, 2=conflict, 3=delayed
  s_code(0) <= s_channel.early    or s_channel.delayed;
  s_code(1) <= s_channel.conflict or s_channel.delayed;
  
  s_ridx <= f_eca_mux(s_busy, s_num & s_code, snoop_num_i & snoop_type_i);
  s_widx <= r_ridx;
  
  s_count_o <= s_data_o(c_data_bits-1 downto g_log_size);
  s_zero    <= not f_eca_or(s_count_o);
  
  s_wen    <= r_busy;
  s_data_i <= f_eca_add(s_count_o, 1) & f_eca_mux(s_zero, r_index, s_data_o(g_log_size-1 downto 0));
 
  -- !!! latch it when valid, and then hold it
  with r_snoop_field2 select
  snoop_data_o <=
    s_snoop.event(63 downto 32) when "000",
    s_snoop.event(31 downto  0) when "001",
    s_snoop.param(63 downto 32) when "010",
    s_snoop.param(31 downto  0) when "011",
    s_snoop.tag  (31 downto  0) when "100",
    s_snoop.tef  (31 downto  0) when "101",
    s_snoop.time (63 downto 32) when "110",
    s_snoop.time (31 downto  0) when "111",
    (others => 'X') when others;
  
  snoop_valid_o <= not rr_busy and s_snoop_ok;
  
  main : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      r_snoop_field1 <= snoop_field_i;
      r_snoop_field2 <= r_snoop_field1;
      r_ridx         <= s_ridx;
      r_index        <= s_index;
      r_error        <= s_error;
      r_final        <= s_final;
      r_busy         <= s_busy;
      rr_busy        <= r_busy;
    end if;
  end process;

end rtl;
