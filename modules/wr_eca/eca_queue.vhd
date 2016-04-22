--! @file eca_queue.vhd
--! @brief ECA-Queue Adapter
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component takes an action channel and provides a FIFO interface.
--! Naturally, this loses a ton of the determinism of the ECA.
--! However, the intent is to connect this to a soft real-time CPU.
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
use work.eca_queue_auto_pkg.all;
use work.eca_pkg.all;

entity eca_queue is
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
end eca_queue;

architecture rtl of eca_queue is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  signal ra_pop_xor    : std_logic_vector(5 downto 0) := (others => '0');
  signal sa_pop        : std_logic;
  signal sa_refill     : std_logic;
  signal sa_valid      : std_logic;
  signal ra_valid      : std_logic := '0';
  signal ra_refill_xor : std_logic := '0';

  signal sq_pop_raw    : std_logic;
  signal sq_pop        : std_logic;
  signal rq_pop_xor    : std_logic := '0';
  signal rq_valid      : std_logic := '0';
  signal rq_refill_xor : std_logic_vector(3 downto 0) := (others => '0');
  signal sq_refill     : std_logic;
  signal rq_ready_xor  : std_logic_vector(4 downto 0) := (others => '0');
  signal sq_ready      : std_logic;
  signal rq_ready      : std_logic := '1';
  
  signal rq_channel    : t_channel;
  
begin

  sa_pop    <= ra_pop_xor(3) xor ra_pop_xor(2);
  sa_valid  <= a_channel_i.valid and not sa_pop;
  sa_refill <= a_channel_i.valid and not ra_valid;
  
  a_stall_o <= sa_valid;

  action : process(a_clk_i, a_rst_n_i) is
  begin
    if a_rst_n_i = '0' then
      ra_pop_xor    <= (others => '0');
      ra_valid      <= '0';
      ra_refill_xor <= '0';
    elsif rising_edge(a_clk_i) then
      ra_pop_xor    <= rq_pop_xor & ra_pop_xor(ra_pop_xor'high downto 1);
      ra_valid      <= sa_valid;
      ra_refill_xor <= ra_refill_xor xor sa_refill;
    end if;
  end process;
  
  sq_refill <= rq_refill_xor(1) xor rq_refill_xor(0);
  sq_ready  <= rq_ready_xor(1)  xor rq_ready_xor(0);
  sq_pop <= sq_pop_raw and rq_valid and rq_ready;
  
  queue : process(q_clk_i, q_rst_n_i) is
  begin
    if q_rst_n_i = '0' then
      rq_pop_xor    <= '0';
      rq_valid      <= '0';
      rq_refill_xor <= (others => '0');
      rq_ready_xor  <= (others => '0');
      rq_ready      <= '1';
    elsif rising_edge(q_clk_i) then
      rq_pop_xor    <= rq_pop_xor xor sq_pop;
      rq_refill_xor <= ra_refill_xor & rq_refill_xor(rq_refill_xor'high downto 1);
      rq_ready_xor  <= ra_pop_xor(0) & rq_ready_xor (rq_ready_xor'high  downto 1);
      
      if sq_pop = '1' then
        rq_valid <= '0';
        rq_ready <= '0';
      end if;
      if sq_refill = '1' then -- order does not matter
        rq_valid <= '1';
      end if;
      if sq_ready = '1' then
        rq_ready <= '1';
      end if;
    end if;
  end process;
  
  bulk : process(q_clk_i) is
  begin
    if rising_edge(q_clk_i) then
      if sq_refill = '1' then
        rq_channel <= a_channel_i;
      end if;
    end if;
  end process;

  INST_eca_queue_auto : eca_queue_auto
    generic map(
      g_queue_id        => g_queue_id)
    port map(
      clk_sys_i         => q_clk_i,
      rst_sys_n_i       => q_rst_n_i,
      stall_i(0)        => "not"(rq_ready),
      error_i           => "0",
      pop_o(0)          => sq_pop_raw,
      flags_V_i(0)      => rq_ready,
      flags_i(0)        => rq_channel.late,
      flags_i(1)        => rq_channel.early,
      flags_i(2)        => rq_channel.conflict,
      flags_i(3)        => rq_channel.delayed,
      flags_i(4)        => rq_valid,
      num_V_i(0)        => rq_ready,
      num_i             => rq_channel.num,
      event_id_hi_V_i(0)=> rq_ready,
      event_id_hi_i     => rq_channel.event(63 downto 32),
      event_id_lo_V_i(0)=> rq_ready,
      event_id_lo_i     => rq_channel.event(31 downto  0),
      param_hi_V_i(0)   => rq_ready,
      param_hi_i        => rq_channel.param(63 downto 32),
      param_lo_V_i(0)   => rq_ready,
      param_lo_i        => rq_channel.param(31 downto  0),
      tag_V_i(0)        => rq_ready,
      tag_i             => rq_channel.tag,
      tef_V_i(0)        => rq_ready,
      tef_i             => rq_channel.tef,
      deadline_hi_V_i(0)=> rq_ready,
      deadline_hi_i     => rq_channel.deadline(63 downto 32),
      deadline_lo_V_i(0)=> rq_ready,
      deadline_lo_i     => rq_channel.deadline(31 downto  0),
      executed_hi_V_i(0)=> rq_ready,
      executed_hi_i     => rq_channel.executed(63 downto 32),
      executed_lo_V_i(0)=> rq_ready,
      executed_lo_i     => rq_channel.executed(31 downto  0),
      slave_i           => q_slave_i,
      slave_o           => q_slave_o);

end rtl;
