--! @file eca_tlu.vhd
--! @brief ECA TLU
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component recovers clock
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
use work.eca_pkg.t_gpio_array;
use work.eca_internals_pkg.all;
use work.eca_tlu_auto_pkg.all;

entity eca_tlu is
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
end eca_tlu;

architecture rtl of eca_tlu is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  constant c_serdes     : natural := 8;
  constant c_input_bits : natural := f_eca_log2(g_inputs);

  signal sc_select  : std_logic_vector(7 downto 0);
  signal sc_enable  : std_logic;
  signal sc_event   : t_event;
  signal sc_stable  : std_logic_vector(31 downto 0);
  signal sc_write   : std_logic;
  signal rc_xor_o   : std_logic                             := '0';
  signal rc_xor_i   : std_logic_vector(4 downto 0)          := (others => '0');
  signal rc_stall   : std_logic                             := '0';

  signal ra_xor     : std_logic_vector(4 downto 0)          := (others => '0');
  signal ra_enables : std_logic_vector(g_inputs-1 downto 0) := (others => '0');
  
  signal raw_select : std_logic_vector(7 downto 0);
  signal raw_enable : std_logic;
  signal raw_event  : t_event;
  signal raw_stable : std_logic_vector(31 downto 0);
  
  type t_stable_array is array(g_inputs-1 downto 0) of std_logic_vector(g_stable-1 downto 0);
  signal ra_stables : t_stable_array;
  
  type t_time_array is array(g_inputs-1 downto 0) of t_time;
  signal sa_times  : t_time_array;
  signal sa_edges  : std_logic_vector(g_inputs-1 downto 0);
  signal sa_stbs   : std_logic_vector(g_inputs-1 downto 0);
  signal sa_acks   : std_logic_vector(g_inputs-1 downto 0);
  signal sa_event  : t_event;
  
  type t_state is (S_IDLE, S_OUTPUT, S_WAIT);
  signal ra_state : t_state   := S_IDLE;
  signal ra_stb   : std_logic := '0';

  signal sa_stb    : std_logic;
  signal sa_mux    : std_logic_vector(c_input_bits-1 downto 0);
  signal ra_mux    : std_logic_vector(c_input_bits-1 downto 0);
  signal ra_event  : t_event;
  signal ra_time   : t_time;
  
begin

  auto : eca_tlu_auto
    generic map(
      g_num_inputs => g_inputs)
    port map(
      clk_sys_i      => c_clk_i,
      rst_sys_n_i    => c_rst_n_i,
      slave_i        => c_slave_i,
      slave_o        => c_slave_o,
      error_i        => "0",
      stall_i(0)     => rc_stall,
      input_select_o => sc_select,
      enable_o(0)    => sc_enable,
      event_hi_o     => sc_event(63 downto 32),
      event_lo_o     => sc_event(31 downto  0),
      stable_o       => sc_stable,
      write_o(0)     => sc_write);

  main_c : process(c_clk_i, c_rst_n_i) is
  begin
    if c_rst_n_i = '0' then
      rc_xor_o <= '0';
      rc_xor_i <= (others => '0');
    elsif rising_edge(c_clk_i) then
      rc_xor_o <= rc_xor_o xor (sc_write and not rc_stall);
      rc_xor_i <= ra_xor(0) & rc_xor_i(rc_xor_i'high downto 1);
      
      if sc_write = '1' then
        rc_stall <= '1';
      end if;
      if rc_xor_i(0) /= rc_xor_i(1) then
        rc_stall <= '0';
      end if;
    end if;
  end process;
  
  main_a : process(a_clk_i, a_rst_n_i) is
  begin
    if a_rst_n_i = '0' then
      ra_xor     <= (others => '0');
      ra_enables <= (others => '0');
    elsif rising_edge(a_clk_i) then
      ra_xor <= rc_xor_o & ra_xor(ra_xor'high downto 1);
      ra_enables(to_integer(unsigned(raw_select))) <= raw_enable;
    end if;
  end process;
  
  bulk_a : process(a_clk_i) is
  begin
    if rising_edge(a_clk_i) then
      if ra_xor(1) /= ra_xor(0) then
        raw_select <= sc_select;
        raw_enable <= sc_enable;
        raw_event  <= sc_event;
        raw_stable <= sc_stable;
      end if;
      ra_stables(to_integer(unsigned(raw_select))) <= raw_stable(g_stable-1 downto 0);
    end if;
  end process;
  
  fsms : for i in 0 to g_inputs-1 generate
    fsm : eca_tlu_fsm
      generic map(
        g_history => g_history,
        g_stable  => g_stable,
        g_serdes  => c_serdes)
      port map(
        clk_i    => a_clk_i,
        rst_n_i  => a_rst_n_i,
        enable_i => ra_enables(i),
        stable_i => ra_stables(i),
        time_i   => a_time_i,
        gpio_i   => a_gpio_i(i),
        ack_i    => sa_acks(i),
        stb_o    => sa_stbs(i),
        edge_o   => sa_edges(i),
        time_o   => sa_times(i));
  
    -- Send ack to the correct core once edge_o and time_o latched
    sa_acks(i) <= f_eca_eq(ra_mux, std_logic_vector(to_unsigned(i, c_input_bits)))
                  when ra_state = S_OUTPUT else '0';
  end generate;
  
  events : eca_sdp
    generic map(
      g_addr_bits  => c_input_bits,
      g_data_bits  => 64,
      g_bypass     => false, -- new or old ... don't care
      g_dual_clock => false)
    port map(
      r_clk_i  => a_clk_i,
      r_addr_i => sa_mux,
      r_data_o => sa_event,
      w_clk_i  => a_clk_i,
      w_en_i   => '1',
      w_addr_i => raw_select(c_input_bits-1 downto 0),
      w_data_i => raw_event);
  
  -- Select one of the strobes for service
  sa_mux <= f_eca_1hot_decode(sa_stbs and f_eca_add(not sa_stbs, '1'));
  sa_stb <= f_eca_or(sa_stbs);
  
  push_fsm : process(a_clk_i, a_rst_n_i) is
  begin
    if a_rst_n_i = '0' then
      ra_state <= S_IDLE;
      ra_stb   <= '0';
    elsif rising_edge(a_clk_i) then
      case ra_state is
        when S_IDLE =>
          if sa_stb = '1' then
            ra_state <= S_OUTPUT;
          end if;
        when S_OUTPUT =>
          ra_stb   <= '1';
          ra_state <= S_WAIT;
        when S_WAIT  =>
          if a_stall_i = '0' then
            ra_stb <= '0';
            if sa_stb = '1' then
              ra_state <= S_OUTPUT;
            else
              ra_state <= S_IDLE;
            end if;
          end if;
      end case;
    end if;
  end process;
  
  output : process(a_clk_i) is
  begin
    if rising_edge(a_clk_i) then
      ra_mux <= sa_mux;
      if ra_state = S_OUTPUT then
        ra_event    <= sa_event;
        ra_event(0) <= sa_edges(to_integer(unsigned(ra_mux)));
        ra_time     <= sa_times(to_integer(unsigned(ra_mux)));
      end if;
    end if;
  end process;
  
  a_stream_o.stb   <= ra_stb;
  a_stream_o.event <= ra_event;
  a_stream_o.param <= (others => '0');
  a_stream_o.tef   <= (others => '0');
  a_stream_o.time  <= ra_time;
  
end rtl;
