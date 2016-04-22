--! @file eca_tlu_fsm.vhd
--! @brief ECA TLU FSM
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component detects when a signal toggles
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

entity eca_tlu_fsm is
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
end eca_tlu_fsm;

architecture rtl of eca_tlu_fsm is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  constant c_increment_bits : natural := f_eca_log2(g_serdes+1);
  constant c_history_bits   : natural := f_eca_log2(g_history);
  constant c_delay_adjust   : natural := g_serdes * 4;
  constant c_opposed_init   : std_logic_vector(c_history_bits-1 downto 0) := std_logic_vector(to_unsigned(c_delay_adjust, c_history_bits));
  
  signal r_prior     : std_logic := '0';
  signal r_gpio      : std_logic_vector(g_serdes        -1 downto 0) := (others => '0');
  signal r_flat      : std_logic_vector(g_stable        -1 downto 0) := (others => '0');
  signal r_increment : std_logic_vector(c_increment_bits-1 downto 0) := (others => '0');
  signal r_opposed   : std_logic_vector(c_history_bits  -1 downto 0) := c_opposed_init;
  signal r_state     : std_logic := '0';
  signal r_stb       : std_logic := '0';
  signal r_edge      : std_logic := '-';
  signal r_time      : t_time    := (others => '-');
  
  signal s_state     : std_logic_vector(g_serdes        -1 downto 0);
  signal s_prior     : std_logic_vector(g_serdes        -1 downto 0);
  signal s_flat_eq   : std_logic;
  
  -- Hopefully logic minimization does a good job on this
  function f_count(x : std_logic_vector)
    return std_logic_vector is
    variable result : std_logic_vector(c_increment_bits-1 downto 0) := (others => '0');
  begin
    for i in x'range loop
      result := f_eca_add(result, x(i));
    end loop;
    return result;
  end f_count;

begin

  s_state <= (others => r_state);
  s_prior <= (others => r_prior);
  s_flat_eq <= f_eca_eq(r_flat, stable_i);
  
  main : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_prior     <= '0';
      r_gpio      <= (others => '0');
      r_flat      <= (others => '0');
      r_increment <= (others => '0');
      r_opposed   <= c_opposed_init;
      r_state     <= '0';
      r_stb       <= '0';
      r_edge      <= '-';
      r_time      <= (others => '-');
    elsif rising_edge(clk_i) then
    
      r_prior     <= r_gpio(r_gpio'high);
      r_gpio      <= gpio_i;
      
      -- Spread the counting over a few cycles for timing closure.
      -- Since we only clear these when the input signal has been stable, the
      -- information lost is relating to the constant past => safe to ignore
      r_increment <= f_count(r_gpio xor s_state);
      r_opposed   <= f_eca_add(r_opposed, r_increment);
      
      if ack_i = '1' then
        r_stb <= '0';
      end if;
      
      if s_prior /= r_gpio then
        r_flat <= (others => '0');
      else
        r_flat <= f_eca_add(r_flat, not s_flat_eq);
        if s_flat_eq = '1' then
          r_state     <= r_prior;
          r_increment <= (others => '0');
          r_opposed   <= c_opposed_init;
          if r_state /= r_prior then
            r_stb  <= enable_i;
            r_edge <= r_prior;
            r_time <= f_eca_sub(time_i, r_opposed);
          end if;
        end if;
      end if;
    end if;
  end process;
  
  stb_o  <= r_stb;
  edge_o <= r_edge;
  time_o <= r_time;

end rtl;
