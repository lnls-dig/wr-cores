--! @file eca_rmw.vhd
--! @brief ECA read-modify-write memory
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2015 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This core implements dual-ported read-modify-write memory. Port A has
--! priority and will always service requests within one cycle.
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

library work;
use work.eca_internals_pkg.all;

-- Async logic applied inputs. Async outputs. 
-- When x_en_i='1' the address on x_addr_i will be read with output available 
-- (based on x_ack_o) on x_data_o the next cycle. Whatever is fed into x_data_i
-- on that cycle will be written back to the same address.
-- The core guarantees read-new-data ordering between and within both ports.
entity eca_rmw is
  generic(
    g_addr_bits  : natural;
    g_data_bits  : natural);
  port(
    clk_i     : in  std_logic;
    rst_n_i   : in  std_logic;
    a_en_i    : in  std_logic;
    a_ack_o   : out std_logic; -- a has priority, so a_ack_o is a_en_i one cycle delayed
    a_addr_i  : in  std_logic_vector(g_addr_bits-1 downto 0);
    a_data_o  : out std_logic_vector(g_data_bits-1 downto 0);
    a_data_i  : in  std_logic_vector(g_data_bits-1 downto 0);
    b_en_i    : in  std_logic;
    b_ack_o   : out std_logic;
    b_addr_i  : in  std_logic_vector(g_addr_bits-1 downto 0);
    b_data_o  : out std_logic_vector(g_data_bits-1 downto 0);
    b_data_i  : in  std_logic_vector(g_data_bits-1 downto 0));
end eca_rmw;

-- Dual banking on the low bit means that if one of the ports alternates
-- odd/even addresses, the other port will always succeed at writing after
-- trying for two cycles. Furthermore, the ordering is well defined.
-- If port A receives two read-modify-writes A0 and A1 while port B receives
-- one read-modify-write B0 over those same two clock cycles, it is guaranteed
-- that regardless of the addresses involved, they will be executed in order:
--   A0-read, A0-write, B0-read, B0-write, B1-read, B1-write

architecture rtl of eca_rmw is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  signal s_q0_addr : std_logic_vector(g_addr_bits-1 downto 0);
  signal s_q1_addr : std_logic_vector(g_addr_bits-1 downto 0);
  signal r_q0_addr : std_logic_vector(g_addr_bits-1 downto 0);
  signal r_q1_addr : std_logic_vector(g_addr_bits-1 downto 0);
  signal s_a_q0    : std_logic;
  signal s_a_q1    : std_logic;
  signal s_b_q0    : std_logic;
  signal s_b_q1    : std_logic;
  signal s_q0_ren  : std_logic;
  signal s_q1_ren  : std_logic;
  signal r_q0_wen  : std_logic := '0';
  signal r_q1_wen  : std_logic := '0';
  signal s_mux     : std_logic;
  signal r_mux     : std_logic;
  signal q0_data   : std_logic_vector(g_data_bits-1 downto 0);
  signal q1_data   : std_logic_vector(g_data_bits-1 downto 0);
  signal s_q0_data : std_logic_vector(g_data_bits-1 downto 0);
  signal s_q1_data : std_logic_vector(g_data_bits-1 downto 0);
  
begin

  Q0 : eca_sdp
    generic map(
      g_addr_bits  => g_addr_bits-1,
      g_data_bits  => g_data_bits,
      g_bypass     => true)
    port map(
      r_clk_i  => clk_i,
      r_addr_i => s_q0_addr(s_q0_addr'high downto 1),
      r_data_o => q0_data,
      w_clk_i  => clk_i,
      w_en_i   => r_q0_wen,
      w_addr_i => r_q0_addr(r_q0_addr'high downto 1),
      w_data_i => s_q0_data);
  
  Q1 : eca_sdp
    generic map(
      g_addr_bits  => g_addr_bits-1,
      g_data_bits  => g_data_bits,
      g_dual_clock => false,
      g_bypass     => true)
    port map(
      r_clk_i  => clk_i,
      r_addr_i => s_q1_addr(s_q1_addr'high downto 1),
      r_data_o => q1_data,
      w_clk_i  => clk_i,
      w_en_i   => r_q1_wen,
      w_addr_i => r_q1_addr(r_q1_addr'high downto 1),
      w_data_i => s_q1_data);
  
  -- Which banks do the ports want?
  s_a_q0 <= a_en_i and not a_addr_i(0);
  s_a_q1 <= a_en_i and     a_addr_i(0);
  s_b_q0 <= b_en_i and not b_addr_i(0);
  s_b_q1 <= b_en_i and     b_addr_i(0);
  
  -- Port A always wins, port B only if A does not use it
  -- Is the bank used this cycle?
  s_q0_ren <= s_a_q0 or s_b_q0;
  s_q1_ren <= s_a_q1 or s_b_q1;
  
  -- What addresses do the banks receive? (port A has priority)
  s_q0_addr <= f_eca_mux(s_a_q0, a_addr_i, b_addr_i);
  s_q1_addr <= f_eca_mux(s_a_q1, a_addr_i, b_addr_i);
  
  -- Need all 4 of these muxes to depend on exactly ONE registered bit
  -- That way the synthesis tool's optimization can simplify feedback cases
  -- s_mux='0' means A:Q0 & B:Q1
  -- s_mux='1' means A:Q1 & B:Q0
  s_mux <= f_eca_mux(a_en_i, a_addr_i(0), not b_addr_i(0));
  
  -- Pick the bank used for output
  a_data_o <= f_eca_mux(r_mux, q1_data, q0_data);
  b_data_o <= f_eca_mux(r_mux, q0_data, q1_data);
  
  -- What data do the banks receive?
  s_q0_data <= f_eca_mux(r_mux, b_data_i, a_data_i);
  s_q1_data <= f_eca_mux(r_mux, a_data_i, b_data_i);
  
  control : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      a_ack_o  <= '0';
      b_ack_o  <= '0';
      r_q0_wen <= '0';
      r_q1_wen <= '0';
    elsif rising_edge(clk_i) then
      a_ack_o  <= a_en_i;
      b_ack_o  <= b_en_i and (not a_en_i or (a_addr_i(0) xor b_addr_i(0)));
      r_q0_wen <= s_q0_ren;
      r_q1_wen <= s_q1_ren;
    end if;
  end process;
  
  main : process(clk_i) is
  begin
    if rising_edge(clk_i) then
      r_q0_addr <= s_q0_addr;
      r_q1_addr <= s_q1_addr;
      r_mux     <= s_mux;
    end if;
  end process;
  
end rtl;
