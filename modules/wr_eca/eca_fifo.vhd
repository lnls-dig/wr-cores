--! @file eca_fifo.vhd
--! @brief ECA FIFO
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! A simple single-clock FIFO
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

entity eca_fifo is
  generic(
    g_log_size : natural;
    g_rows     : natural;
    g_cols     : natural);
  port(
    clk_i   : in  std_logic;
    rst_n_i : in  std_logic;
    push_i  : in  std_logic;
    full_o  : out std_logic;
    data_i  : in  t_eca_matrix(g_rows-1 downto 0, g_cols-1 downto 0);
    pop_i   : in  std_logic;
    valid_o : out std_logic;
    fresh_o : out std_logic; -- If the record was not delayed in the FIFO
    data_o  : out t_eca_matrix(g_rows-1 downto 0, g_cols-1 downto 0));
end eca_fifo;

architecture rtl of eca_fifo is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  constant c_low  : unsigned(g_log_size-1 downto 0) := (others => '0');
  constant c_high : unsigned(g_log_size   downto 0) := '1' & c_low;
    
  signal s_data_i : std_logic_vector(g_rows*g_cols-1 downto 0);
  signal s_data_o : std_logic_vector(g_rows*g_cols-1 downto 0);
  
  signal s_ridx  : unsigned(g_log_size downto 0);
  signal s_widx  : unsigned(g_log_size downto 0);
  
  signal r_ridx  : unsigned(g_log_size downto 0) := (others => '0');
  signal r_widx  : unsigned(g_log_size downto 0) := (others => '0');
  signal rr_widx : unsigned(g_log_size downto 0) := (others => '0');
  
  signal s_raddr : std_logic_vector(g_log_size-1 downto 0);
  signal s_waddr : std_logic_vector(g_log_size-1 downto 0);
  signal s_pop   : std_logic;
  signal s_push  : std_logic;
  
  signal r_valid : std_logic := '0';
  signal r_fresh : std_logic := '1';
  signal r_full  : std_logic := '0';
  
  function f_idx(r, c : natural) return natural is
  begin
    return r * g_cols + c;
  end f_idx;
  
begin

  rows : for r in 0 to g_rows-1 generate
    cols : for c in 0 to g_cols-1 generate
      s_data_i(f_idx(r, c)) <= data_i(r, c);
      data_o(r, c) <= s_data_o(f_idx(r, c));
    end generate;
  end generate;
  
  s_pop  <= pop_i  and r_valid;
  s_push <= push_i and not r_full;
  
  s_ridx  <= r_ridx + (""&s_pop);
  s_widx  <= r_widx + (""&s_push);
  s_raddr <= std_logic_vector(s_ridx(g_log_size-1 downto 0));
  s_waddr <= std_logic_vector(r_widx(g_log_size-1 downto 0));
  
  ram : eca_sdp
    generic map(
      g_addr_bits  => g_log_size,
      g_data_bits  => g_rows*g_cols,
      g_bypass     => false,
      g_dual_clock => false)
    port map(
      r_clk_i  => clk_i,
      r_addr_i => s_raddr,
      r_data_o => s_data_o,
      w_clk_i  => clk_i,
      w_en_i   => s_push,
      w_addr_i => s_waddr,
      w_data_i => s_data_i);

  main : process(clk_i, rst_n_i) is
  begin
    if rst_n_i = '0' then
      r_ridx  <= (others => '0');
      r_widx  <= (others => '0');
      rr_widx <= (others => '0');
      r_valid <= '0';
      r_fresh <= '1';
      r_full  <= '0';
    elsif rising_edge(clk_i) then
      r_ridx  <= s_ridx;
      r_widx  <= s_widx;
      rr_widx <= r_widx;
      r_valid <= f_eca_active_high(s_ridx /= r_widx);
      r_fresh <= f_eca_active_high(s_ridx = rr_widx);
      r_full  <= f_eca_active_high((s_widx xor c_high) = s_ridx);
    end if;
  end process;
  
  valid_o <= r_valid;
  fresh_o <= r_fresh;
  full_o  <= r_full;

end rtl;
