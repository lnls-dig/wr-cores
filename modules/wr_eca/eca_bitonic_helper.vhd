--! @file eca_bitonic_helper.vhd
--! @brief ECA Bitonic Sorting Network (helper entity)
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2016 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! DO NOT USE THIS ENTITY! Use the eca_bitonic entity!
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

entity eca_bitonic_helper is
  generic(
    g_log_size : natural;
    g_wide     : natural;
    g_order    : boolean); -- true = smallest first
  port(
    clk_i   : in  std_logic;
    rst_n_i : in  std_logic;
    en_i    : in  std_logic;
    nums_i  : in  t_eca_matrix(2**g_log_size-1 downto 0, g_wide-1 downto 0);
    nums_o  : out t_eca_matrix(2**g_log_size-1 downto 0, g_wide-1 downto 0));
end eca_bitonic_helper;

architecture rtl of eca_bitonic_helper is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";
  
  constant c_log_size1 : natural := g_log_size - 1;
  constant c_full      : natural := 2**g_log_size;
  constant c_half      : natural := 2**c_log_size1;

  type t_slices is array(natural range <>) of std_logic_vector(g_wide-1 downto 0);
  
  signal s_full         : t_slices(c_full-1 downto 0);
  signal s_sub0, s_sub1 : t_slices(c_half-1 downto 0);

begin

  no0 : assert g_log_size > 0 report "Incorrect g_log_size" severity failure;
  
  -- Relabel nums_i to s_full
  relabel : for i in 0 to c_full-1 generate
    bits : for b in 0 to g_wide-1 generate
      s_full(i)(b) <= nums_i(i, b);
    end generate;
  end generate;
  
  -- Flip it half-way
  swaps : for i in 0 to c_half-1 generate
    swap : eca_bitonic_swap
      generic map(
        g_wide  => g_wide,
        g_order => g_order)
      port map(
        clk_i   => clk_i,
        rst_n_i => rst_n_i,
        en_i    => en_i,
        a_i     => s_full(i),
        b_i     => s_full(i+c_half),
        a_o     => s_sub0(i),
        b_o     => s_sub1(i));
  end generate;
  
  -- Handle base-case
  base1 : if g_log_size = 1 generate
    bits : for b in 0 to g_wide-1 generate
      nums_o(0,b) <= s_sub0(0)(b);
      nums_o(1,b) <= s_sub1(0)(b);
    end generate;
  end generate;
  
  basen1 : if g_log_size > 1 generate
    blockn1 : block is
      signal m_sub_i0, m_sub_i1 : t_eca_matrix(c_half-1 downto 0, g_wide-1 downto 0);
      signal m_sub_o0, m_sub_o1 : t_eca_matrix(c_half-1 downto 0, g_wide-1 downto 0);
    begin
    
      sub0 : work.eca_internals_pkg.eca_bitonic_helper
        generic map(
          g_log_size => c_log_size1,
          g_wide     => g_wide,
          g_order    => g_order)
        port map(
          clk_i   => clk_i,
          rst_n_i => rst_n_i,
          en_i    => en_i,
          nums_i  => m_sub_i0,
          nums_o  => m_sub_o0);
          
      sub1 : work.eca_internals_pkg.eca_bitonic_helper
        generic map(
          g_log_size => c_log_size1,
          g_wide     => g_wide,
          g_order    => g_order)
        port map(
          clk_i   => clk_i,
          rst_n_i => rst_n_i,
          en_i    => en_i,
          nums_i  => m_sub_i1,
          nums_o  => m_sub_o1);
      
      -- Relabel
      relabel : for i in 0 to c_half-1 generate
        bits : for b in 0 to g_wide-1 generate
          m_sub_i0(i, b) <= s_sub0(i)(b);
          m_sub_i1(i, b) <= s_sub1(i)(b);
          nums_o(i,        b) <= m_sub_o0(i, b);
          nums_o(i+c_half, b) <= m_sub_o1(i, b);
        end generate;
      end generate;
      
    end block;
  end generate;
  
end rtl;
