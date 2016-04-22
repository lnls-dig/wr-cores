--! @file eca_compact.vhd
--! @brief ECA Compacting Network
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2016 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component moves all valid inputs to the left
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

entity eca_compact is
  generic(
    g_size : natural;
    g_wide : natural;
    g_step : natural := 1);
  port(
    clk_i   : in  std_logic;
    rst_n_i : in  std_logic;
    holes_i : in  std_logic_vector(g_size-1 downto 0) := (others => '-');
    valid_i : in  std_logic_vector(g_size-1 downto 0);
    valid_o : out std_logic_vector(g_size-1 downto 0);
    data_i  : in  t_eca_matrix(g_size-1 downto 0, g_wide-1 downto 0);
    data_o  : out t_eca_matrix(g_size-1 downto 0, g_wide-1 downto 0));
end eca_compact;

architecture rtl of eca_compact is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  -- result(i) := xor_{j in 0 to i} x(j)
  -- latency O(log n), area = O(n)
  function prefix_xor(x : std_logic_vector) return std_logic_vector is
    constant len  : natural := x'length;
    variable gap  : natural;
    variable var  : std_logic_vector(x'length-1 downto 0);
  begin
    var := x;
    
    if len > 1 then
      for row in 0 to f_eca_log2(len+1)-2 loop
        gap := 2**row;
        for i in 0 to (len/gap-2)/2 loop
          var((2*i+2)*gap-1) := var((2*i+2)*gap-1) xor var((2*i+1)*gap-1);
        end loop;
      end loop;
    end if;
    
    if len > 2 then
      for row in f_eca_log2(len/3+1)-1 downto 0 loop
        gap := 2**row;
        for i in 0 to (len/gap-3)/2 loop
          var((2*i+3)*gap-1) := var((2*i+3)*gap-1) xor var((2*i+2)*gap-1);
        end loop;
      end loop;
    end if;
    
    return var;
  end prefix_xor;

begin

  nill : if g_step >= g_size generate
    valid_o <= valid_i;
    data_o  <= data_i;
  end generate;
  
  notnill : if g_step < g_size generate
    notnillb : block is
    
      signal r_valid : std_logic_vector(g_size-1 downto 0) := (others => '0');
      signal s_valid : std_logic_vector(g_size-1 downto 0);
      signal r_data  : t_eca_matrix(g_size-1 downto 0, g_wide-1 downto 0);
      signal s_data  : t_eca_matrix(g_size-1 downto 0, g_wide-1 downto 0);
      
      signal s_holes      : std_logic_vector(g_size-1 downto 0);
      signal s_xor_holes  : std_logic_vector(g_size-1 downto 0);
      signal r_xor_holes  : std_logic_vector(g_size-1 downto 0) := (others => '0');
  
    begin
    
      control : process(clk_i, rst_n_i) is
      begin
        if rst_n_i = '0' then
          r_valid     <= (others => '0');
          r_xor_holes <= (others => '0');
        elsif rising_edge(clk_i) then
          r_valid     <= valid_i;
          r_xor_holes <= s_xor_holes;
        end if;
      end process;
      
      s_holes <= not valid_i when g_step = 1 else holes_i;
      s_xor_holes <= prefix_xor(s_holes);
      
      main : process(clk_i) is
      begin
        if rising_edge(clk_i) then
          r_data <= data_i;
        end if;
      end process;
      
      muxes : for i in 0 to g_size-g_step-1 generate
        s_valid(i) <= f_eca_mux(r_xor_holes(i+g_step-1), r_valid(i+g_step), r_valid(i));
        bits : for b in 0 to g_wide-1 generate
          s_data(i,b) <= f_eca_mux(r_xor_holes(i+g_step-1), r_data(i+g_step,b), r_data(i,b));
        end generate;
      end generate;
      
      cuts : for i in g_size-g_step to g_size-1 generate
        s_valid(i)  <= r_valid(i) and not r_xor_holes(g_size-1);
        bits : for b in 0 to g_wide-1 generate
          s_data(i,b) <= r_data(i,b);
        end generate;
      end generate;
      
      done : if g_step+g_step >= g_size generate
        valid_o <= s_valid;
        data_o  <= s_data;
      end generate;
      
      step : if g_step+g_step < g_size generate
        stepb : block is
          signal r_even_holes : std_logic_vector(g_size-1 downto 0);
        begin
        
          latch : process(clk_i) is
          begin
            if rising_edge(clk_i) then
              r_even_holes <= not s_xor_holes and s_holes;
            end if;
          end process;
          
          self : work.eca_internals_pkg.eca_compact
            generic map(
              g_size => g_size,
              g_wide => g_wide,
              g_step => g_step+g_step)
            port map(
              clk_i   => clk_i,
              rst_n_i => rst_n_i,
              holes_i => r_even_holes,
              valid_i => s_valid,
              valid_o => valid_o,
              data_i  => s_data,
              data_o  => data_o);
        
        end block;
      end generate;
    end block;
  end generate;
  
end rtl;
