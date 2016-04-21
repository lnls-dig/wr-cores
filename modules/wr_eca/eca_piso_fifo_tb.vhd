--! @file eca_piso_fifo_tb.vhd
--! @brief ECA PISO FIFO test code
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2015 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This core tests the behaviour of the eca_piso_fifo unit
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

entity eca_piso_fifo_tb is
  generic(
    g_case  : natural);
  port(
    clk_i   : in std_logic;
    rst_n_i : in std_logic);
end eca_piso_fifo_tb;

architecture rtl of eca_piso_fifo_tb is

  constant c_log_ports : natural := g_case;
  constant c_log_size  : natural := c_log_ports + 3;
  constant c_ports     : natural := 2**c_log_ports;
  constant c_width     : natural := 32;

  signal r_push   : std_logic_vector(c_ports-1 downto 0) := (others => '0');
  signal r_pop    : std_logic := '0';
  signal r_data_i : t_eca_matrix(c_ports-1 downto 0, c_width-1 downto 0);
  signal s_valid  : std_logic;
  signal s_fresh  : std_logic;
  signal s_data_o : std_logic_vector(c_width-1 downto 0);
  
begin

  fifo : eca_piso_fifo
    generic map(
      g_log_size  => c_log_size,
      g_log_ports => c_log_ports,
      g_width     => c_width)
    port map(
      clk_i   => clk_i,
      rst_n_i => rst_n_i,
      push_i  => r_push,
      data_i  => r_data_i,
      pop_i   => r_pop,
      valid_o => s_valid,
      fresh_o => s_fresh,
      data_o  => s_data_o);
  
  main : process(clk_i, rst_n_i) is
    variable s1, s2 : positive := 42;
    variable push  : std_logic_vector(c_ports-1 downto 0);
    variable pop   : std_logic;
    variable count : natural := 0;
    variable check : natural := 0;
    variable data  : unsigned(c_width-1 downto 0);
    variable idle  : boolean := false;
  begin
    if rst_n_i = '0' then
      r_push <= (others => '0');
      r_pop  <= '0';
    elsif rising_edge(clk_i) then
      
      p_eca_uniform(s1, s2, push);
      p_eca_uniform(s1, s2, pop);
      
      if r_pop = '1' and s_valid = '1' then
        assert check = unsigned(s_data_o) report "Count mismatch" severity failure;
        check := check + 1;
      end if;
      
      for p in 0 to c_ports-1 loop
        if push(p) = '1' then
          if idle or count = check+2**c_log_size then
            push(p) := '0';
            idle := true;
          else
            data  := to_unsigned(count, data'length);
            count := count + 1;
          end if;
        else
          data := (others => 'X');
        end if;
        for b in 0 to c_width-1 loop
          r_data_i(p,b) <= data(b);
        end loop;
      end loop;
      
      if check = count then
        idle := false;
      end if;
      
      r_push <= push;
      r_pop  <= pop;
      
    end if;
  end process;
  
end rtl;
