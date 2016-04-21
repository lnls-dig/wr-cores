--! @file eca_rmw_tb.vhd
--! @brief ECA read-modify-write memory test code
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2015 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This core tests the behaviour of the eca_rmw unit
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

entity eca_rmw_tb is
  generic(
    g_case  : natural);
  port(
    clk_i   : in std_logic;
    rst_n_i : in std_logic);
end eca_rmw_tb;

architecture rtl of eca_rmw_tb is

  constant c_addr_bits : natural := g_case + 2; -- small to ensure lots of collisions
  constant c_data_bits : natural := 8;
  constant c_depth     : natural := 2**c_addr_bits;
  
  signal a_en_i   : std_logic := '0';
  signal a_ack_o  : std_logic;
  signal a_addr_i : std_logic_vector(c_addr_bits-1 downto 0);
  signal a_data_o : std_logic_vector(c_data_bits-1 downto 0);
  signal a_data_i : std_logic_vector(c_data_bits-1 downto 0);
  signal b_en_i   : std_logic := '0';
  signal b_ack_o  : std_logic;
  signal b_addr_i : std_logic_vector(c_addr_bits-1 downto 0);
  signal b_data_o : std_logic_vector(c_data_bits-1 downto 0);
  signal b_data_i : std_logic_vector(c_data_bits-1 downto 0);
  
  signal r_a_en   : std_logic;
  signal r_b_en   : std_logic;
  signal r_a_addr : std_logic_vector(c_addr_bits-1 downto 0);
  signal r_b_addr : std_logic_vector(c_addr_bits-1 downto 0);
  
begin

  rmw : eca_rmw
    generic map(
      g_addr_bits => c_addr_bits,
      g_data_bits => c_data_bits)
    port map(
      clk_i    => clk_i,
      rst_n_i  => rst_n_i,
      a_en_i   => a_en_i,
      a_ack_o  => a_ack_o,
      a_addr_i => a_addr_i,
      a_data_o => a_data_o,
      a_data_i => a_data_i,
      b_en_i   => b_en_i,
      b_ack_o  => b_ack_o,
      b_addr_i => b_addr_i,
      b_data_o => b_data_o,
      b_data_i => b_data_i);
      
  test : process(clk_i, rst_n_i) is
    variable s1, s2 : positive := 42;
    
    type t_memory is array(c_depth-1 downto 0) of std_logic_vector(c_data_bits-1 downto 0);
    variable v_memory : t_memory := (others => (others => '-'));
    
    variable a_en   : std_logic;
    variable a_addr : std_logic_vector(c_addr_bits-1 downto 0);
    variable a_data : std_logic_vector(c_data_bits-1 downto 0);
    variable b_en   : std_logic;
    variable b_addr : std_logic_vector(c_addr_bits-1 downto 0);
    variable b_data : std_logic_vector(c_data_bits-1 downto 0);
    
  begin
    if rst_n_i = '0' then
      a_en_i <= '0';
      b_en_i <= '0';
      r_a_en <= '0';
      r_b_en <= '0';
    elsif rising_edge(clk_i) then
      -- Check output
      assert a_ack_o = r_a_en report "Port A did not get priority" severity failure;
      assert (b_ack_o and not r_b_en) = '0' report "Port B got priority without asking" severity failure;
      
      assert r_a_addr /= r_b_addr or a_ack_o = '0' or b_ack_o = '0' report "Unblocked A-B data race" severity failure;
      
      -- Check read-outs
      if a_ack_o = '1' then
        assert a_data_o = v_memory(to_integer(unsigned(r_a_addr))) report "unexpected A readout" severity failure;
      end if;
      if b_ack_o = '1' then
        assert b_data_o = v_memory(to_integer(unsigned(r_b_addr))) report "unexpected B readout" severity failure;
      end if;
      
      -- Update write-back
      if a_ack_o = '1' then
        v_memory(to_integer(unsigned(r_a_addr))) := a_data_i;
      end if;
      if b_ack_o = '1' then
        v_memory(to_integer(unsigned(r_b_addr))) := b_data_i;
      end if;
      
      -- Pick random values
      p_eca_uniform(s1, s2, a_en);
      p_eca_uniform(s1, s2, b_en);
      p_eca_uniform(s1, s2, a_addr);
      p_eca_uniform(s1, s2, b_addr);
      p_eca_uniform(s1, s2, a_data);
      p_eca_uniform(s1, s2, b_data);
      
      -- Control the tested unit
      a_en_i   <= a_en;
      b_en_i   <= b_en;
      a_addr_i <= a_addr;
      b_addr_i <= b_addr;
      a_data_i <= a_data;
      b_data_i <= b_data;
      
      r_a_en   <= a_en_i;
      r_b_en   <= b_en_i;
      r_a_addr <= a_addr_i;
      r_b_addr <= b_addr_i;
    end if;
  end process;

end rtl;
