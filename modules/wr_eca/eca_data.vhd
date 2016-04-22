--! @file eca_data.vhd
--! @brief ECA channel data stored in table
--! @author Wesley W. Terpstra <w.terpstra@gsi.de>
--!
--! Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
--!
--! This component stores all fields of an action except valid and executed
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

entity eca_data is
  generic(
    g_log_size : natural);
  port(
    clk_i   : in  std_logic;
    rst_n_i : in  std_logic;
    w_en_i  : in  std_logic;
    w_idx_i : in  std_logic_vector(g_log_size-1 downto 0);
    w_dat_i : in  t_channel;
    r_idx_i : in  std_logic_vector(g_log_size-1 downto 0);
    r_dat_o : out t_channel);
end eca_data;

architecture rtl of eca_data is
  -- Quartus 11+ goes crazy and infers 7 M9Ks in an altshift_taps! Stop it.
  attribute altera_attribute : string; 
  attribute altera_attribute of rtl : architecture is "-name AUTO_SHIFT_REGISTER_RECOGNITION OFF";

  constant c_conflict : natural := 0;
  constant c_late     : natural := c_conflict+1;
  constant c_early    : natural := c_late    +1;
  constant c_delayed  : natural := c_early   +1;
  
  subtype c_num    is natural range t_num'length  +c_delayed    downto c_delayed   +1;
  subtype c_event  is natural range t_event'length+c_num'high   downto c_num'high  +1;
  subtype c_param  is natural range t_param'length+c_event'high downto c_event'high+1;
  subtype c_tag    is natural range t_tag'length  +c_param'high downto c_param'high+1;
  subtype c_tef    is natural range t_tef'length  +c_tag'high   downto c_tag'high  +1;
  subtype c_time   is natural range t_time'length +c_tef'high   downto c_tef'high  +1;
  
  signal s_dat_i : std_logic_vector(c_time'high downto 0);
  signal s_dat_o : std_logic_vector(c_time'high downto 0);

begin

  s_dat_i(c_conflict) <= w_dat_i.conflict;
  s_dat_i(c_late)     <= w_dat_i.late;
  s_dat_i(c_early)    <= w_dat_i.early;
  s_dat_i(c_delayed)  <= w_dat_i.delayed;
  s_dat_i(c_num)      <= w_dat_i.num;
  s_dat_i(c_event)    <= w_dat_i.event;
  s_dat_i(c_param)    <= w_dat_i.param;
  s_dat_i(c_tag)      <= w_dat_i.tag;
  s_dat_i(c_tef)      <= w_dat_i.tef;
  s_dat_i(c_time)     <= w_dat_i.deadline;

  r_dat_o.valid    <= '1';
  r_dat_o.conflict <= s_dat_o(c_conflict);
  r_dat_o.late     <= s_dat_o(c_late);
  r_dat_o.early    <= s_dat_o(c_early);
  r_dat_o.delayed  <= s_dat_o(c_delayed);
  r_dat_o.num      <= s_dat_o(c_num);
  r_dat_o.event    <= s_dat_o(c_event);
  r_dat_o.param    <= s_dat_o(c_param);
  r_dat_o.tag      <= s_dat_o(c_tag);
  r_dat_o.tef      <= s_dat_o(c_tef);
  r_dat_o.deadline <= s_dat_o(c_time);
  r_dat_o.executed <= (others => '0');
  
  table : eca_sdp
    generic map(
      g_addr_bits  => g_log_size,
      g_data_bits  => s_dat_i'length,
      g_bypass     => false,
      g_dual_clock => false)
    port map(
      r_clk_i  => clk_i,
      r_addr_i => r_idx_i,
      r_data_o => s_dat_o,
      w_clk_i  => clk_i,
      w_en_i   => w_en_i,
      w_addr_i => w_idx_i,
      w_data_i => s_dat_i);

end rtl;
